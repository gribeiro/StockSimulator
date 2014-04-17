package com.stocksimulator.abs
import org.joda.time._
import scala.collection.mutable.ListBuffer
import scala.{ Some, None }
import com.stocksimulator.debug._
import com.stocksimulator.debug.LogNames._
import scala.collection.mutable.LinkedHashMap
import org.joda.convert.ToString
import scala.collection.mutable.HashMap
import scala.collection.mutable.HashSet
import com.stocksimulator.helpers._
import com.stocksimulator.helpers.PMaker._
import com.stocksimulator.helpers.ImplicitClasses._
import com.stocksimulator.utils.BlackScholes

case class Position(quantity: Int, pnl: Double)

class Parameters {
  val mem = new LinkedHashMap[String, Object]
  def set(s: String, obj: Object) = {
    
    mem.put(s, obj)
  }

  def get(s: String) = {
    mem.get(s)
  }

  def keys = mem.keySet
  def unwrap = mem
  def size = mem.size
 override def equals(obj: Any) = {
    obj match {
      case p: Parameters => p.inputStr == inputStr
      case _ => false
    }
  }
  
  override def hashCode(): Int ={
    inputStr.hashCode()
  }
  def inputStr = {
    val memOne = for ((k, v) <- mem) yield k + "=" + v.toString()
    memOne.mkString("_")
  }
  override def toString(): String = mem.toString()
}


trait BuySellAdapter {
  def buy(st: Stock, qt: Int, amount: Double): TunnelTicketStatus
  def sell(st: Stock, qt: Int, amount: Double): TunnelTicketStatus
  def replaceBuy(ticketToReplace: Ticket, qt: Int, amount: Double): TunnelTicketStatus
  def replaceSell(ticketToReplace: Ticket, qt: Int, amount: Double): TunnelTicketStatus
  def getStatus(tunnelTicket: TunnelTicketStatus): TunnelTicketStatus
  def cancelOrder(order: Ticket)
  def appendWaitOrder(wait: Wait, qt: Int, amount: Double) 
}

abstract class BuySellAdopt(bsAdap: BuySellAdapter) extends BuySellAdapter {
  def buy(st: Stock, qt: Int, amount: Double) = bsAdap.buy(st, qt, amount)
  def sell(st: Stock, qt: Int, amount: Double) = bsAdap.sell(st, qt, amount)
  def cancelOrder(order: Ticket) = bsAdap.cancelOrder(order)
  def replaceBuy(ticketToReplace: Ticket, qt: Int, amount: Double) = bsAdap.replaceBuy(ticketToReplace, qt, amount)
  def replaceSell(ticketToReplace: Ticket, qt: Int, amount: Double) = bsAdap.replaceSell(ticketToReplace, qt, amount)
  def getStatus(tunnelTicket: TunnelTicketStatus): TunnelTicketStatus = bsAdap.getStatus(tunnelTicket)
  def appendWaitOrder(wait: Wait, qt: Int, amount: Double) = bsAdap.appendWaitOrder(wait, qt, amount)
}



object Strategy {
  def midPrice(q: Quote) = {
    //Log("Bid Price: " + q.bid.price.toString)
    //Log("Ask Price: " + q.ask.price.toString)
    (q.bid.price + q.ask.price) / 2
  }
  def midPrice(t: Trade) = t.priceVol.price
  def midPrice(s: StockInfo): Double = {
    s match {
      case t: Trade => midPrice(t)
      case q: Quote => midPrice(q)
    }
  }
}
 
abstract class Strategy(market: Market, private val param: Parameters) extends BuySellAdapter {
  implicit def ParamMaker2Int(pm: ParamMaker): Int = getIntParam(pm.name)
  implicit def ParamMaker2Double(pm: ParamMaker): Double = this.getParam(pm.name).asInstanceOf[Double]
  implicit def ParamMaker2String(pm: ParamMaker): String = this.getParam(pm.name).asInstanceOf[String]
  
  type report = (Ticket, OrderResult) => Unit
  type tInfo = Iterable[(Ticket, OrderResult)]
  type mInfo = Map[Stock, StockInfo]

  private val result = new Parameters
  private val tunnel = new StrategyTicketTunnel(market)

  def getParam(s: String) = {
    param.get(s) match {
      case Some(a) => a
      case None => None
    }
  }
  
  def getIntParam(s: String):Int = getParam(s).asInstanceOf[Double].intValue()
  def getDoubleParam(s: String):Double = getParam(s).asInstanceOf[Double]
  
  def bsVolCall(spot: Double, strike: Double, r: Double, optionPrice: Double, timeToExpiry: Double) = BlackScholes.bsVolCall(spot, strike, r, optionPrice, timeToExpiry)
  def priceEuropeanBlackScholesCall(spot: Double, strike: Double, r: Double, timeToExpiry: Double, volatility: Double) = BlackScholes.priceEuropeanBlackScholesCall(spot, strike, r, timeToExpiry, volatility)
  
  protected def putResult(key: String, obj: Object) {
    result.set(key, obj)
  }
  protected def print(s: String) = Log(s)
  def onStart()
  def onStop()
  def onQuotes()

  def onBuyReport(stock: Stock, volume: Int, price: Double)
  def onSellReport(stock: Stock, volume: Int, price: Double)

  private val myTickets = new ListBuffer[Ticket]
  val stocks = market.stocks
  private val position = new HashMap[Stock, Position]
  private var marketLast: mInfo = Map.empty[Stock, StockInfo]
  protected var lastTick: DateTime = new DateTime(0)

  private val sProvideLiquidity = new StrategyProvideLiquidity(this)

  def provideLiquidity(symbol: Stock, e: Event, qtd: Int, price: Double) = sProvideLiquidity.provideLiquidity(symbol: Stock, e: Event, qtd: Int, price: Double)
  
  def provideSellLiquidity(symbol: Stock, qtd: Int, price: Double) = provideLiquidity(symbol, Sell, qtd, price)
  def provideBuyLiquidity(symbol: Stock, qtd: Int, price: Double) = provideLiquidity(symbol, Buy, qtd, price)
  
  val windows = new WindowCaller[Double]
  def callback(): Unit
  private val strategyCallback = new SimpleCallBack(5000, callback)

 
   def midPrice(s: StockInfo) = Strategy.midPrice(s)

   def askPrice(st: Stock): Double = {
    val st2 = getSymbol(st)
    st2 match {
      case t: Trade => t.priceVol.price
      case q: Quote => q.ask.price
    }
  }

   def bidPrice(st: Stock): Double = {
    val st2 = getSymbol(st)
    st2 match {
      case t: Trade => t.priceVol.price
      case q: Quote => q.bid.price
    }
  }

  def createMAvg(stock: Stock, windowSize: Int, elapsed: Int) = {

    val newWin = new MovingAvg(windowSize, elapsed, () => {
      marketLast.get(stock) match {
        case Some(sI) => midPrice(sI)
        case _ => 0
      }
    })
    windows <-- newWin
    newWin
  }
  private implicit def stockInfo2Double(sI: Option[StockInfo]): Double = {
    sI match {
      case Some(s) => midPrice(s)
      case None => 0.0
    }
  }
  def createRatioFor2SymbolsMAvg(principal: Stock, ref1: Stock, ref2: Stock, windowSize: Int, elapsed: Int) = {

    val newWin = new MovingAvg(windowSize, elapsed, () => {
      val principalPrice: Double = marketLast.get(principal)
      val ref1Price: Double = marketLast.get(ref1)
      val ref2Price: Double = marketLast.get(ref2)

      val referencia = ref1Price * ref2Price
      val result = principalPrice over referencia

      result
    })
    windows <-- newWin
    newWin
  }
  
  
  def createRatioMAvgForOption(stock1: Stock, stock2: Stock, windowSize: Int, elapsed: Int) = {

	  val optionInfoB = stock2.optionInfo.get
    val volWin = new MovingAvg(windowSize, elapsed, () => {
      val precoA:Double = marketLast.get(stock1)
      val precoB:Double = marketLast.get(stock2)

      val bsVol = bsVolCall(precoA, optionInfoB.strike, optionInfoB.r, precoB, optionInfoB.ratio)
      
      bsVol
    })
    windows <-- volWin
    val newWin = new MovingAvg(windowSize, elapsed, () => {
      
      if(volWin.isAvailable) {
      val mediaVol = volWin.lastVal()
     
      val precoA:Double = marketLast.get(stock1)
      val precoB:Double = marketLast.get(stock2)
       
      
      val precoTeorico = priceEuropeanBlackScholesCall(precoA, optionInfoB.strike, optionInfoB.r, optionInfoB.ratio, mediaVol)
      

      val result = precoTeorico

      result
      } else 0
    })
    windows <-- newWin
    newWin
  }
  
  def createRatioMAvg(stock1: Stock, stock2: Stock, windowSize: Int, elapsed: Int) = {


    val newWin = new MovingAvg(windowSize, elapsed, () => {
      val precoA:Double = marketLast.get(stock1)
      val precoB:Double = marketLast.get(stock2)

      val result = precoA over precoB

      result
    })
    windows <-- newWin
    newWin
  }

  def init(): Parameters = {

    onStart()

    //!market
    windows <-- strategyCallback
    while (market) {

      val tick = !market
    		 
      val (marketInfo, ticketInfoNonFlat) = tick
      val ticketInfo = ticketInfoNonFlat
      if (marketInfo.size > 0) {

        val (_, someInfo) = marketInfo.head

        val period = new Period(lastTick, someInfo.iDatetime)
        val millis = period.getMillis()
        if (lastTick != new DateTime(0)) windows.call(millis)
        lastTick = someInfo.iDatetime

        //if(millis > 0) onQuotes() <-- Ja tem o filtro contador, nao precisa mais. <-- Setar WATCH SYMBOL
        sProvideLiquidity.updateAll()
        onQuotes()

        marketLast = marketInfo
      }

      for (ab <- ticketInfo) {
        updatePosition(List(ab))
        publicReports(List(ab))
      }

    }
    this.log("Strategy: Allocating result..")
    putResult("position", position)
    putResult("marketLast", marketLast)
    val partA = lastTick
    val dateStr = (List(partA.dayOfMonth().get(), partA.monthOfYear().get(), partA.year().get()).mkString("/"))
    putResult("date", dateStr)
    //Log(marketLast)
    onStop()
    result
  }

  def getSymbol(st: Stock) = {
    marketLast.get(st) match {
      case Some(sInfo) => sInfo
      case None => EmptyInfo(st)
    }
  }

  def getPosition(st: Stock) = {
    if (position.contains(st)) {
      position(st)
    } else {
      val nPos = Position(0, 0)
      position.put(st, nPos)
      nPos
    }
  }

  private def sendOrder(order: Order) = {
    // Log(order)
    //Log(position)
    val tunnelReport = tunnel.sendOrder(order)
    tunnelReport
  }

  def getStatus(tunnelTicket: TunnelTicketStatus) = {
    tunnelTicket match {
      case a: Wait =>
        val ret = tunnel.check(a)
        ret
      case r: Ready => tunnel.check(r)
      case x => x
    }
  }
  def buy(st: Stock, qt: Int, amount: Double) = {
    val buyOrder = BuyOrder(lastTick, st, qt, amount)
    sendOrder(buyOrder)
  }

  def exitPosition(st: Stock, step: Double) = {
    val position = getPosition(st).quantity

    if (position > 0) sell(st, position, bidPrice(st) - step) else buy(st, position * (-1), askPrice(st) + step)

  }
  private def replace(ticketToReplace: Ticket, newOrder: Order): TunnelTicketStatus = {

    val ticketNature = ticketToReplace.order.nature
    val orderNature = newOrder.nature
    if (ticketNature == orderNature) {
      orderNature match {
        case SellNature =>
          val orderToSend = SellReplaceOrder(ticketToReplace, newOrder.asInstanceOf[SellOrder])
          sendOrder(orderToSend)
        case BuyNature =>
          val orderToSend = BuyReplaceOrder(ticketToReplace, newOrder.asInstanceOf[BuyOrder])
          sendOrder(orderToSend)
      }
    } else new Killed
  }

  def replaceBuy(ticketToReplace: Ticket, qt: Int, amount: Double) = {
    val buyOrder = BuyOrder(lastTick, ticketToReplace.order.stock, qt, amount)
    replace(ticketToReplace, buyOrder)
  }

  def replaceSell(ticketToReplace: Ticket, qt: Int, amount: Double) = {
    val sellOrder = SellOrder(lastTick, ticketToReplace.order.stock, qt, amount)
    replace(ticketToReplace, sellOrder)
  }

  def appendWaitOrder(wait: Wait, qt: Int, amount: Double) = {
    val nature = wait.ticket.order.nature
    nature match {
      case SellNature =>
        val order = SellReplaceOrder(wait.ticket, SellOrder(lastTick, wait.ticket.order.stock, qt, amount))
        wait.appendReplace(order)
      case BuyNature =>
        val order = BuyReplaceOrder(wait.ticket, BuyOrder(lastTick, wait.ticket.order.stock, qt, amount))
        wait.appendReplace(order)
    }
  }

  def sell(st: Stock, qt: Int, amount: Double) = {
    val sellOrder = SellOrder(lastTick, st, qt, amount)
    sendOrder(sellOrder)
  }

  def cancelOrder(ticket: Ticket) = {
    market.cancelOrder(ticket)
  }
  private def onReport(ticketInfo: tInfo, buyReport: report, sellReport: report) = {
    ticketInfo match {
      case List((orderTicket, result: BuyOrderResult)) => {

        putResult("BuyOrder" + orderTicket.id.toString(), result)
        buyReport(orderTicket, result)
      }
      case List((orderTicket, result: SellOrderResult)) => {
        putResult("SellOrder" + orderTicket.id.toString(), result)
        sellReport(orderTicket, result)
      }
      case _ => {}
    }
  }

  private def updatePosition(ticketInfo: tInfo) = {
    def update(signal: Int)(orderTicket: Ticket, result: OrderResult) = {
      val order = orderTicket.order
      val stock = order.stock

      val qtd = result.quantity
      val price = result.value

      val actualPosition = getPosition(stock)
      val newPosition = Position(actualPosition.quantity + (qtd * signal), actualPosition.pnl - qtd * (price * signal))
      //Log(newPosition)
      position(stock) = newPosition
    }
    onReport(ticketInfo, update(1), update(-1))

  }

  private def publicReports(ticketInfo: tInfo) = {
    def assessInfo(buy: Boolean)(orderTicket: Ticket, result: OrderResult) = {
      val order = orderTicket.order
      val stock = order.stock

      val qtd = result.quantity
      val price = result.value
      if (buy) onBuyReport(stock, qtd, price) else onSellReport(stock, qtd, price)
    }

    onReport(ticketInfo, assessInfo(true), assessInfo(false))
  }

  private def updateMarketInfo(marketInfo: mInfo) = {

  }
}
