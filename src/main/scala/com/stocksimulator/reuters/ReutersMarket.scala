package com.stocksimulator.reuters
import com.stocksimulator.abs._
import com.stocksimulator.debug._
import scala.collection.mutable.ListBuffer
import scala.collection.mutable.HashMap
import scala.collection.mutable.LinkedHashMap
import com.stocksimulator.main.RBSFactory
import io.jvm.uuid._
import scala.collection.mutable.HashSet
import com.stocksimulator.helpers.ImplicitClasses._
import com.stocksimulator.helpers.ImplicitClasses
import com.stocksimulator.abs.TicketProvider

class ReutersMarket(feed: Feed, mc: List[MarketComponent], marketDelay: Int = RBSFactory.delay) extends Market(mc) {

  var oldInfo: Map[Stock, StockInfo] = Map.empty[Stock, StockInfo]

  val processed: HashSet[UUID] = HashSet.empty[UUID]
  val filters = mc.collect {
    case a: MarketComponentFilter => a
  }
  override val tProvider = if (marketDelay > 0) new DelayedTicketProvider(afterCancel, marketDelay) else new TicketProvider(afterCancel)

  val bookOrder = mc.collectFirst { case s: BookOrderComponent => s } match {
    case Some(a) => a
    case None => ReutersMarketComponents.standardBookOrder(1)
  }

  val ticketResult = new HashMap[Ticket, OrderResult]
  val emptyFeed = Map.empty[Stock, StockInfo]
  val timeControl = new TimeControl(feed.instruments)
  val cancelDetector = new CancelDetector

  Log("Creating time feed control...")
  while(feed) { 
    val infoToFeed = !feed
    //Log(infoToFeed)
    timeControl <-- infoToFeed 
  }
  Log("Time feed created!")
  def childBeforeTick() = {

    tProvider match {
      case delayed: DelayedTicketProvider =>
        if (timeControl.hasTime) delayed.timeHasPassed(timeControl.timePassed)
      case _ => {}
    }

  }
  def childTick() = {
    if (feed || timeControl) performTick() else throw new Exception
  }

  def isActive(): Boolean = timeControl

  private def performTick(): (Map[Stock, StockInfo], ListBuffer[(Ticket, OrderResult)]) = {

    val (buyTickets, sellTickets) = tickets.buyAndSellPartition
    val newInfo = oldInfo
    var results = new ListBuffer[(Ticket, OrderResult)]
    Log(newInfo)
    if (buyBook.size != 0 || sellBook.size != 0 || tickets.size != 0) {
      for ((stock, info) <- newInfo) yield {
        if (!info.hasAppeared) {
          info.hasAppeared = true
          info match {
            case q: Quote =>
              for (t <- updateTicketsOnQuote(q, stock)) {
                results += t
              }
            case t: Trade => for (t <- updateTicketsOnTrade(t, stock)) {
              results += t
            }
          }
        }
      }
    } else {
      emptyResult
    }

    oldInfo = !timeControl
    if(results.size > 0) {		
    
    def ticketVal(t: Iterable[Ticket]) = t.head.order.value
    Log("Tick atual:" + newInfo, true)
    if(buyTickets.size > 0) Log("Ordem de compra: " + ticketVal(buyTickets), true)
    if(sellTickets.size > 0) Log("Ordem de venda: " + ticketVal(sellTickets), true)
    Log("Order Result: "+  results, true)
    Log("\n", true)
    }
    val filterResult = if (results.size > 0) (filters.foldLeft(true) { _ && _.filter() }) || components.size == 0 else true
    if (filterResult) (newInfo, results) else (emptyInfo, results)
  }

  private def updateTicketsOnQuote(quote: Quote, stock: Stock) = {

    filters.foreach { comp => comp.onQuote(quote, stock) }
    val tickets2 = tickets.getStock(stock)

    val (buyTickets, sellTickets) = tickets2.buyAndSellPartition

    val bidPrice = quote.bid.price
    val askPrice = quote.ask.price
    val bidVol = quote.bid.vol
    val askVol = quote.ask.vol
    val datetime = quote.datetime

    val actionBuyTickets = buyTickets ~>= askPrice //buyTickets.filter(t => t.order.value >= askPrice)
    val actionSellTickets = sellTickets ~<= bidPrice //sellTickets.filter(t => t.order.value <= bidPrice)

    val buyOrderResults = actionBuyTickets ! (datetime, askVol, askPrice) //actionBuyTickets.map(t => t -> BuyOrderResult(datetime, math.min(t.order.quantity, askVol), askPrice))
    val sellOrderResults = actionSellTickets ! (datetime, bidVol, bidPrice) //actionSellTickets.map(t => t -> SellOrderResult(datetime, math.min(t.order.quantity, bidVol), bidPrice))

    //  val combinedResults = buyOrderResults ++ sellOrderResults
    removeTickets(actionBuyTickets)
    removeTickets(actionSellTickets)

    //Book stuff

    val actionQueueBuy = buyTickets ~== bidPrice //buyTickets.filter(t => t.order.value == bidPrice)
    val actionQueueSell = sellTickets ~== askPrice //sellTickets.filter(t => t.order.value == askPrice)

    //adiciona no book - valor atual
    val tempTicketBookBuy = new HashMap[Ticket, Book]
    for (actionTicket <- actionQueueBuy) {
      if (!buyBook.contains(actionTicket)) tempTicketBookBuy += actionTicket -> new Book(bidVol)
    }

    buyBook ++= tempTicketBookBuy

    val tempTicketBookSell = new HashMap[Ticket, Book]
    for (actionTicket <- actionQueueSell) {
      if (!sellBook.contains(actionTicket)) tempTicketBookSell += actionTicket -> new Book(askVol)
    }
    sellBook ++= tempTicketBookSell

    //adiciona no book - valor distante

    val actionFirstQueueBuy = buyTickets ~< bidPrice //buyTickets.filter(t => t.order.value < bidPrice)
    val actionFirstQueueSell = sellTickets ~> askPrice //sellTickets.filter(t => t.order.value > askPrice)

    val tempTicketBookBuyFirst = new LinkedHashMap[Ticket, Book]
    for (actionTicket <- actionFirstQueueBuy) {
      tempTicketBookBuyFirst ++= bookOrder.buyOrder(buyBook, actionTicket)
    }

    buyBook ++= tempTicketBookBuyFirst

    val tempTicketBookSellFirst = new LinkedHashMap[Ticket, Book]
    for (actionTicket <- actionFirstQueueSell) {
      tempTicketBookSellFirst ++= bookOrder.sellOrder(sellBook, actionTicket)
    }
    sellBook ++= tempTicketBookSellFirst

    //***************** Cancel Logic

    cancelDetector.quoteTouch(stock, quote.ask.price, quote.ask.vol)
    cancelDetector.quoteTouch(stock, quote.bid.price, quote.bid.vol)
    //******** 

    val tZeroBuy = buyBook ? bidPrice

    val tZeroSell = sellBook ? askPrice

    val tZeroResultBuy = tZeroBuy ! (datetime, bidVol, bidPrice) //tZeroBuy.map(t => t -> BuyOrderResult(datetime, math.min(t.order.quantity, bidVol), bidPrice))
    val tZeroResultSell = tZeroSell ! (datetime, askVol, askPrice) //tZeroSell.map(t => t -> SellOrderResult(datetime, math.min(t.order.quantity, askVol), askPrice))

    removeTickets(tZeroBuy)
    removeTickets(tZeroSell)
    val ret2 = new ListBuffer[(Ticket, OrderResult)]
    // val lis = List(buyOrderResults, sellOrderResults, tZeroResultBuy, tZeroResultSell)

    for (l <- buyOrderResults) {
      ret2 += l
    }
    for (l <- sellOrderResults) {
      ret2 += l
    }
    for (l <- tZeroResultBuy) {
      ret2 += l
    }
    for (l <- tZeroResultSell) {
      ret2 += l
    }
    // val ret = combinedResults ++ tZeroResultBuy ++ tZeroResultSell

    ret2
  }

  private def updateTicketsOnTrade(trade: Trade, stock: Stock) = {
    filters.foreach { comp => comp.onTrade(trade, stock) }
    val tickets2 = tickets.getStock(stock)

    val (buyTickets, sellTickets) = tickets2.buyAndSellPartition

    val (price, vol) = trade.priceVol.getPair
    val datetime = trade.datetime

    //Detect Trade
    /* val actionBuyTickets = buyTickets.filter(t => t.order.value > price)
    val actionSellTickets = sellTickets.filter(t => t.order.value < price)

    val buyOrderResults = actionBuyTickets.map(t => t -> BuyOrderResult(datetime, math.min(t.order.quantity, vol), price))
    val sellOrderResults = actionSellTickets.map(t => t -> SellOrderResult(datetime, math.min(t.order.quantity, vol), price))

    val combinedResults = buyOrderResults ++ sellOrderResults
    removeTickets(actionBuyTickets)
    removeTickets(actionSellTickets)*/

    val actionQueueBuy = buyTickets ~== price //buyTickets.filter(t => t.order.value == trade.priceVol.price)
    val actionQueueSell = sellTickets ~== price //sellTickets.filter(t => t.order.value == trade.priceVol.price)

    val bidCancelled = cancelDetector.check(stock, price)
    if (bidCancelled > 0) {
      for ((ticket, book) <- buyBook; actionTicket <- actionQueueBuy; if (ticket == actionTicket)) {
        buyBook(ticket).incompleteFeed(bidCancelled)
      }
      for ((ticket, book) <- sellBook; actionTicket <- actionQueueSell; if (ticket == actionTicket)) {
        sellBook(ticket).incompleteFeed(bidCancelled)
      }
    }

    //Book stuff

    //Atualiza book
    for ((ticket, book) <- buyBook; actionTicket <- actionQueueBuy; if (ticket == actionTicket)) {
      buyBook(ticket).feed(trade.priceVol.vol)
    }

    for ((ticket, book) <- sellBook; actionTicket <- actionQueueSell; if (ticket == actionTicket)) {
      sellBook(ticket).feed(trade.priceVol.vol)
    }
    //*************

    cancelDetector.tradeTouch(stock, trade.priceVol.price, trade.priceVol.vol)

    val tZeroBuy = buyBook ? price
    val tZeroSell = sellBook ? price

    val tZeroResultBuy = tZeroBuy ! (datetime, vol, price) //tZeroBuy.map(t => t -> BuyOrderResult(datetime, math.min(t.order.quantity, vol), price))
    val tZeroResultSell = tZeroSell ! (datetime, vol, price) //tZeroSell.map(t => t -> SellOrderResult(datetime, math.min(t.order.quantity, vol), price))
    removeTickets(tZeroBuy)
    removeTickets(tZeroSell)
    val buffer = new ListBuffer[(Ticket, OrderResult)]

    for (b <- tZeroResultBuy) {
      buffer += b
    }
    for (s <- tZeroResultSell) {
      buffer += s
    }

    // val ret = tZeroResultBuy ++ tZeroResultSell //++ combinedResults
    //ret.to[ListBuffer]
    buffer
  }

}
