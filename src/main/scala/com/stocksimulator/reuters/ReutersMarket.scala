package com.stocksimulator.reuters
import com.stocksimulator.abs._
import com.stocksimulator.debug._
import com.stocksimulator.debug.LogNames._
import scala.collection.mutable.ListBuffer
import scala.collection.mutable.HashMap
import scala.collection.mutable.LinkedHashMap
import scala.collection.mutable.ArrayBuffer

import io.jvm.uuid._
import scala.collection.mutable.HashSet
import com.stocksimulator.helpers.ImplicitClasses._
import com.stocksimulator.helpers.ImplicitClasses
import com.stocksimulator.abs.TicketProvider
import scala.concurrent._
import scala.concurrent.duration._
import com.stocksimulator.abs.EventTC._
import com.stocksimulator.abs.AutoDemotion._
import ExecutionContext.Implicits.global
import scala.util.Success
import scala.util.Failure
import org.joda.time._
class ReutersMarket(feed: Feed, mc: List[MarketComponent], marketDelay: Int = 100) extends Market(mc) {
  val stocks = feed.instruments

  val filters = mc.collect {
    case a: MarketComponentFilter => a
  }
  override val tProvider = if (marketDelay > 0) new DelayedTicketProvider(afterCancel, marketDelay) else new TicketProvider(afterCancel)

  val bookOrder = mc.collectFirst { case s: BookOrderComponent => s } match {
    case Some(a) => a
    case None => ReutersMarketComponents.standardBookOrder(30)
  }

  private val ticketResult = new HashMap[Ticket, OrderResult]
  private val emptyFeed = Map.empty[Stock, StockInfo]
  private val timeControl = new TimeControl(feed.instruments)
  private val cancelDetector = new CancelDetector

  this.log("Creating time feed control...")
  feed.foreach { 
    infoToFeed =>
    //Log(infoToFeed)
    timeControl <-- infoToFeed.map {
   
      case (stockKey, stockInfo) => 
        
        stockKey -> stockInfo.promoteTo[StockInfoHA]
    }
  }
  this.log("Time feed created!")
  def childBeforeTick() = {

    tProvider match {
      case delayed: DelayedTicketProvider =>
        if (timeControl.hasTime) delayed.timeHasPassed(timeControl.timePassed)
      case _ => {}
    }

  }
  def childTick() = {
   // Log("childTick Awaiting...")
   //val callTimeControl = 
  // Log(callTimeControl)
   if (timeControl) performTick(!timeControl) else throw new Exception
    
  }

  def isActive(): Boolean = timeControl
  val nDup = ArrayBuffer.empty[(Stock, DateTime, Event)]
  private def performTick(oldInfo2: Map[Stock, StockInfoHA]): (Map[Stock, StockInfo], List[(Ticket, OrderResult)]) = {

    val (buyTickets, sellTickets) = tickets.buyAndSellPartition
    val newInfo = oldInfo2
    
    val results = new ListBuffer[(Ticket, OrderResult)]
     //println(newInfo)
    if (buyBook.size != 0 || sellBook.size != 0 || tickets.size != 0) {
      for ((_, info) <- newInfo) yield {
        if (!info.hasAppeared) {
           
          info.setHasAppeared(true) //Side Effect
          info.unfold match {
            case q: Quote =>
             
              for (t <- updateTicketsOnQuote(q, q.iStock)) {
                results += t
              }
            case t: Trade => for (t <- updateTicketsOnTrade(t, t.iStock)) {
              results += t
            }
          }
        } else {
         // this.log("Warning: Duplicate info was retrieved!")
        }
      }
    }

   
     val sendInfo: Map[Stock, StockInfo] = newInfo.map {
      case (stock, stockInfoHA) =>
        stock -> stockInfoHA.demote
    }
   for(oneResult <- results) {		
    val rEvent = event(oneResult._1.order) 
    val strEvent = rEvent match {
      case Buy => "COMPRA"
      case _ => "VENDA"
    }
    val rStock = oneResult._1.order.stock
    val rPrice = oneResult._1.order.value
    val rVol = oneResult._1.order.quantity
    val rDt = oneResult._1.order.dateTime
    
    
    val nDupChk = (rStock, rDt, rEvent)
    if(!nDup.contains(nDupChk)) {
      nDup += nDupChk
      val printStr = List(strEvent, rStock.name, "PREÇO", rPrice, "QTD", rVol) mkString " "
      println(printStr)
    }
    //def ticketVal(t: Iterable[Ticket]) = t.head.order.value
    //this.log("Tick atual:" + sendInfo)
    //if(buyTickets.size > 0) Log("Ordem de compra: " + buyTickets)
    //if(sellTickets.size > 0) Log("Ordem de venda: " + sellTickets)
    
    
    }
    val filterResult = if (results.size > 0) (filters.foldLeft(true) { _ && _.filter() }) || components.size == 0 else true
  
    if (filterResult) (sendInfo, results.toList) else (emptyInfo, results.toList)
  }

  //Warning!! Ugly side effects!!
  
  
  
  private def executeObviousSellTrades(sellTickets: Iterable[Ticket], quote: Quote, stock: Stock) = {
	val bidPrice = quote.bid.price
    val askPrice = quote.ask.price
    val bidVol = quote.bid.vol
    val askVol = quote.ask.vol
    val datetime = quote.datetime

    val sellExec = executeOrder(datetime, bidVol, bidPrice, stock) _
    val actionSellTickets = sellTickets ~<= bidPrice //sellTickets.filter(t => t.order.value <= bidPrice)
    val sellOrderResults = actionSellTickets.map(sellExec)
    actionSellTickets.map(removeTicket)
    sellOrderResults
  }
  
  private def executeObviousBuyTrades(buyTickets: Iterable[Ticket], quote: Quote, stock: Stock) = {
    val bidPrice = quote.bid.price
    val askPrice = quote.ask.price
    val bidVol = quote.bid.vol
    val askVol = quote.ask.vol
    val datetime = quote.datetime
    
    val actionBuyTickets = buyTickets ~>= askPrice //buyTickets.filter(t => t.order.value >= askPrice)
    val buyExec = executeOrder(datetime, askVol, askPrice, stock) _
    val buyOrderResults = actionBuyTickets.map(buyExec) //actionBuyTickets.map(t => t -> BuyOrderResult(datetime, math.min(t.order.quantity, askVol), askPrice))
    actionBuyTickets.map(removeTicket)

    buyOrderResults
  }
  private def updateTicketsOnQuote(quote: Quote, stock: Stock) = {

    filters.foreach { comp => comp.onQuote(quote, stock) } // Calling Side Effect
    val tickets2 = tickets.getStock(stock)

    val buyTickets = tickets2.buys
    val sellTickets = tickets2.sells

    val bidPrice = quote.bid.price
    val askPrice = quote.ask.price
    val bidVol = quote.bid.vol
    val askVol = quote.ask.vol
    val datetime = quote.datetime
    
   val buyOrderResults = if(buyTickets.size > 0 ) executeObviousBuyTrades(buyTickets, quote, stock) else Iterable.empty[(Ticket, OrderResult)]
   val sellOrderResults = if(sellTickets.size > 0) executeObviousSellTrades(sellTickets, quote, stock)  else Iterable.empty[(Ticket, OrderResult)]
    //Side Effect #1
    //  val combinedResults = buyOrderResults ++ sellOrderResults
    
    //Book stuff

    val actionQueueBuy = buyTickets ~== bidPrice //buyTickets.filter(t => t.order.value == bidPrice)
    val actionQueueSell = sellTickets ~== askPrice //sellTickets.filter(t => t.order.value == askPrice)

    //adiciona no book - valor atual
    //val tempTicketBookBuy = new HashMap[Ticket, Book]
    //Side Effect #2
    for (actionTicket <- actionQueueBuy) {
      if (!buyBook.contains(actionTicket)) buyBook += actionTicket -> new Book(bidVol)
    }

    //buyBook ++= tempTicketBookBuy

    //val tempTicketBookSell = new HashMap[Ticket, Book]
    //Side Effect #3
    for (actionTicket <- actionQueueSell) {
      if (!sellBook.contains(actionTicket)) sellBook += actionTicket -> new Book(askVol)
    }
    //sellBook ++= tempTicketBookSell

    //adiciona no book - valor distante

    val actionFirstQueueBuy = buyTickets ~< bidPrice //buyTickets.filter(t => t.order.value < bidPrice)
    val actionFirstQueueSell = sellTickets ~> askPrice //sellTickets.filter(t => t.order.value > askPrice)

    
    //Side Effect #4
    //val tempTicketBookBuyFirst = new LinkedHashMap[Ticket, Book]
    for (actionTicket <- actionFirstQueueBuy) {
      buyBook ++= bookOrder.buyOrder(buyBook, actionTicket)
    }

    //buyBook ++= tempTicketBookBuyFirst

   // val tempTicketBookSellFirst = new LinkedHashMap[Ticket, Book]
    //Side Effect #4
    for (actionTicket <- actionFirstQueueSell) {
      sellBook ++= bookOrder.sellOrder(sellBook, actionTicket)
    }
    //sellBook ++= tempTicketBookSellFirst

    //***************** Cancel Logic
//Side Effect #5
    cancelDetector.quoteTouch(stock, quote.ask.price, quote.ask.vol)
    cancelDetector.quoteTouch(stock, quote.bid.price, quote.bid.vol)
    //******** 

    val tZeroBuy = buyBook ? bidPrice

    val tZeroSell = sellBook ? askPrice

    val tZeroResultBuy = tZeroBuy ! (datetime, bidVol, bidPrice, stock) //tZeroBuy.map(t => t -> BuyOrderResult(datetime, math.min(t.order.quantity, bidVol), bidPrice))
    val tZeroResultSell = tZeroSell ! (datetime, askVol, askPrice, stock) //tZeroSell.map(t => t -> SellOrderResult(datetime, math.min(t.order.quantity, askVol), askPrice))

    //Side Effect #6
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

  //Warning!! Ugly side effects!!
  private def updateTicketsOnTrade(trade: Trade, stock: Stock) = {
    filters.foreach { comp => comp.onTrade(trade, stock) }
    val tickets2 = tickets.getStock(stock)
    
    val buyTickets = tickets2.buys
    val sellTickets = tickets2.sells
    
    val price = trade.priceVol.price
    val vol = trade.priceVol.vol

    val datetime = trade.datetime



    val actionQueueBuy = buyTickets ~== price //buyTickets.filter(t => t.order.value == trade.priceVol.price)
    val actionQueueSell = sellTickets ~== price //sellTickets.filter(t => t.order.value == trade.priceVol.price)

    val bidCancelled = cancelDetector.check(stock, price)
    if (bidCancelled > 0) {
      for (ticket <- buyBook.keySet; actionTicket <- actionQueueBuy; if (ticket == actionTicket)) {
        buyBook(ticket).incompleteFeed(bidCancelled)
      }
      for (ticket <- sellBook.keySet; actionTicket <- actionQueueSell; if (ticket == actionTicket)) {
        sellBook(ticket).incompleteFeed(bidCancelled)
      }
    }

    //Book stuff

    //Atualiza book
    for (ticket <- buyBook.keySet; actionTicket <- actionQueueBuy; if (ticket == actionTicket)) {
      buyBook(ticket).feed(trade.priceVol.vol)
    }

    for (ticket <- sellBook.keySet; actionTicket <- actionQueueSell; if (ticket == actionTicket)) {
      sellBook(ticket).feed(trade.priceVol.vol)
    }
    //*************

    cancelDetector.tradeTouch(stock, trade.priceVol.price, trade.priceVol.vol)

    val tZeroBuy = buyBook ? price
    val tZeroSell = sellBook ? price

    val tZeroResultBuy = tZeroBuy ! (datetime, vol, price, stock) //tZeroBuy.map(t => t -> BuyOrderResult(datetime, math.min(t.order.quantity, vol), price))
    val tZeroResultSell = tZeroSell ! (datetime, vol, price, stock) //tZeroSell.map(t => t -> SellOrderResult(datetime, math.min(t.order.quantity, vol), price))
    removeTickets(tZeroBuy)
    removeTickets(tZeroSell)
    val buffer = new ListBuffer[(Ticket, OrderResult)]

    for (b <- tZeroResultBuy) {
      buffer += b
    }
    for (s <- tZeroResultSell) {
      buffer += s
    }

    buffer
  }

}
