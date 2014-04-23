package com.stocksimulator.reuters
import com.stocksimulator.abs._
import com.stocksimulator.debug._
import com.stocksimulator.debug.LogNames._
import scala.collection.mutable.ListBuffer
import scala.collection.mutable.HashMap
import scala.collection.mutable.LinkedHashMap

import io.jvm.uuid._
import scala.collection.mutable.HashSet
import com.stocksimulator.helpers.ImplicitClasses._
import com.stocksimulator.helpers.ImplicitClasses
import com.stocksimulator.abs.TicketProvider
import scala.concurrent._
import scala.concurrent.duration._
import com.stocksimulator.abs.AutoDemotion._
import ExecutionContext.Implicits.global
import scala.util.Success
import scala.util.Failure

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

  val ticketResult = new HashMap[Ticket, OrderResult]
  val emptyFeed = Map.empty[Stock, StockInfo]
  val timeControl = new TimeControl(feed.instruments)
  val cancelDetector = new CancelDetector

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

  private def performTick(oldInfo2: Map[Stock, StockInfoHA]): (Map[Stock, StockInfo], ListBuffer[(Ticket, OrderResult)]) = {

    val (buyTickets, sellTickets) = tickets.buyAndSellPartition
    val newInfo = oldInfo2
    
    val results = new ListBuffer[(Ticket, OrderResult)]
     //println(newInfo)
    if (buyBook.size != 0 || sellBook.size != 0 || tickets.size != 0) {
      for ((stock, info) <- newInfo) yield {
        if (!info.hasAppeared) {
           
          info.setHasAppeared(true)
          info.unfold match {
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

   
     val sendInfo: Map[Stock, StockInfo] = newInfo.map {
      case (stock, stockInfoHA) =>
        stock -> stockInfoHA.demote
    }
    if(results.size > 0 && false) {		

    def ticketVal(t: Iterable[Ticket]) = t.head.order.value
    this.log("Tick atual:" + sendInfo)
    if(buyTickets.size > 0) Log("Ordem de compra: " + ticketVal(buyTickets))
    if(sellTickets.size > 0) Log("Ordem de venda: " + ticketVal(sellTickets))
    this.log("Order Result: "+  results)
    this.log("\n")

    }
    val filterResult = if (results.size > 0) (filters.foldLeft(true) { _ && _.filter() }) || components.size == 0 else true
  
    if (filterResult) (sendInfo, results) else (emptyInfo, results)
  }

  private def updateTicketsOnQuote(quote: Quote, stock: Stock) = {

    filters.foreach { comp => comp.onQuote(quote, stock) }
    val tickets2 = tickets.getStock(stock)

    val buyTickets = tickets2.buys
    val sellTickets = tickets2.sells

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
