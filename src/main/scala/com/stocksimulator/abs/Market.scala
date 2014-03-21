package com.stocksimulator.abs
import com.github.nscala_time.time.Imports._
import scala.collection.mutable.ListBuffer
import scala.collection.mutable.HashMap
import com.stocksimulator.debug._
import scala.collection.mutable.LinkedHashMap


object Market {
  implicit def market2Boolean(m: Market) = m()
}
abstract class Market(val components: List[MarketComponent] = List.empty[MarketComponent]) {
  //Ticket id
  type tickResult = (Map[Stock, StockInfo], ListBuffer[(Ticket, OrderResult)])
  
  protected var tickets = new ListBuffer[Ticket]
  private var ticketsBeforeProcessing = new ListBuffer[Ticket]
  protected val buyBook = new LinkedHashMap[Ticket, Book]
  protected val sellBook = new LinkedHashMap[Ticket, Book]
  protected val tProvider = new TicketProvider(afterCancel)
  protected val emptyResult = ListBuffer.empty[(Ticket, OrderResult)]
  protected val emptyInfo = Map.empty[Stock, StockInfo]

  
  def getTickets() = tProvider.crudeSnapshot
  def lastId() = tProvider.lastId
  def childBeforeTick()
  def beforeTick() = {
	  tickets = tProvider.snapshot

	  ticketsBeforeProcessing = tProvider.snapshot
  }
  def childTick(): tickResult
  
  def tick(): tickResult = {
    childBeforeTick()
    beforeTick()
    childTick()
  }
  def unary_! = tick()
  def isActive(): Boolean
  def apply() = isActive()
  def afterCancel(ticket: Ticket):() => Unit = { 
    () =>
    	buyBook.remove(ticket)
    	sellBook.remove(ticket)
  }
  protected def removeTickets(ts: Iterable[Ticket]) = {
    for (t <- ts) {
      tProvider.removeOrder(t, afterCancel(t))
    }
  }

  def sendOrder(order: Order): Ticket = {
	tProvider.sendOrder(order)
  }

  def cancelOrder(ticket: Ticket) = {
    tProvider.cancelOrder(ticket, afterCancel(ticket))
  }
}