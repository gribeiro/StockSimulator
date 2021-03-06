package com.stocksimulator.abs
import com.github.nscala_time.time.Imports._
import scala.collection.mutable.ListBuffer
import scala.collection.mutable.HashMap
import com.stocksimulator.debug._
import scala.collection.mutable.LinkedHashMap
import scala.collection.mutable.ArrayBuffer
import scala.collection.mutable.LinkedHashSet
import com.stocksimulator.abs.EventTC._

object Market {
  implicit def market2Boolean(m: Market) = m()
}
abstract class Market(val components: List[MarketComponent] = List.empty[MarketComponent]) {
  
  //Ticket id
  type tickResult = (Map[Stock, StockInfo], List[(Ticket, OrderResult)])
  val stocks: Set[Stock]
  protected var tickets = new LinkedHashSet[Ticket]
  private var ticketsBeforeProcessing = new LinkedHashSet[Ticket]
  protected val buyBook = new LinkedHashMap[Ticket, Book]
  protected val sellBook = new LinkedHashMap[Ticket, Book]
  protected val tProvider = new TicketProvider(afterCancel)
  protected val emptyResult = LinkedHashSet.empty[(Ticket, OrderResult)]
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
  protected def removeTicket(t: Ticket):Unit = {
    tProvider.removeOrder(t, afterCancel(t))
  }
  
    def executeOrder(datetime: DateTime, vol: Int, price: Double, instrument: Stock)(t: Ticket) = {
     event(t) match {
        case Buy => t -> BuyOrderResult(datetime, math.min(t.order.quantity, vol), price, instrument )
        case Sell => t -> SellOrderResult(datetime, math.min(t.order.quantity, vol), price, instrument)
      } 

  }
  
  protected def removeTickets(ts: Iterable[Ticket]) = {
    for (t <- ts) {
     removeTicket(t)
    }
  }

  def sendOrder(order: Order): Ticket = {
	tProvider.sendOrder(order)
  }

  def cancelOrder(ticket: Ticket) = {
    tProvider.cancelOrder(ticket, afterCancel(ticket))
  }
}