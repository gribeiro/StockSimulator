package com.stocksimulator.abs

import scala.collection.mutable.ListBuffer
import scala.collection.mutable.PriorityQueue
import scala.collection.mutable.HashMap
import com.stocksimulator.debug._
import scala.collection.mutable.LinkedHashMap
import com.stocksimulator.helpers.ImplicitClasses._
import com.stocksimulator.macros._
class TicketProvider(defaultAfterCancel: (Ticket) => () => Unit) {

  
  protected val tickets = new ListBuffer[Ticket]
  protected val replace = new LinkedHashMap[Int, Int]
 
  protected var idCounter = 0;
  protected var lId = 0
  
  def lastId = { 
    val idList = crudeSnapshot.map(f => f._id)
    val newMax = if(idList.size > 0) idList.max else 0
    lId = math.max(lId, newMax)
    lId
  }
  def snapshot = tickets.filter(t => t.order != null)
  def crudeSnapshot = tickets
  
  protected def hasTicket(ticket: Ticket): Boolean = snapshot has ticket
  
  protected def sendOrderDefault(order: Order): Ticket = {
    idCounter += 1
    order match {
      case replaceOrder: ReplaceOrder =>
        val lastTicket = replaceOrder.ticketToReplace
        if (hasTicket(lastTicket)) {
          val newOrder = replaceOrder.newOrder
          val getNewOrder = sendOrder(newOrder)
          replace += lastTicket.id -> getNewOrder.id
          cancelOrder(lastTicket, defaultAfterCancel(lastTicket))
          getNewOrder
        } else {
          val dullTicket = Ticket(idCounter, null)
          tickets += dullTicket
          dullTicket
        }
      case _ =>
        val newTicket = Ticket(idCounter, order)
        tickets += newTicket
        
        newTicket
    }
  }
  def sendOrder(order: Order): Ticket = {

    sendOrderDefault(order)

  }

  protected def cancelOrderDefault(ticket: Ticket, afterCancel: () => Unit) = {
    tickets -= ticket
    afterCancel()
  }
  def cancelOrder(ticket: Ticket, afterCancel: () => Unit) = {
    cancelOrderDefault(ticket, afterCancel)
  }

  def removeOrder(ticket: Ticket, afterCancel: () => Unit) = {
    cancelOrderDefault(ticket, afterCancel)
  }

}

class DelayedTicketProvider(defaultAfterCancel: (Ticket) => () => Unit, delay: Int) extends TicketProvider(defaultAfterCancel) {

  private val ticketToRun = new LinkedHashMap[Ticket, Long]
  private val ticketToCancel = new LinkedHashMap[(Ticket, () => Unit), Long]
  
  override def snapshot = crudeSnapshot.filter(t => t.order != null)
  override def crudeSnapshot = {

    
    for (idToCheck <- replace.keys; if (!tickets.exists(t => t.id == idToCheck))) {
      for (ticket <- tickets; if (ticket.id == replace(idToCheck))) {
        removeOrder(ticket, defaultAfterCancel(ticket))
      }
    }

    val cancel = ticketToCancel.filter {
      case (_, timePassed) => timePassed <= 0
    }
   
    cancel.foreach {
      case ((lTicket, lAfterCancel), _) =>
        ticketToRun.remove(lTicket) //ticket
        replace.remove(lTicket.id) //ticket id
        tickets -= lTicket // ticket
        cancelOrderDefault(lTicket, lAfterCancel) //ticket & afterCancel
    }

    val validTickets = tickets.filter {
      p =>
        ticketToRun.get(p) match {
          case Some(number) => number <= 0
          case None => false
        }
    }

    
    for (((lTicket, lAfterCancel), _) <- ticketToCancel; if (!(tickets has lTicket)) ) {
      ticketToCancel.remove((lTicket, lAfterCancel))
      ticketToRun.remove(lTicket)
    }


    validTickets
    
  }
  override def sendOrder(order: Order): Ticket = {
    val ticket = sendOrderDefault(order)
    if (ticket.order != null) ticketToRun += ticket -> delay
    
    
    ticket
  }

  private def updateHashMap[T](hm: LinkedHashMap[T, Long], updateFun: (Long) => Long) = {
    hm.par.foreach {
      case (key, timePassed) =>
        hm.update(key, updateFun(timePassed))
    }
  }
  def timeHasPassed(mSeconds: Long) = {
    def updateFun(vl: Long) = if (vl > 0) vl - mSeconds else vl

    updateHashMap(ticketToRun, updateFun)
    updateHashMap(ticketToCancel, updateFun)
        val preValidTickets = tickets.filter {
      p =>
        ticketToRun.get(p) match {
          case Some(number) => number <= 0
          case None => false
        }
    }
    
       val unvalidTickets = tickets.filter {
      p =>
        ticketToRun.get(p) match {
          case Some(number) => number > 0
          case None => false
        }
    }

    if(preValidTickets.size > 0) {
      lId = math.max(lId, preValidTickets.map(p => p.id).max)
    } else if(unvalidTickets.size == 0) lId = idCounter

  }
  override def cancelOrder(ticket: Ticket, afterCancel: () => Unit) = {
    ticketToCancel += (ticket, afterCancel) -> delay

  }
}