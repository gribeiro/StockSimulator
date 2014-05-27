package com.stocksimulator.abs

import scala.collection.mutable.ArrayBuffer
import scala.collection.mutable.HashMap
import com.stocksimulator.debug.Log

trait TunnelTicketStatus

case class Ready(ticket: Ticket) extends TunnelTicketStatus
case class Wait(id: Int, ticket: Ticket) extends TunnelTicketStatus {
  var myOrder: Option[Order] = None
  def appendReplace(order: Order) = myOrder = Some(order)
  
}
case class Killed() extends TunnelTicketStatus

class StrategyTicketTunnel(market: Market) {
  private var lID = 0
  private val buffer = new HashMap[Int, Ticket]
  private def tickets = market.getTickets

  private def lastId(): Int = market.lastId
  private def checkTicketExistence(t: Ticket) = tickets.exists(t2 => t == t2)
  def sendOrder(order: Order): TunnelTicketStatus = {
    val ticket = market.sendOrder(order)
    
    val ticketId = ticket.id
    if (lastId < ticketId) {
      buffer += ticket.id -> ticket
      Wait(ticket.id, ticket)
    } else Ready(ticket)
  }

  def check(wait: Wait): TunnelTicketStatus = {
    val id = wait.id
    if(id==14) {
      val stop =1
    }
    if (lastId >= id) {
      val ticket = buffer(id)
      buffer -= id
      if (checkTicketExistence(ticket) && ticket.order != null) {
        wait.myOrder match {
          case Some(order) => sendOrder(order)
          case None => Ready(ticket)
        }
        
        } else new Killed
    } else wait
  }
  
  def check(ready: Ready): TunnelTicketStatus = {
    if (checkTicketExistence(ready.ticket) && ready.ticket.order != null) ready else new Killed
  }
}