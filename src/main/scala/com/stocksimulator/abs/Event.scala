package com.stocksimulator.abs

sealed trait Event
object Buy extends Event
object Sell extends Event
object Neutral extends Event

object EventTC {
  
  
  def event[T: EventOf](eventable: T) = {
    val ev = implicitly[EventOf[T]]
    ev.event(eventable)
  }
  
  def natureCompare[T: EventOf](a: T, b: T):Boolean = {
    val ev = implicitly[EventOf[T]]
    val aEvent = ev.event(a)
    val bEvent = ev.event(b)
    
    (aEvent, bEvent) match {
      case (Neutral, _) => true
      case (_, Neutral) => true
      case (e, f) if(e==f) => true
      case (_,_) => false
    }
  }
  trait EventOf[T] {
    def event(obj: T): Event
  }

  class IncompatibleNatureException extends Exception
  
  implicit object EventOfEvent extends EventOf[Event] {
    def event(obj: Event) = obj
  }

  implicit object EventOfOrder extends EventOf[Order] {
    def event(obj: Order) = {
      obj match {
        case _: BuyOrder => Buy
        case _: SellOrder => Sell
        case _: BuyReplaceOrder => Buy
        case _: SellReplaceOrder => Sell
      }
    }
  }

  implicit object EventOfTicket extends EventOf[Ticket] {
    def event(obj: Ticket) = {
      val ev = implicitly[EventOf[Order]]
      ev.event(obj.order)
    }
  }

  implicit object EventOfOrderResult extends EventOf[OrderResult] {

    def event(obj: OrderResult) = {
      obj match {
        case _: BuyOrderResult => Buy
        case _: SellOrderResult => Sell
      }
    }

  }

  implicit object EventOfTunnelTicket extends EventOf[TunnelTicketStatus] {

    def event(obj: TunnelTicketStatus) = {
      val ev = implicitly[EventOf[Ticket]]
      obj match {
        case Killed => Neutral
        case w: Wait => ev.event(w.ticket)
        case r: Ready => ev.event(r.ticket)
      }
    }

  }

}