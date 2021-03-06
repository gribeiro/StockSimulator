package com.stocksimulator.abs
import com.github.nscala_time.time.Imports._


sealed abstract class Order(_dateTime: DateTime, _stock: Stock,_quantity: Int, _value: Double) {
  val dateTime = _dateTime
  val quantity = _quantity
  val value = _value
  val stock = _stock
}

sealed abstract class ReplaceOrder(_ticketToReplace: Ticket, _newOrder:Order) extends Order(_ticketToReplace.order.dateTime, _ticketToReplace.order.stock, _ticketToReplace.order.quantity, _ticketToReplace.order.value) {
  val ticketToReplace = _ticketToReplace
  val newOrder = _newOrder
}

case class BuyOrder(_dateTime: DateTime, _stock: Stock, _quantity: Int, _value: Double) extends Order(_dateTime, _stock, _quantity, _value) 
case class SellOrder(_dateTime: DateTime, _stock: Stock, _quantity: Int, _value: Double) extends Order(_dateTime, _stock, _quantity, _value)

case class BuyReplaceOrder(_ticketToReplace: Ticket, _newOrder:BuyOrder) extends ReplaceOrder(_ticketToReplace, _newOrder) 
case class SellReplaceOrder(_ticketToReplace: Ticket, _newOrder: SellOrder) extends ReplaceOrder(_ticketToReplace, _newOrder) 

abstract class OrderResult(_dateTime: DateTime, _quantity: Int, _value: Double, val iType: String, val instrument: Stock) {
  val dateTime = _dateTime
  val quantity = _quantity
  val value = _value
}
case class BuyOrderResult(_dateTime: DateTime, _quantity: Int, _value: Double, _instrument: Stock) extends OrderResult(_dateTime, _quantity, _value, "Buy", _instrument)
case class SellOrderResult(_dateTime: DateTime, _quantity: Int, _value: Double, _instrument: Stock) extends OrderResult(_dateTime, _quantity, _value, "Sell", _instrument)
case object EmptyOrderResult extends OrderResult(DateTime.now, 0, 0, "Empty", Stock("N/A")) {
  val requestDateTime = DateTime.now
}

abstract class GeneralTicket(val id: Int, val order:Order)
case class Ticket(_id: Int, _order: Order) extends GeneralTicket(_id, _order)
object EmptyTicket extends Ticket(-1, null)