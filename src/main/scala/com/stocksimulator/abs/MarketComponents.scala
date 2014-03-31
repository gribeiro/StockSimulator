package com.stocksimulator.abs
import scala.collection.mutable.ListBuffer
import scala.collection.mutable.HashMap
import scala.collection.mutable.LinkedHashMap

abstract class MarketComponent
case class MarketComponentFilter(onTrade: (Trade, Stock)=>Unit, onQuote: (Quote, Stock) => Unit, filter: () => Boolean) extends MarketComponent
case class BookOrderComponent(buyOrder: (LinkedHashMap[Ticket, Book], Ticket) => LinkedHashMap[Ticket, Book], sellOrder: (LinkedHashMap[Ticket, Book], Ticket) => LinkedHashMap[Ticket, Book]) extends MarketComponent