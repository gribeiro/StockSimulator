package com.stocksimulator.reuters
import com.stocksimulator.abs._
import scala.collection.mutable.HashMap
import org.joda.time.format.DateTimeFormat
import scala.collection.mutable.LinkedHashMap
import com.stocksimulator.debug.Log
import com.stocksimulator.helpers.ImplicitClasses._

object ReutersCommon {
  val dateFormat = DateTimeFormat.forPattern("dd/MM/yyyy HH:mm:ss.SSS")
  val microDateFormat = DateTimeFormat.forPattern("dd/MM/yyyy")
}

object ReutersMarketComponents {

  //componente para facilitar controle temporal da estrategia
  def counterComponent(s: Stock) = {
    var realQuote = 0;

    val onQuote = (q: Quote, stock: Stock) => if (realQuote < 0 && stock == s) realQuote += 1
    val onTrade = (t: Trade, stock: Stock) => if (stock == s) realQuote -= 1
    val filter = () => realQuote == 0
    MarketComponentFilter(onTrade, onQuote, filter)
  }

  def multiCounterComponent(stockSet: Set[Stock]) = {
    val multiRealQuote = new LinkedHashMap[Stock, Int]
    for (aStock <- stockSet) multiRealQuote(aStock) = 0
    val onQuote = (q: Quote, stock: Stock) =>
      if (multiRealQuote.keySet has stock) {
        if (multiRealQuote(stock) < 0 && (stockSet has stock)) multiRealQuote(stock) += 1
      }
    val onTrade = (t: Trade, stock: Stock) =>
      if (multiRealQuote.keySet has stock) {
        if (stockSet has stock) multiRealQuote(stock) -= 1
      }
    val filter = () => multiRealQuote.values.exists(p => p == 0)
    MarketComponentFilter(onTrade, onQuote, filter)
  }

  def standardBookOrder(param: Int) = {
    val buyOrder = (buyBook: LinkedHashMap[Ticket, Book], actionTicket: Ticket) => {
      val tempTicketBookBuyFirst = new LinkedHashMap[Ticket, Book]
      if (!buyBook.contains(actionTicket)) tempTicketBookBuyFirst += actionTicket -> new Book(param)

      tempTicketBookBuyFirst
    }

    val sellOrder = (sellBook: LinkedHashMap[Ticket, Book], actionTicket: Ticket) => {
      val tempTicketBookSellFirst = new LinkedHashMap[Ticket, Book]
      if (!sellBook.contains(actionTicket)) tempTicketBookSellFirst += actionTicket -> new Book(param)
      tempTicketBookSellFirst
    }

    BookOrderComponent(buyOrder, sellOrder)
  }
}
