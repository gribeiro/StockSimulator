package com.stocksimulator.abs
import com.github.nscala_time.time.Imports._
import scala.math.Ordered
import io.jvm.uuid._
import com.stocksimulator.reuters.ReutersCommon



class OptionStock(stock: Stock) extends Promotion[Stock](stock) {
  val preStock: String = stock
  
  
  val dollarPos = preStock.indexOf("$")
  val strike:Option[Int] = if(dollarPos != -1) Some(preStock.substring(dollarPos+1, preStock.length()).toInt) else None

  
  def getSymbol(stdStrike: Int, dueMonth: Int) = {
    val letter = OptionInstrument.reverseLetterTable(dueMonth)
    preStock.substring(0, dollarPos) + letter + stdStrike.toString + ".SA"
  }
  
  def getSymbol(stdStrike: Int, date: DateTime): String = {
    val year = date.year().get()
    val month = date.monthOfYear().get()
    val day = date.dayOfMonth().get()
    
    val vencimento = OptionInstrument.vencimento(year, month)
    
    val vencimentoAtual = if(day <= vencimento) month else month - 1
    getSymbol(stdStrike, vencimentoAtual)
  }
  var date: Option[DateTime] = None
  
  def setDate(dat: DateTime) = {
    date = Some(dat)
    this
  }
  
  override def demote:Stock = {
    val demoteToStock = (date, strike) match {
      case (Some(dt), Some(str)) =>
        Stock(getSymbol(str, dt))
      case (_, _) => stock
    }
    demoteToStock.addPromotion(this)
    demoteToStock
  }
} 


case class Stock(name: String) extends Promotable[Stock] {
  def checkOption(date: String): Stock = {
    val formater = ReutersCommon.microDateFormat
    val dateObj = formater.parseDateTime(date)
    this.promoteTo[OptionStock].setDate(dateObj).demote
  }
}

case object Stock {
  implicit def string2stock(s: String):Stock = Stock(s)
  implicit def stock2string(s: Stock):String = s.name
}
case class PriceVol(price: Double, vol: Int)  {
  def getPair(): (Double, Int) = (price, vol)
}

object StockInfo {
  def apply(sInfo: StockInfo, newDateTime: DateTime):StockInfo = {
    sInfo match {
      case Quote(stock, bid, ask, datetime) => Quote(stock, bid, ask, newDateTime)
      case Trade(stock, priceVol, datetime) => Trade(stock, priceVol, newDateTime)
    }
  }
}

class StockInfoHA (stockInfo:StockInfo) extends Promotion[StockInfo](stockInfo) with Ordered[StockInfoHA] {
  var _hasAppeared = false
  
  def hasAppeared = _hasAppeared
  def setHasAppeared(b: Boolean) = {
    _hasAppeared = b
  }
  def unfold = demote
  
  def compare(that:StockInfoHA) = unfold.compare(that.unfold) 
}
abstract class StockInfo(_stock: Stock, _datetime: DateTime) extends Promotable[StockInfo] with Ordered[StockInfo]  {
  val iStock = _stock
  val iDatetime = _datetime
  val uuid = UUID.random
  
  def compare(that:StockInfo) = {
    (iDatetime compare that.iDatetime) * (-1)
  }
}
case class Quote(stock: Stock, bid: PriceVol, ask: PriceVol, datetime: DateTime) extends StockInfo(stock, datetime)
case class Trade(stock: Stock, priceVol: PriceVol, datetime: DateTime) extends StockInfo(stock, datetime)
case class EmptyInfo(stock: Stock, datetime: DateTime) extends StockInfo(stock, datetime)

object EmptyInfo {
  def apply(st: Stock) = {
    new EmptyInfo(st, DateTime.now)
  }
}

