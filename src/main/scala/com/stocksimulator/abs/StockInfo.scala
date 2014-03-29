package com.stocksimulator.abs
import com.github.nscala_time.time.Imports._
import scala.math.Ordered
import io.jvm.uuid._


case class Stock(name: String)

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

class StockInfoHA (stockInfo: StockInfo) extends Ordered[StockInfoHA] {
  var _hasAppeared = false
  
  def hasAppeared = _hasAppeared
  def setHasAppeared(b: Boolean) = {
    _hasAppeared = b
  }
  def unfold = stockInfo
  
  def compare(that:StockInfoHA) = stockInfo.compare(that.unfold) 
}
abstract class StockInfo(_stock: Stock, _datetime: DateTime) extends Ordered[StockInfo] {
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

