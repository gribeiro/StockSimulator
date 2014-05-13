package com.stocksimulator.abs
import scala.math.Ordered
import io.jvm.uuid._
import com.stocksimulator.reuters.ReutersCommon
import org.joda.time.YearMonthDay
import org.joda.time.DateTime
import org.joda.time.Period
import org.joda.time.Days

case class OptionStockInfo(val strike: Int, val vencimento: Int, val mes: Int, val r: Double, val ratio: Double) extends PromotionInfo
class OptionStock(stock: Stock) extends Promotion[Stock](stock) {
  val preStock: String = stock
  var vencimento: Option[Int] = None
  var ratio: Option[Double] = None
  val dollarPos = preStock.indexOf("$")
  val strike: Option[Int] = if (dollarPos != -1) Some(preStock.substring(dollarPos + 1, preStock.length()).toInt) else None

  def getSymbol(stdStrike: Int, dueMonth: Int) = {
    val letter = OptionInstrument.reverseLetterTable(dueMonth)
    preStock.substring(0, dollarPos) + letter + stdStrike.toString + ".SA"
  }

  def getSymbol(stdStrike: Int, date: DateTime): String = {
    val year = date.year().get()
    val month = date.monthOfYear().get()
    val day = date.dayOfMonth().get()

    vencimento = Some(OptionInstrument.vencimento(year, month))
   
    val proxVenc =  if(month < 12) new YearMonthDay(year,month+1, OptionInstrument.vencimento(year, month+1)) else new YearMonthDay(year+1,1, OptionInstrument.vencimento(year+1, 1))
    val vencAnt = if(month > 1) new YearMonthDay(year, month-1, OptionInstrument.vencimento(year, month-1)) else new YearMonthDay(year-1, 12, OptionInstrument.vencimento(year-1, 12))
    val falta = Days.daysBetween(date, proxVenc.toDateTimeAtMidnight()).getDays()
    val total =  Days.daysBetween(vencAnt.toDateTimeAtMidnight(), proxVenc.toDateTimeAtMidnight()).getDays()
    ratio = Some(falta.toDouble / 365)
    val vencimentoAtual = if (day <= vencimento.get) month else month - 1
    getSymbol(stdStrike, vencimentoAtual)
  }
  var date: Option[DateTime] = None

  def setDate(dat: DateTime) = {
    date = Some(dat)
    this
  }

  override def demote: Stock = {
    val demoteToStock = (date, strike) match {
      case (Some(dt), Some(str)) =>
        val res = Stock(getSymbol(str, dt))
        val vencimentoGetted = vencimento match {
          case Some(venc) => venc
          case None => throw new Exception("Vencimento should be available")
        }
        val promotion = OptionStockInfo(strike.get, vencimento.get, dt.monthOfYear().get(), 0.11, ratio.get)
        
        res.addPromotionInfo(promotion)
        for(pinfos <- stock.promotions) res.addPromotionInfo(pinfos)
        res
      case (_, _) => stock
    }

    demoteToStock
  }
}

case class Stock(name: String) extends Promotable[Stock] {
  def checkOption(date: String): Stock = {
    val formater = ReutersCommon.microDateFormat
    val dateObj = formater.parseDateTime(date)
    this.promoteTo[OptionStock].setDate(dateObj).demote
  }
  
  lazy val isOption = optionInfo match {
    case Some(_) =>  true
    case _ => false
  }
  def optionInfo: Option[OptionStockInfo] = _promotions.collectFirst {
    case promoI: OptionStockInfo => promoI
  	}
  }

case object Stock {
  implicit def string2stock(s: String): Stock = Stock(s)
  implicit def stock2string(s: Stock): String = s.name
}
case class PriceVol(price: Double, vol: Int) {
  def getPair(): (Double, Int) = (price, vol)
}

object StockInfo {
  def apply(sInfo: StockInfo, newDateTime: DateTime): StockInfo = {
    sInfo match {
      case Quote(stock, bid, ask, datetime) => Quote(stock, bid, ask, newDateTime)
      case Trade(stock, priceVol, datetime) => Trade(stock, priceVol, newDateTime)
    }
  }
}

class StockInfoHA(stockInfo: StockInfo) extends Promotion[StockInfo](stockInfo) with Ordered[StockInfoHA] {
  var _hasAppeared = false

  def hasAppeared = _hasAppeared
  def setHasAppeared(b: Boolean) = {
    _hasAppeared = b
  }
  def unfold = demote

  def compare(that: StockInfoHA) = unfold.compare(that.unfold)
}
sealed abstract class StockInfo(_stock: Stock, _datetime: DateTime) extends Promotable[StockInfo] with Ordered[StockInfo] {
  val iStock = _stock
  val iDatetime = _datetime
  val uuid = UUID.random

  def compare(that: StockInfo) = {
    (iDatetime compareTo that.iDatetime) * (-1)
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

