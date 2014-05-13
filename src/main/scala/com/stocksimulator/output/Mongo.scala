package com.stocksimulator.output

import com.stocksimulator.abs.Parameters
import com.mongodb.casbah.Imports._
import com.stocksimulator.abs.OrderResult
import com.stocksimulator.abs.BuyOrderResult
import com.stocksimulator.abs.SellOrderResult
import com.stocksimulator.abs.EmptyOrderResult
import com.stocksimulator.abs.Utils
import com.stocksimulator.debug._
import com.stocksimulator.debug.LogNames._
import org.joda.time.DateTime
import com.stocksimulator.abs.Stock
import com.stocksimulator.abs.StockInfo
import com.stocksimulator.abs.Quote
import scala.collection.mutable.HashMap
import com.stocksimulator.abs.Position
import com.stocksimulator.abs.Trade
import com.stocksimulator.main._
import com.stocksimulator.abs.BuyOrderResult
import com.stocksimulator.abs.SellOrderResult
object PNL {
  val tabela = HashMap.empty[Stock, Double].withDefaultValue(1.0)
  tabela("WINc1") = 0.2
  tabela("WDOc1") = 10
  tabela("DOLc1") = 50
  def apply(param: Parameters) = {

    if (param.size > 0) {
      val unwrapped = param.unwrap
      val position = unwrapped("position").asInstanceOf[HashMap[Stock, Position]]
      val marketLast = unwrapped("marketLast").asInstanceOf[Map[Stock, StockInfo]]
      this.log(position)
      val nInfo = for ((lastStock, info) <- marketLast; (posStock, pos) <- position; if (posStock == lastStock)) yield {

        val price = info match {
          case q: Quote =>  if (pos.quantity >= 0) q.bid.price else q.ask.price
          case t: Trade => t.priceVol.price
        }

        (pos.pnl + price * pos.quantity.toDouble) * tabela(info.iStock)

      }
      this.log(nInfo)
      nInfo.sum
    } else 0

  }
}
abstract class ExtraInfoGenerator[@specialized(Int, Double, Float)  T] (implicit num: Numeric[T]){

  protected def pnl(out: Parameters) = PNL(out)
  def apply(in: Parameters, out: Parameters): T

  def mean(xs: Iterable[T]): Double = xs match {
    case Nil => 0.0
    case ys => num.toDouble(ys.reduceLeft(num.plus(_, _))) / ys.size.toDouble
    //ys.reduceLeft(num.plus(_,_)) / ys.size.toDouble
  }

  def stddev(xs: Iterable[T], avg: Option[Double] = None): Double = xs match {
    case Nil => 0.0
    case ys =>
      val usableAvg = avg.getOrElse(mean(xs))
      math.sqrt((0.0 /: ys) {
        (a, e) => a + math.pow(num.toDouble(e) - usableAvg, 2.0)
      } / xs.size)
  }

  def preReturn(in: Parameters, out: Parameters) = {
    val inContent = in.unwrap.toMap
    val outContent = out.unwrap.toMap
    val initialCash = 1000000.0
    val list = for ((oStr, oVal) <- outContent) yield {
      oVal match {
        case rep: OrderResult => rep
        case _ => EmptyOrderResult
      }

    }
    val sorted = list.toList.filter(p => p.value != 0).sortBy(f => f.dateTime.getMillis())

    var pos: Int = 0
    var cash: Double = initialCash

    val posCashPnl = sorted.map {
      orderInfo =>
        orderInfo match {
          case buy: BuyOrderResult =>
            pos += buy.quantity
            cash -= buy.quantity * buy.value
            (pos, cash, cash + buy.value * pos)
          case sell: SellOrderResult =>
            pos -= sell.quantity
            cash += sell.quantity * sell.value
            (pos, cash, cash + sell.value * pos)
        }
    }

    posCashPnl.zip(posCashPnl.drop(1)).map {
      case ((pos1, cash1, pnl1), (pos2, cash2, pnl2)) =>
        if (pnl1 != 0) (pnl2 - pnl1) / pnl1 else 0.0
    }
  }
}

object SortinoGenerator extends ExtraInfoGenerator[Double] {
  def apply(in: Parameters, out: Parameters): Double = {

    val _preReturn = preReturn(in, out)
    val negativeReturns = _preReturn.filter(r => r < 0)
    val stdDevNegativeReturn = stddev(negativeReturns)
    _preReturn.sum / stdDevNegativeReturn
  }
}

object SharpeGenerator extends ExtraInfoGenerator[Double] {
  def apply(in: Parameters, out: Parameters): Double = {
    val _preReturn = preReturn(in, out)
    val stdReturn = stddev(_preReturn)
    _preReturn.sum / stdReturn
  }
}

class ExtraInfoFarm[T](in: Parameters, out: Parameters) {
  val mem = new HashMap[String, ExtraInfoGenerator[T]]

  def register(name: String, gen: ExtraInfoGenerator[T]) = {
    mem += name -> gen
  }

  def get() = {
    (for ((name, generator) <- mem) yield name -> generator(in, out)).to[List]
  }
}

class MongoOutput(in: Parameters, out: Parameters, id: String, sId: String) {
  private val inContent = in.unwrap.toMap
  private val outContent = if (out == null) Map.empty[String, Object] else out.unwrap.toMap

  private val pnl2 = if(outContent.size > 0) PNL(out) else 0.0
  private val inputHash = Utils.md5Hash(inContent.toString)
  private val infoFarm = new ExtraInfoFarm[Double](in, out)

  infoFarm.register("sortino", SortinoGenerator)
  infoFarm.register("sharpe", SharpeGenerator)
  // Log(outContent)
  var date = outContent("date").asInstanceOf[String]
  val mongoOBJs = {
    val list2 = for ((oStr, oVal) <- outContent) yield {
      oVal match {
        case rep: OrderResult =>
          if(date == null) date = (List(rep.dateTime.dayOfMonth().get(), rep.dateTime.monthOfYear().get(), rep.dateTime.year().get()).mkString("/"))
          Map("Order" -> rep.iType, "DateTimeLong" -> rep.dateTime.getMillis(),"DateTime" -> rep.dateTime.toString(), "Quantity" -> rep.quantity, "Value" -> rep.value)

        case _ => Map("Order" -> "Empty", "DateTimeLong" -> 0L)
      }

    }
    val sortedList2 = list2.toSeq.sortBy {
      mongoMap => mongoMap("DateTimeLong").asInstanceOf[Long]
    }
    val list = sortedList2.map(mm => MongoDBObject(mm.toList))
    val md5 = outContent.get("md5").orElse(Some("N/A")).get.asInstanceOf[String]

    val inputStr = in.inputStr
    //Log(RBSFactory.mongoOutputSymbol)

    val position = outContent.get("position").get.asInstanceOf[HashMap[Stock, Position]]

    
val listd = list.distinct
    val results = List("Orders" -> listd, "sID" -> sId, "simID" -> id, "Input" -> inContent, "PNL" -> pnl2, "md5" -> md5, "inputHash" -> inputHash, "inputStr" -> (inputStr), "date" -> date) ++ infoFarm.get()
    MongoDBObject(results)
  }

  def output = mongoOBJs

}