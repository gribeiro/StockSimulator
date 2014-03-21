package com.stocksimulator.reuters

import com.stocksimulator.abs._
import com.github.tototoshi.csv._
import org.joda.time._
import java.io.File
import scala.annotation.tailrec
import scala.collection.Parallel
import scala.collection.parallel._
import scala.concurrent.ExecutionContext.Implicits.global
import org.joda.time.format.DateTimeFormat
import scala.collection.mutable.LinkedHashMap
import com.stocksimulator.debug._

abstract class GeneralInfo(val ric: String, val date: String, val time: String, val gmt: String, val sType: String, val price: String, val volume: String, val bidPrice: String, val bidSize: String, val askPrice: String, val askSize: String)
case class LineInfo(ric: String, date: String, time: String, gmt: String, sType: String, price: String, volume: String, bidPrice: String, bidSize: String, askPrice: String, askSize: String)

trait Filter
case class HourFilter(from: DateTime, to: DateTime) extends Filter
case class ExtendedHourFilter(include: HourFilter, exclude: Array[HourFilter]) extends Filter
case object EmptyFilter extends Filter

class ReutersCsvFeed(filename: String, knownInstruments: Set[Stock] = Set()) extends Feed {
  lazy val csvReader = CSVReader.open(new File(filename))
  lazy val rawInfo = getMeRaw()

  def getMeRaw() = (csvReader.toStream()).drop(1)
  lazy val lazyLineInfo = (rawInfo.map(makeLineInfo))
  val dateFormat = ReutersCommon.dateFormat
  val instruments = if (knownInstruments.size > 0) knownInstruments else readInstruments

  val instrument = (for { x <- instruments } yield x -> lazyLineInfo.filter(filters(x)))

  val iterators = for ((stock, stream) <- instrument) yield (stock, stream.iterator)

  val lastBidPrice = new LinkedHashMap[Stock, Double]
  val lastBidVol = new LinkedHashMap[Stock, Int]

  val lastAskPrice = new LinkedHashMap[Stock, Double]
  val lastAskVol = new LinkedHashMap[Stock, Int]

  for (i <- instruments) {
    lastBidPrice.put(i, 0)
    lastBidVol.put(i, 0)
    lastAskPrice.put(i, 0)
    lastAskVol.put(i, 0)
  }

  private def filters(x: Stock) = {
    (f: LineInfo) =>
      {
        val filterList = ((f.sType == "Trade" && f.price != "0") || (f.sType == "Quote" && f.askPrice != "0" && f.bidPrice != "0")) :: List(f.ric == x.name)
        val res = filterList.reduceRight(_ && _)
        res
      }
  }
  def hasNext(): Boolean = {
    val qtd = iterators.par.filter {
      case (_, it) => {
        it.hasNext
      }
    }
    qtd.size > 0
  }

  def next(): Map[Stock, StockInfo] = {
    if (hasNext()) nextPerform() else throw new Exception
  }

  private def nextPerform(): Map[Stock, StockInfo] = {
    val nextInfo = iterators.par.filter {
      case (_, it) => {
        it.hasNext
      }
    }.map {
      case (stock, it) => {

        (stock, it.next())
      }
    }

    def makeStockInfo(info: (Stock, LineInfo)): Pair[Stock, Option[StockInfo]] = {
      val raw = info._2
      val stock = Stock(raw.ric)
      val sType = raw.sType
      val dtime = List(raw.date, raw.time) mkString " "
      val datetime = DateTime.parse(dtime, dateFormat)

      if (sType == "Quote") {

        def memoryGetter[T](transf: (String) => T)(s: String, memVector: LinkedHashMap[Stock, T]): T = {
          if (s == "") memVector(stock) else {
            val newVal = transf(s)
            memVector(stock) = newVal
            newVal
          }
        }

        val intGetter = memoryGetter[Int](s => s.toInt) _
        val doubleGetter = memoryGetter[Double](s => s.toDouble) _

        val bidPrice = doubleGetter(raw.bidPrice, lastBidPrice)
        val bidSize = intGetter(raw.bidSize, lastBidVol)
        val bid = PriceVol(bidPrice, bidSize)

        val askPrice = doubleGetter(raw.askPrice, lastAskPrice)
        val askSize = intGetter(raw.askSize, lastAskVol)

        val ask = PriceVol(askPrice, askSize)
        val result = if (bidPrice < askPrice) Some(Quote(stock, bid, ask, datetime)) else {
          Log("BADRESULT: " + Quote(stock, bid, ask, datetime))
          None
          }
        (stock, result)
      } else {
        val price = raw.price.toDouble
        val size = raw.volume.toInt
        val pv = PriceVol(price, size)
       (stock, Some(Trade(stock, pv, datetime)))
      }

    }

    def filterBadResults(result: Pair[Stock, Option[StockInfo]]): Option[StockInfo] = {
      result match {
        case (stock, Some(info)) => Some(info)
        case (stock, None) =>
          val nextInfoForStock = iterators.filter {
            case (st,  it) => {
              it.hasNext && st == stock
            }
          }.map {
            case (stock, it) => {

              (stock, it.next())
            }
          }
         if(nextInfoForStock.size > 0)
        	 filterBadResults(makeStockInfo(nextInfoForStock.head))
         else None
      }
    }
    val res = nextInfo.map(makeStockInfo).map(filterBadResults).filter(f => f != None).map(f => f.get).map(f => Pair(f.iStock, f))
    res.seq.toMap

  }

  def makeLineInfo(list: List[String]) = {
    val vect = list.to[Vector]
    LineInfo(vect(0), vect(1), vect(2), vect(3), vect(4), vect(5), vect(6), vect(7), vect(8), vect(9), vect(10))
  }

  def readInstruments(): Set[Stock] = {
    val first = lazyLineInfo(0).ric

    @tailrec
    def readAllInstruments(s: Stream[LineInfo], stock: Set[String]): Set[Stock] = {

      val ns = s.par.filter(li => !stock(li.ric)).toStream
      val newRic = ns match {
        case x #:: xs => x.ric
        case Stream.Empty => None
      }
      newRic match {
        case None => stock.map(s => Stock(s))
        case x: String => readAllInstruments(ns, Set(x).union(stock))
      }
    }
    Set(Stock(first)).union(readAllInstruments(lazyLineInfo, Set(first)))
  }

}
