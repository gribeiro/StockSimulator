package com.stocksimulator.reuters
import com.stocksimulator.abs._
import org.joda.time.DateTime



abstract class FeedFromMemory extends Feed {
  protected val memory:Array[Map[Stock, StockInfo]]

  def apply(idx: Int) =  memory(idx)
  def iterator = memory.iterator
  def length = memory.length
}

class ConstructFeed(protected val memory: Array[Map[Stock, StockInfo]], val instruments: Set[Stock]) extends FeedFromMemory {

  
  val allPrices = memory.flatten.groupBy(_._1).map {
    f => 
      
      val b = f._2.map {
      g => g._2 match {
         case q: Quote => (q.bid.price + q.ask.price)*1/2
         case t: Trade => t.priceVol.price
       }
    }
    val max = b.max
    val min = b.min
    (f._1, max, min)
  }
  //val max = allPrices.map(a => ))
  //val min = allPrices.minBy(_._2)
}

class FileFeed(filename: String, val instruments: Set[Stock]) extends FeedFromMemory {
  protected val memory = FileManager(filename)
}

class FileFeedTimeFilter(ff: FileFeed) extends TimeFilter[Feed] {
  
 def timeFiltering(from: DateTime, to: DateTime) = {
   val instruments = ff.instruments
   val memory = ff.filter {
     mapStockStockInfo =>
       mapStockStockInfo.values.map {
         stockInfo =>
           val currentMillis = stockInfo.iDatetime.getMillis()
           currentMillis >= from.getMillis() && currentMillis <= to.getMillis()
       }.reduce(_ && _)
   }
   new ConstructFeed(memory.toArray, instruments)
 }
}