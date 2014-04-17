package com.stocksimulator.reuters
import com.stocksimulator.abs._
import org.joda.time.DateTime



abstract class FeedFromMemory extends Feed {
  protected val memory:Array[Map[Stock, StockInfo]]

  def apply(idx: Int) =  memory(idx)
  def iterator = memory.iterator
  def length = memory.length
}

class ConstructFeed(protected val memory: Array[Map[Stock, StockInfo]], val instruments: Set[Stock]) extends FeedFromMemory

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