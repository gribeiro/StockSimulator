package com.stocksimulator.abs

object Feed {
  implicit def feed2Boolean(f: Feed): Boolean = f()
}

trait Feed {
	val instruments: Set[Stock]
	def next(): Map[Stock, StockInfo]
	def hasNext(): Boolean	
	def apply():Boolean = hasNext()
	val totalDataSize: Int
	def unary_! = next()
}

trait CloneFeed extends Feed {
  def cloneContent(): Array[Map[Stock, StockInfo]]
}

class FeedFromClone(content: Array[Map[Stock, StockInfo]], val instruments:Set[Stock]) extends Feed {
  private val iterator = content.iterator
  val totalDataSize = content.size
  def next() = iterator.next()
  def hasNext() = iterator.hasNext
  
}