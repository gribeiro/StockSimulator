package com.stocksimulator.abs

object Feed {
  implicit def feed2Boolean(f: Feed): Boolean = f()
}

trait Feed {
	val instruments: Set[Stock]
	def next(): Map[Stock, StockInfo]
	def hasNext(): Boolean	
	def apply():Boolean = hasNext()
	
	def unary_! = next()
}

trait CloneFeed extends Feed {
  def cloneContent(): Array[Map[Stock, StockInfo]]
}

class FeedFromClone(clone: CloneFeed) extends Feed {
  private val iterator = clone.cloneContent().iterator
  
  val instruments = clone.instruments
  def next() = iterator.next()
  def hasNext() = iterator.hasNext
  
}