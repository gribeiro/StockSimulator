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
