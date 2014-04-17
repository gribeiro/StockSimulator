package com.stocksimulator.abs

trait Feed extends Seq[Map[Stock, StockInfo]] {
	val instruments: Set[Stock]
}
