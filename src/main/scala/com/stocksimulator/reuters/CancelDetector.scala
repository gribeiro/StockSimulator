package com.stocksimulator.reuters

import scala.collection.mutable.HashMap
import com.stocksimulator.abs.Stock
import scala.collection.mutable.ArrayBuffer
import com.stocksimulator.debug.Log

case class StockDoubleKey(stock: Stock, price: Double)

class CancelDetector {
  type AllSeries = HashMap[StockDoubleKey, ArrayBuffer[Int]]
  private val allQuoteSeries = new HashMap[StockDoubleKey, ArrayBuffer[Int]]
  private val allTradeSeries = new HashMap[StockDoubleKey, ArrayBuffer[Int]]

  private val allSwitch = new HashMap[StockDoubleKey, Boolean]

  private def seriesGet(allSeries: AllSeries, query: StockDoubleKey) = {
    allSeries.get(query) match {
      case Some(qs) => qs
      case None =>
        val newS = new ArrayBuffer[Int]
        allSeries(query) = newS
        newS
    }
  }
  private def seriesGet(allSeries: AllSeries, st: Stock, pr: Double): ArrayBuffer[Int] = seriesGet(allSeries, StockDoubleKey(st, pr))
  private def getQuoteSeries(st: Stock, pr: Double) = seriesGet(allQuoteSeries, st, pr)
  private def getTradeSeries(st: Stock, pr: Double) = seriesGet(allTradeSeries, st, pr)

  def quoteTouch(st: Stock, pr: Double, vol: Int): Unit = {
    val currentQuoteSeries = getQuoteSeries(st, pr)
    if(currentQuoteSeries.size >= 1) {
    	val head = currentQuoteSeries.head
    	currentQuoteSeries.clear
    	currentQuoteSeries += head
    }
    currentQuoteSeries += vol
    allSwitch += StockDoubleKey(st, pr) -> true
  }

  def tradeTouch(st: Stock, pr: Double, vol: Int): Unit = {
    val currentTradeSeries = getTradeSeries(st, pr)
    currentTradeSeries += vol

  }

  def check(st: Stock, pr: Double) = {
    val stPR = StockDoubleKey(st, pr)
    if (allSwitch.getOrElse(stPR, false)) {
      allSwitch(stPR) = false
      val currentQuoteSeries = getQuoteSeries(st, pr)
      val currentTradeSeries = getTradeSeries(st, pr)
     /* val zipped = (currentQuoteSeries zip (currentQuoteSeries.drop(1)))
      val quotePositiveDiff = zipped map {
        case (a, b) => a - b
      }*/
      val summedQuote = currentQuoteSeries.head - currentQuoteSeries.last
        // Log(quotePositiveDiff.sum + " and " + summedQuote)
      val summedTrades = currentTradeSeries.sum
     currentTradeSeries.clear 
     currentTradeSeries += summedTrades
      if (summedQuote > summedTrades) {
        allQuoteSeries -= StockDoubleKey(st, pr)
        allTradeSeries -= StockDoubleKey(st, pr)
        val cancels = summedQuote - summedTrades
        tradeTouch(st, pr, cancels)
        cancels
      } else -1
    } else -1
  }

}