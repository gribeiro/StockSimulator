package com.stocksimulator.reuters

import scala.collection.mutable.HashMap
import com.stocksimulator.abs.Stock
import scala.collection.mutable.ArrayBuffer

class CancelDetector {
  type AllSeries = HashMap[(Stock, Double), ArrayBuffer[Int]]
  private val allQuoteSeries = new HashMap[(Stock, Double), ArrayBuffer[Int]]
  private val allTradeSeries = new HashMap[(Stock, Double), ArrayBuffer[Int]]

  private val allSwitch = new HashMap[(Stock, Double), Boolean]

  private def seriesGet(allSeries: AllSeries, query: (Stock, Double)) = {
    allSeries.get(query) match {
      case Some(qs) => qs
      case None =>
        val newS = new ArrayBuffer[Int]
        allSeries(query) = newS
        newS
    }
  }
  private def seriesGet(allSeries: AllSeries, st: Stock, pr: Double): ArrayBuffer[Int] = seriesGet(allSeries, (st, pr))
  private def getQuoteSeries(st: Stock, pr: Double) = seriesGet(allQuoteSeries, st, pr)
  private def getTradeSeries(st: Stock, pr: Double) = seriesGet(allTradeSeries, st, pr)

  def quoteTouch(st: Stock, pr: Double, vol: Int): Unit = {
    val currentQuoteSeries = getQuoteSeries(st, pr)
    currentQuoteSeries += vol
    allSwitch += (st, pr) -> true
  }

  def tradeTouch(st: Stock, pr: Double, vol: Int): Unit = {
    val currentTradeSeries = getTradeSeries(st, pr)
    currentTradeSeries += vol

  }

  def check(st: Stock, pr: Double) = {
    val stPR = (st, pr)
    if (allSwitch.getOrElse(stPR, false)) {
      allSwitch(stPR) = false
      val currentQuoteSeries = getQuoteSeries(st, pr)
      val currentTradeSeries = getTradeSeries(st, pr)
      val zipped = (currentQuoteSeries zip (currentQuoteSeries.drop(1)))
      val quotePositiveDiff = zipped map {
        case (a, b) => a - b
      }
      val summedQuote = quotePositiveDiff.sum
      val summedTrades = currentTradeSeries.sum
      if (summedQuote > summedTrades) {
        //allQuoteSeries -= ((st, pr))
        //allTradeSeries -= ((st, pr))
        val cancels = summedQuote - summedTrades
        tradeTouch(st, pr, cancels)
        cancels
      } else -1
    } else -1
  }

}