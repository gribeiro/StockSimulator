package com.stocksimulator.common_strategies
import com.stocksimulator.abs._
import com.stocksimulator.abs.RunningContextModule._

abstract class QuoteOnlyStrategy(market: Market, param: Parameters)(implicit runningContext: RunningContext) extends Strategy(market, param) {
  def onBuyReport(stock: Stock, volume: Int, price: Double) = {}
  def onSellReport(stock: Stock, volume: Int, price: Double) = {}
  def callback() = {}
  def onStart() = {}
  def onStop() = {}
}