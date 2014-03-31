package com.stocksimulator.common_strategies
import com.stocksimulator.abs._

abstract class QuoteOnlyStrategy(market: Market, param: Parameters) extends Strategy(market, param) {
  def onBuyReport(stock: Stock, volume: Int, price: Double) = {}
  def onSellReport(stock: Stock, volume: Int, price: Double) = {}
  def callback() = {}
  def onStart() = {}
  def onStop() = {}
}