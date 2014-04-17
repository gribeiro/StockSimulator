package com.stocksimulator.java_loader

import com.stocksimulator.main._
import com.stocksimulator.abs._
import com.stocksimulator.common_strategies._


abstract class JavaAdapter {
  protected var _strat: JavaStdStrategy = null
  def strategy = _strat
  def onQuotes()
  def onStart()
  def callback()
  def onBuyReport(stock: Stock, volume: Int, price: Double)
  def onSellReport(stock: Stock, volume: Int, price: Double)
  def setStrategy(strat: JavaStdStrategy) = {
    _strat = strat
  }
}


object CreateStrategyForAdapter {
  def apply(filename: String, fs: String) = {
    (market: Market, param: Parameters) => {
      val adapt = MemoryCompiler.apply(filename, fs).asInstanceOf[JavaAdapter]
      new JavaStdStrategy(market, param) {
        val adapter = adapt
      }
    }
  }
}

abstract class JavaStdStrategy(market: Market, param: Parameters) extends QuoteOnlyStrategy(market, param) {
  val adapter: JavaAdapter
  
  def onQuotes = {
    adapter.onQuotes()

  }
  override def callback = adapter.callback()
  override def onStart  = { 

  adapter.setStrategy(this)
    adapter.onStart()
  }
  override def onBuyReport(stock: Stock, volume: Int, price: Double) = {
    adapter.onBuyReport(stock, volume, price)
  }
  override def onSellReport(stock: Stock, volume: Int, price: Double) = {
    adapter.onSellReport(stock, volume, price)
  }
}

trait JavaStrategyTypes  {
  def generic = classOf[JavaStdStrategy]
}


