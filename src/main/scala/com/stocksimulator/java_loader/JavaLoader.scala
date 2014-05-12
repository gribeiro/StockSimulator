package com.stocksimulator.java_loader

import com.stocksimulator.main._
import com.stocksimulator.abs._
import com.stocksimulator.common_strategies._
import com.stocksimulator.helpers.ParamMaker
import com.stocksimulator.debug._, LogNames._


abstract class JavaAdapterQ extends JavaAdapter {
  
  
  def callback() = {}
  def onSellReport(stock: Stock, volume: Int, price:Double) = {}
  def onBuyReport(stock: Stock, volume: Int, price:Double) = {}
  
  def onStart = {
    this.log(this.getClass()+" is on!!")
  }
  
}




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
  
  implicit def ParamMaker2Int(pm: ParamMaker): Int = strategy.getIntParam(pm.name)
  implicit def ParamMaker2Double(pm: ParamMaker): Double = strategy.getParam(pm.name).asInstanceOf[Double]
  implicit def ParamMaker2String(pm: ParamMaker): String = strategy.getParam(pm.name).asInstanceOf[String]
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


