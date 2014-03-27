package com.stocksimulator.java_loader

import com.stocksimulator.main._
import com.stocksimulator.abs._
import com.stocksimulator.common_strategies._
import javax.script._

object JavaStrategyLoader {
  var javaOBJ: Object = null
  def apply[T] = {
    javaOBJ.asInstanceOf[T]
  }
}

abstract class JavaAdapter {
  def onQuotes():Unit
  def onStart():Unit
}

class JavaStdStrategy(market: Market, param: Parameters) extends Strategy(market, param) {
  
  val engine = MemoryCompiler.loadAgain(this)
  engine.eval("$adapter = MicroAdapter.new()")
  engine.put("market", market)
  engine.put("param", param)
  //engine.eval("$adapter.setStrategy($strat)")
  println("JavaStdStrategy loaded...")
  def onQuotes = engine.eval("$adapter.onQuotes")
  def onStart = engine.eval("$adapter.onStart")
  

  def callback() = {}
  def onBuyReport(stock: Stock, volume: Int, price: Double) = {}
  def onSellReport(stock: Stock, volume: Int, price: Double) = {}
  def onStop() = {}

}

trait JavaStrategyTypes  {
  def generic = classOf[JavaStdStrategy]
}

class JavaBS[T <: Strategy](rb: JavaBSAdapter, _klass: Class[T]) extends AdapterBSSet[T](rb, _klass) {
  val filename = rb.filename
  JavaStrategyLoader.javaOBJ = rb.javaOBJ

}

abstract class JavaBSAdapter(val javaOBJ: Object, date: String) extends BSAdapter(date) with JavaStrategyTypes {
 def getBS[T <: Strategy] = {
		 new JavaBS(this, generic)
  }
 val filename: String
}
