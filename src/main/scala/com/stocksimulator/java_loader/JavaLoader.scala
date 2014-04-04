package com.stocksimulator.java_loader

import com.stocksimulator.main._
import com.stocksimulator.abs._
import com.stocksimulator.common_strategies._

object JavaStrategyLoader {
  var javaOBJ: Object = null
  def apply[T, U] = {
    javaOBJ.asInstanceOf[T]
  }
}

abstract class JavaAdapter {
  protected var _strat: JavaStdStrategy = null
  def strategy = _strat
  def onQuotes()
  def onStart()
  def callback()
  def setStrategy(strat: JavaStdStrategy) = {
    _strat = strat
  }
}

class JavaStdStrategy(market: Market, param: Parameters) extends QuoteOnlyStrategy(market, param) {
  val adapter = MemoryCompiler.loadAgain.asInstanceOf[JavaAdapter]
  adapter.setStrategy(this)
  def onQuotes = adapter.onQuotes()
  override def callback = adapter.callback()
  override def onStart  = adapter.onStart()
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
