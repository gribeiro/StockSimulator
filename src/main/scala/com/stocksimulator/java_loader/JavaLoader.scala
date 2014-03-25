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

abstract class JavaAdapter(val strategy: Strategy) {
  def onQuotes()
}

abstract class JavaStdStrategy(market: Market, param: Parameters) extends QuoteOnlyStrategy(market, param) {
  val adapter = JavaStrategyLoader[JavaAdapter, JavaStdStrategy]
  def onQuotes = adapter.onQuotes()

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
