package com.stocksimulator.common_strategies
import javax.script._
import java.io._
import com.stocksimulator.abs._
import com.stocksimulator.debug.Log
import java.security.MessageDigest
import sun.misc.IOUtils

object RubyStrategyLoader {
  def md5SumString(bytes: Array[Byte]): String = {
    val md5 = MessageDigest.getInstance("MD5")
    md5.reset()
    md5.update(bytes)

    md5.digest().map(0xFF & _).map { "%02x".format(_) }.foldLeft("") { _ + _ }
  }
  var filename: String = ""
  var klass: String = ""
  var file: String = ""
  def apply[T, U](strategy: U) = {
    val manager = new ScriptEngineManager
    val engine = manager.getEngineByName("jruby")

    val md5 = if (filename == "self") {
    	//engine.eval(file)
    	md5SumString(file.getBytes())
    } else {
      val filereader = new FileReader(filename)
      engine.eval(filereader)
      md5SumString(scala.io.Source.fromFile(filename).mkString.getBytes())
    }
    engine.put("this", strategy)
    (engine.eval(klass + ".new($this)").asInstanceOf[T], md5)

  }
}

abstract class RubyAdapter(val strategy: RubyStdStrategy) {

  def onQuotes()

  def getSymbol(s: String) = {
    strategy.getSymbol(Stock(s))
  }

  def makeWindow(stock: String, windowSize: Int, elapsed: Int) = {
    strategy.createMAvg(Stock(stock), windowSize, elapsed)
  }
}

abstract class RubyRatioAdapter(strategy: RubyRatioStrategy) {
  def symbolA: String
  def symbolB: String
  def gran: Int
  def maxPos: Int
  def step: Int
}
abstract class RubyDoubleRatioAdapter(strategy: RubyDoubleRatioStrategy) {
  def symbolA: String
  def symbolB: String
  def symbolC: String
  def gran: Int
  def maxPos: Int
  def step: Int
}

abstract class RubyStdStrategy(market: Market, param: Parameters) extends QuoteOnlyStrategy(market, param) {
  val (adapter, hash) = RubyStrategyLoader[RubyAdapter, RubyStdStrategy](this)
  def onQuotes = adapter.onQuotes()
  putResult("md5", hash)
}

class RubyRatioStrategy(market: Market, param: Parameters) extends RatioArbitrerStrategy(market, param) {
  val (adapter, hash) = RubyStrategyLoader[RubyRatioAdapter, RubyRatioStrategy](this)
  val symbolA: Stock = adapter.symbolA
  val symbolB: Stock = adapter.symbolB
  val gran = adapter.gran
  val maxPos = adapter.maxPos
  val step = adapter.step

  putResult("md5", hash)
}

class RubyDoubleRatioStrategy(market: Market, param: Parameters) extends DoubleRatioArbitrerStrategy(market, param) {
  val (adapter, hash) = RubyStrategyLoader[RubyDoubleRatioAdapter, RubyDoubleRatioStrategy](this)
  val symbolA: Stock = adapter.symbolA
  val symbolB: Stock = adapter.symbolB
  val symbolC: Stock = adapter.symbolC
  val gran = adapter.gran
  val maxPos = adapter.maxPos
  val step = adapter.step

  putResult("md5", hash)
}

