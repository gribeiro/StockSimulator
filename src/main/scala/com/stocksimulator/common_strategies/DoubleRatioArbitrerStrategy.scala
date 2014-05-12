package com.stocksimulator.common_strategies
import com.stocksimulator.abs._
import com.stocksimulator.debug.Log
import com.stocksimulator.helpers.PMaker._
import com.stocksimulator.helpers._
import com.stocksimulator.java_loader.JavaAdapterQ

abstract class DoubleRatioArbitrerStrategy extends JavaAdapterQ {
  val symbolA: Stock
  val symbolB: Stock
  
  val symbolC: Stock
  

  lazy val roundUp = StrategyUtils.roundUpFactory(gran)
  lazy val roundDown = StrategyUtils.roundDownFactory(gran)
  
  
  lazy val gran: Int = p"gran"
  lazy val maxPos: Int = p"maxPos"
  lazy val step: Int = p"step"
  lazy val elapsed: Int = p"elapsed"
  lazy val spread: Double = p"spread"  

  lazy val mvAvg = strategy.createRatioFor2SymbolsMAvg(symbolA, symbolB, symbolC, elapsed / 1000, elapsed);


  def onQuotes() = {

    val pos = strategy.getPosition(symbolA).quantity
    if (mvAvg.isAvailable) {

      val infoPair = (strategy.getSymbol(symbolA), strategy.getSymbol(symbolB), strategy.getSymbol(symbolC))
      infoPair match {
        case (a: Quote, b: Quote, c: Quote) =>
          val midPr = strategy.midPrice(b)*strategy.midPrice(c)
          val precoTeoricoA = mvAvg.lastVal * midPr
    
          val spreadVenda = spread
          val spreadCompra = spread

          val buyPrice = roundDown(Math.min(a.bid.price, precoTeoricoA - spreadCompra))
          val sellPrice = roundUp(Math.max(a.ask.price, precoTeoricoA + spreadVenda));
          if (pos < maxPos && buyPrice > 0) {
            strategy.provideBuyLiquidity(symbolA, step, buyPrice)
          }

          if (pos > -maxPos && sellPrice > 0) {
            strategy.provideSellLiquidity(symbolA, step, sellPrice)
          }

        case _ => {}
      }
    }
  }

}