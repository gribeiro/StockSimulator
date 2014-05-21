package com.stocksimulator.common_strategies

import com.stocksimulator.abs._
import com.stocksimulator.debug.Log
import com.stocksimulator.debug.LogNames._
abstract class OptionSimpleStrategy extends RatioArbitrerStrategy {
  
  val dolSymbol: Stock
  val brSymbol: Stock
  
  
  lazy val mvAvg = strat.createRatioMAvgForOption(symbolA, symbolB, elapsed / 1000, elapsed);
//  lazy val mvAvgRef = strat.createRatioFor2SymbolsMAvg(brSymbol, symbolB, dolSymbol, elapsed / 1000, elapsed) 
  
  
  override def onStart() = {
    this.log("Checking option")
    if(!symbolB.isOption) throw new Exception("Stock should be a option in this field")
  }
  
  override def onQuotes() = {

    val pos = strat.getPosition(symbolA).quantity
    //this.log("mvAvg: " + mvAvg.lastVal)
    if (mvAvg.isAvailable && mvAvg.lastVal > 0) {
      val infoPair = (strat.getSymbol(symbolA), strat.getSymbol(symbolB))

      infoPair match {
        case (a: Quote, b: Quote) =>
          val midPr = strat.midPrice(a)
          val precoTeorico = mvAvg.lastVal
         // this.log("precoTeorico:" + precoTeorico)
          val buyPrice = roundDown(Math.min(b.bid.price, precoTeorico - spread))
          val sellPrice = roundUp(Math.max(b.ask.price, precoTeorico + spread))

          if (pos < maxPos && buyPrice > 0) {
            strat.provideBuyLiquidity(symbolB, step, buyPrice)
          }

          if (pos > -maxPos && sellPrice > 0) {
            strat.provideSellLiquidity(symbolB, step, sellPrice)
          }

        case _ => {}
      }
    }
  }

}