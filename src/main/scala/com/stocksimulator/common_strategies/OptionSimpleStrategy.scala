package com.stocksimulator.common_strategies

import com.stocksimulator.abs._
import com.stocksimulator.debug.Log
/*
abstract class OptionSimpleStrategy(market: Market, param: Parameters)  extends RatioArbitrerStrategy(market, param) {
  
  
  override lazy val mvAvg = createRatioMAvgForOption(symbolA, symbolB, elapsed / 1000, elapsed);
  
  override def onStart() = {
    
    if(!symbolB.isOption) throw new Exception("Stock should be a option in this field")
  }
  
  override def onQuotes() = {

    val pos = getPosition(symbolA).quantity
    if (mvAvg.isAvailable && mvAvg.lastVal > 0) {
      val infoPair = (getSymbol(symbolA), getSymbol(symbolB))

      infoPair match {
        case (a: Quote, b: Quote) =>
          val precoTeorico = mvAvg.lastVal
          val buyPrice = roundDown(Math.min(b.bid.price, precoTeorico - spread))
          val sellPrice = roundUp(Math.max(b.ask.price, precoTeorico + spread))

          if (pos < maxPos && buyPrice > 0) {
            provideBuyLiquidity(symbolB, step, buyPrice)
          }

          if (pos > -maxPos && sellPrice > 0) {
            provideSellLiquidity(symbolB, step, sellPrice)
          }

        case _ => {}
      }
    }
  }

}*/