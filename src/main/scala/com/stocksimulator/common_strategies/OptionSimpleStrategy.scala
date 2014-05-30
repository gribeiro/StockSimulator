package com.stocksimulator.common_strategies

import com.stocksimulator.abs._
import com.stocksimulator.debug.Log
import com.stocksimulator.debug.LogNames._
abstract class OptionSimpleStrategy extends RatioArbitrerStrategy {
  
  val dolSymbol: Stock
  val brSymbol: Stock

  val roundUpHundred = StrategyUtils.roundUpFactory(100)
  val roundDownHundred = StrategyUtils.roundDownFactory(100)

  lazy val volDelta = strat.createOptionMAvgs(symbolA, symbolB, elapsed/1000, elapsed)
  lazy val mvAvg = strat.createRatioMAvgForOption(symbolA, symbolB, elapsed/1000, elapsed, volDelta._1);
//  lazy val mvAvgRef = strat.createRatioFor2SymbolsMAvg(brSymbol, symbolB, dolSymbol, elapsed / 1000, elapsed) 
  
 // 
  override def onStart() = {
    this.log("Checking option")
    if(!symbolB.isOption) throw new Exception("Stock should be a option in this field")
  }
  
  override def onQuotes() = {

    val pos = strat.getPosition(symbolA).quantity
    //this.log("mvAvg: " + mvAvg.lastVal)
    
//println(elapsed)
    if (mvAvg.isAvailable && mvAvg.lastVal > 0) {
      val infoPair = (strat.getSymbol(symbolA), strat.getSymbol(symbolB))
      infoPair match {
        case (a: Quote, b: Quote) =>
          val midPr = strat.midPrice(a)
          val precoTeorico = mvAvg.lastVal*midPr
          val delta = volDelta._2.lastVal()
          //this.log("precoTeorico:" + precoTeorico)
          val buyPrice = precoTeorico - 0.01
          val sellPrice = precoTeorico + 0.01
          val ratio = precoTeorico/strat.midPrice(b)
          //this.log("buyPrice:" + buyPrice)
          val aBuyPr = roundDown(strat.midPrice(a)*ratio - spread)
          val aSellPr = roundUp(strat.midPrice(a)*ratio + spread)
          //this.log("::"+delta)
          val optionStep = roundUpHundred(step/delta).intValue()
          //println("Real Bid: " + b.bid.price + " Vol BID: " + buyPrice)
          if (b.bid.price < buyPrice ) {
            strat.provideBuyLiquidity(symbolB, optionStep, b.ask.price - spread/2)
            strat.provideSellLiquidity(symbolA, step, a.bid.price + spread)
            
          }
 
           if (b.ask.price > sellPrice) {
            strat.provideSellLiquidity(symbolB, optionStep, b.bid.price + spread/2)
            strat.provideBuyLiquidity(symbolA, step, a.ask.price - spread)
            
          }

        case _ => {}
      }
    }
  }

}