package com.stocksimulator.common_strategies

import com.stocksimulator.abs._
import com.stocksimulator.debug.Log
import com.stocksimulator.debug.LogNames._
abstract class OptionSimpleStrategy extends RatioArbitrerStrategy {
  
  //val dolSymbol: Stock = "INDc1"
  val brSymbol: Stock

  val roundUpHundred = StrategyUtils.roundUpFactory(100)
  val roundDownHundred = StrategyUtils.roundDownFactory(100)

  lazy val volDelta = strat.createOptionMAvgs(symbolA, symbolB, elapsed/1000, elapsed, transform)
  lazy val mvAvg = strat.createRatioMAvgForOption(symbolA, symbolB, elapsed/1000, elapsed, volDelta._1, transform)
//  lazy val mvAvgRef = strat.createRatioFor2SymbolsMAvg(brSymbol, symbolB, dolSymbol, elapsed / 1000, elapsed) 
  
 // 
  override def onStart() = {
    this.log("Checking option")
    if(!symbolB.isOption) throw new Exception("Stock should be a option in this field")
  }
 
  
  def priceTransform(br: Double, adr: Double):(Double) => (Double) = {
    br/(adr) * (_:Double)
  } 
  
  def midPrice(s: Stock) = strat.midPrice(strat.getSymbol(s))
  
  def brMid = midPrice(brSymbol) 
  def adrMid = midPrice(symbolA)
  //def dolMid = midPrice(dolSymbol)
  
  def identity(x: Double) = x
  lazy val transform = if(brSymbol == symbolA) identity _ else priceTransform(brMid, adrMid)
  var price = 0
  override def onQuotes() = {

    val pos = strat.getPosition(symbolA).quantity + strat.getPosition(symbolB).quantity
  
      val infoPair = (strat.getSymbol(symbolA), strat.getSymbol(symbolB), strat.getSymbol(brSymbol))
      
      infoPair match {
  
       case (a: Quote, b: Quote, br: Quote) =>
         if (mvAvg.isAvailable && volDelta._1.isAvailable && mvAvg.lastVal > 0) {
         val midPr = strat.midPrice(a)
          val precoTeorico = mvAvg.lastVal
          
          val delta = volDelta._2.lastVal()
          val mVol = volDelta._1.lastVal()
          val buyPrice = precoTeorico - 0.01
          val sellPrice = precoTeorico + 0.01
          val ratio = precoTeorico/strat.midPrice(b)
          val aBuyPr = roundDown(strat.midPrice(a)*ratio - spread)
          val aSellPr = roundUp(strat.midPrice(a)*ratio + spread)
          val optionStep = roundDownHundred(step/delta).intValue()
          val posA = strat.getPosition(brSymbol).quantity
          val posB = strat.getPosition(symbolB).quantity
          
          val tot = posA.abs + posB.abs
          val mid = midPrice(symbolB)
         // this.log(s"$precoTeorico :: $mid")
          //this.log(brSymbol)
          if(posA.abs < maxPos && posB.abs < maxPos) { 
          if (b.ask.price < roundUp(precoTeorico)) { 
           strat.provideBuyLiquidity(symbolB, optionStep, b.bid.price)
          // strat.provideSellLiquidity(symbolB, optionStep, roundUp(precoTeorico))
            
            strat.provideSellLiquidity(brSymbol, step, br.ask.price)
          }
 
         
           if (b.bid.price > roundDown(precoTeorico)) {
             
            strat.provideSellLiquidity(symbolB, optionStep, b.ask.price)
           // strat.provideBuyLiquidity(symbolB, optionStep, roundDown(precoTeorico))
            
            strat.provideBuyLiquidity(brSymbol, step, br.bid.price)
           }
          }
         }
        case _ => {}
      
    }
  }

}