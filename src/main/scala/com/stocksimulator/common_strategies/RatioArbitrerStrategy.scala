package com.stocksimulator.common_strategies
import com.stocksimulator.abs._
import com.stocksimulator.debug.Log
import com.stocksimulator.helpers.PMaker._
import com.stocksimulator.helpers._
import com.stocksimulator.java_loader.JavaAdapterQ
import com.stocksimulator.debug._, LogNames._

abstract class RatioArbitrerStrategy extends JavaAdapterQ {
  val symbolA: Stock
  val symbolB: Stock

  lazy val strat = strategy
  lazy val roundUp = StrategyUtils.roundUpFactory(gran)
  lazy val roundDown = StrategyUtils.roundDownFactory(gran)

  lazy val elapsed_param: Int = strat.getIntParam("elapsed")
  lazy val elapsed = elapsed_param *1000
  lazy val spread: Double = strat.getDoubleParam("spread")
  lazy val step: Int = strat.getIntParam("step")
  lazy val maxPos: Int = strat.getIntParam("maxPos")
  lazy val gran: Double = strat.getDoubleParam("gran")
  lazy val mvAvg = strat.createRatioMAvg(symbolA, symbolB, elapsed / 1000, elapsed);


  def onQuotes() = {

    val pos = strat.getPosition(symbolA).quantity
    if (mvAvg.isAvailable && mvAvg.lastVal > 0) {
      val infoPair = (strat.getSymbol(symbolA), strat.getSymbol(symbolB))
      infoPair match {
        case (a: Quote, b: Quote) =>
          val midPr = strat.midPrice(b)
          
          val precoTeoricoA = mvAvg.lastVal * midPr
          //this.log(s"Preco teorico: $precoTeoricoA")
          val buyPrice = roundDown(Math.min(a.bid.price, precoTeoricoA - spread))
          val sellPrice = roundUp(Math.max(a.ask.price, precoTeoricoA + spread))
          val midprA = a.bid.price
         // this.log(s"Mid Price: $midprA ")
          if (pos < maxPos && buyPrice > 0) {
           // this.log(s"Buyprice: $buyPrice")
            strat.provideBuyLiquidity(symbolA, step, buyPrice)
          }

          if (pos > -maxPos && sellPrice > 0) {
            strat.provideSellLiquidity(symbolA, step, sellPrice)
          }

        case _ => {}
      }
    }
  }

}








