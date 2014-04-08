package com.stocksimulator.common_strategies
import com.stocksimulator.abs._
import com.stocksimulator.debug.Log
import com.stocksimulator.macros.PMaker._
import com.stocksimulator.macros._
abstract class RatioArbitrerStrategy(market: Market, param: Parameters) extends QuoteOnlyStrategy(market, param) {
  val symbolA: Stock
  val symbolB: Stock
  val gran: Double
  val maxPos: Int
  val step: Int
  lazy val roundUp = StrategyUtils.roundUpFactory(gran)
  lazy val roundDown = StrategyUtils.roundDownFactory(gran)

  val elapsed_param: Int = (p"elapsed")
  val elapsed = elapsed_param *1000
  val spread: Double = p"spread"


  lazy val mvAvg = createRatioMAvg(symbolA, symbolB, elapsed / 1000, elapsed);


  def onQuotes() = {

    val pos = getPosition(symbolA).quantity
    if (mvAvg.isAvailable && mvAvg.lastVal > 0) {
      val infoPair = (getSymbol(symbolA), getSymbol(symbolB))
      infoPair match {
        case (a: Quote, b: Quote) =>
          val midPr = midPrice(b)
          val precoTeoricoA = mvAvg.lastVal * midPr
          val buyPrice = roundDown(Math.min(a.bid.price, precoTeoricoA - spread))
          val sellPrice = roundUp(Math.max(a.ask.price, precoTeoricoA + spread))
          if (pos < maxPos && buyPrice > 0) {
            provideBuyLiquidity(symbolA, step, buyPrice)
          }

          if (pos > -maxPos && sellPrice > 0) {
            provideSellLiquidity(symbolA, step, sellPrice)
          }

        case _ => {}
      }
    }
  }

}








