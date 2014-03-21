package com.stocksimulator.common_strategies
import com.stocksimulator.abs._
import com.stocksimulator.debug.Log
import com.stocksimulator.macros.PMaker._
import com.stocksimulator.macros._
abstract class RatioArbitrerStrategy(market: Market, param: Parameters) extends QuoteOnlyStrategy(market, param) {
  val symbolA: Stock
  val symbolB: Stock
  val gran: Int
  val maxPos: Int
  val step: Int
  lazy val roundUp = StrategyUtils.roundUpFactory(gran)
  lazy val roundDown = StrategyUtils.roundDownFactory(gran)

  val elapsed: Int = ParamMaker("elapsed")
  val spread_entrada: Int = ParamMaker("spread_entrada")
  val spread_max: Int = ParamMaker("spread_max")
  val timeToExit: Int = ParamMaker("time_exit")

  val timeExitCallBack = new SimpleCallBack(timeToExit * 1000, timeExitCallBackExec)
  -timeExitCallBack
  windows <-- timeExitCallBack


  lazy val mvAvg = createRatioMAvg(symbolA, symbolB, elapsed / 1000, elapsed);

  def timeExitCallBackExec(): Unit = {
    Log("Time is up! Exiting position...")
    exitPosition(symbolA, 50.0)
    -timeExitCallBack
  }
  def onQuotes() = {

    val pos = getPosition(symbolA).quantity
    if (Math.abs(pos) >= maxPos) -timeExitCallBack else +timeExitCallBack
    if (mvAvg.isAvailable) {
      val infoPair = (getSymbol(symbolA), getSymbol(symbolB))
      infoPair match {
        case (a: Quote, b: Quote) =>
          val midPr = midPrice(b)
          val precoTeoricoA = mvAvg.lastVal * midPr

          val mod = (pos / maxPos) * (spread_max - spread_entrada)
          val spread = spread_entrada
          val spreadVenda = Math.max(spread - mod, spread)
          val spreadCompra = Math.max(spread + mod, spread)

          val buyPrice = roundDown(Math.min(a.bid.price, precoTeoricoA - spreadCompra))
          val sellPrice = roundUp(Math.max(a.ask.price, precoTeoricoA + spreadVenda));
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








