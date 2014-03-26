package com.stocksimulator.common_strategies
import com.stocksimulator.abs._
import com.stocksimulator.debug.Log
import com.stocksimulator.macros.PMaker._
import com.stocksimulator.macros._

abstract class DoubleRatioArbitrerStrategy(market: Market, param: Parameters) extends QuoteOnlyStrategy(market, param) {
  val symbolA: Stock
  val symbolB: Stock
  
  val symbolC: Stock
  
  val gran: Int
  val maxPos: Int
  val step: Int
  lazy val roundUp = StrategyUtils.roundUpFactory(gran)
  lazy val roundDown = StrategyUtils.roundDownFactory(gran)

  val elapsed: Int = p"elapsed"
  val spread: Int = p"spread"
  val spreadMax: Int = p"spread_max"
  val spreadMin: Int = p"spread_min"
  val flag: String = p"flag"
  
  val spreadParams = SpreadParams(maxPos, spreadMax, spreadMin)
  val spreadDinamico = new SpreadDinamico(this, spread, symbolA, flag, spreadParams)
  

  lazy val mvAvg = createRatioFor2SymbolsMAvg(symbolA, symbolB, symbolC, elapsed / 1000, elapsed);


  def onQuotes() = {

    val pos = getPosition(symbolA).quantity
    //if (Math.abs(pos) >= maxPos) -timeExitCallBack else +timeExitCallBack
    if (mvAvg.isAvailable) {
      //Log(this.lastTick)
      val infoPair = (getSymbol(symbolA), getSymbol(symbolB), getSymbol(symbolC))
      infoPair match {
        case (a: Quote, b: Quote, c: Quote) =>
          val midPr = midPrice(b)*midPrice(c)
          val precoTeoricoA = mvAvg.lastVal * midPr

    
          val spreadVenda = spreadDinamico.buySpread
          val spreadCompra = spreadDinamico.sellSpread

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