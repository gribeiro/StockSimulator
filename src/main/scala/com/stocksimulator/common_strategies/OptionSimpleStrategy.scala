package com.stocksimulator.common_strategies

import com.stocksimulator.abs._

abstract class OptionSimpleStrategy(market: Market, param: Parameters)  extends RatioArbitrerStrategy(market, param) {
  
  if(!symbolB.isOption) throw new Exception("Stock should be a option in this field")
  override lazy val mvAvg = createRatioMAvgForOption(symbolA, symbolB, elapsed / 1000, elapsed);
}