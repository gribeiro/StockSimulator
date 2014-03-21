package com.stocksimulator.common_strategies

object StrategyUtils {
  
  def roundUpFactory(gran: Int) = {
    (n: Double) =>
      val rounded = Math.ceil(n).toInt
      if (rounded % gran != 0) rounded + (gran - rounded % gran) else rounded
  }

  def roundDownFactory(gran: Int) = {
    (n: Double) =>
      val rounded = Math.ceil(n).toInt
      rounded - rounded % gran
  }
}