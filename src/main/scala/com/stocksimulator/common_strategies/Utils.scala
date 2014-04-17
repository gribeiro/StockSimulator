package com.stocksimulator.common_strategies

import com.stocksimulator.debug.Log

object StrategyUtils {
 import com.stocksimulator.helpers.ImplicitClasses._  

 

  private def integerUp(gran: Int)(n: Double) = {
      val rounded = Math.ceil(n).toInt
      if (rounded % gran != 0) rounded + (gran - rounded % gran) else rounded
  }
  
  private def integerDown(gran: Int)(n: Double) = {
     val rounded = Math.floor(n).toInt
      rounded - rounded % gran
  }
  def roundUpFactory(gran: Int) = {
	 integerUp(gran) _
  }

  def roundDownFactory(gran: Int) = {
    integerDown(gran) _
  }
  
  private def doubleFactory(myFun: (Int) => (Double) => (Int),gran: Double) = {
    val fractionalSize = gran.toString().dropWhile(d => d != '.').size.toDouble
    val toI = math.pow(10, fractionalSize-1)
    val intPart = (gran*toI).intValue()
    (n: Double) => myFun(intPart)((n*toI))/toI
  }
  
  def roundUpFactory(gran: Double) = {
    doubleFactory(integerUp, gran)
  }
  
  def roundDownFactory(gran: Double) = {
    doubleFactory(integerDown, gran)
  }
  
  
}