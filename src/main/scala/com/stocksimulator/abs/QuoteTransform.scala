package com.stocksimulator.abs

trait StockInfoFunction {
  def apply(si: StockInfo): StockInfo
}
trait StockInfoDouleFunction {
  def apply(sia: StockInfo, sib: StockInfo): StockInfo
}
abstract class StockInfoBiTransform {
  val transformA: StockInfoFunction
  val transformB: StockInfoFunction
  val finalTransform: StockInfoDouleFunction
  
  def apply(sia:StockInfo, sib: StockInfo): StockInfo = {
    val tia = transformA(sia)
    val tib = transformB(sib)
    
    finalTransform(tia, tib)
  }
}
