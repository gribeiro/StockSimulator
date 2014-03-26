package com.stocksimulator.common_strategies

import com.stocksimulator.abs.Strategy
import com.stocksimulator.abs.Stock

trait SpreadUpdate {
  def sellSpread: Double
  def buySpread: Double
}

object SpreadFlag {
  implicit def string2SpreadFlag(s: String):SpreadFlag = {
    s match {
      case "entrada" => AjusteEntradaFlag
      case "saida" => AjusteSaidaFlag
      case "entrada_saida" => AjusteEntradaSaidaFlag
      case _ => NoneFlag
    }
  }
}

trait SpreadFlag 
case object AjusteEntradaFlag extends SpreadFlag
case object AjusteSaidaFlag extends SpreadFlag
case object AjusteEntradaSaidaFlag extends SpreadFlag
case object NoneFlag extends SpreadFlag

case class SpreadParams(posicaoMax: Int, spreadMax: Double, spreadMin: Double)
class SpreadEstatico(spread: Double) extends SpreadUpdate {
  def sellSpread = spread
  def buySpread = spread
}

class SpreadUpdateGenerator(buy: () => Double, sell: () => Double) extends SpreadUpdate {
  def sellSpread = buy()
  def buySpread = sell()
}

class SpreadDinamico(strategy: Strategy, spread: Double, stock: Stock, flag: SpreadFlag, params: SpreadParams) extends SpreadUpdate {
    private val spreadDefault = new SpreadEstatico(spread)
	
    private def position = strategy.getPosition(stock).quantity.toDouble
    private def ratio = (position/params.posicaoMax)
    private def ajusteEntradaSell = () => {
       if(position <= 0) spread - ratio*(params.spreadMax - spread) else spreadDefault.sellSpread
    }
    
    private def ajusteSaidaSell = () => {
      if(position > 0) spread - ratio*(spread - params.spreadMin) else spreadDefault.sellSpread
    }
    
    private def ajusteEntradaBuy = () => {
      if(position >= 0) spread + ratio*(params.spreadMax - spread) else spreadDefault.buySpread
    }
    
    private def ajusteSaidaBuy = () => {
      if(position < 0) spread + ratio*(spread - params.spreadMin) else spreadDefault.buySpread
    }
    
    private def mixedBuy = () => {
      if(position > 0) spread + ratio*(params.spreadMax - spread) 
      else if(position < 0) spread + ratio*(spread - params.spreadMin) else spreadDefault.buySpread
    }
    
    private def mixedSell = () => {
      if(position < 0) spread - ratio*(params.spreadMax - spread) 
      else if(position > 0) spread - ratio*(spread - params.spreadMin) else spreadDefault.sellSpread
    }
    private def callNext(value: Double)(fun1: () => Double, fun2: () => Double)  = () => {
      val f = fun1()
      if(f == value) fun2() else f
    }
    private val callNextBuy = callNext(spreadDefault.buySpread) _
    
    private val callNextSell = callNext(spreadDefault.sellSpread) _
    
    
    private val generator:SpreadUpdate = flag match {
      case AjusteEntradaFlag => new SpreadUpdateGenerator(ajusteEntradaBuy, ajusteEntradaSell)
      case AjusteSaidaFlag => new SpreadUpdateGenerator(ajusteSaidaBuy, ajusteSaidaSell)
      case AjusteEntradaSaidaFlag =>
        //val buyPart = callNextBuy(ajusteEntradaBuy, ajusteSaidaBuy)
        //val sellPart = callNextSell(ajusteEntradaSell, ajusteSaidaSell)
        new SpreadUpdateGenerator(mixedBuy, mixedSell)
      case NoneFlag => spreadDefault
    }
    
    def sellSpread = generator.sellSpread
    def buySpread = generator.buySpread
}