package com.stocksimulator.abs

import scala.collection.mutable.HashMap

trait Event
object Buy extends Event
object Sell extends Event

class StrategyProvideLiquidity(strategy: Strategy) extends BuySellAdopt(strategy) {

  private val pLiq = new HashMap[(Stock, Event), ProvideLiquidity]

  def provideLiquidity(symbol: Stock, e: Event, qtd: Int, price: Double) = {
    val pL = pLiq.get((symbol, e)) match {
      case Some(one) => one
      case None =>
        val newPLiq = new ProvideLiquidity(symbol, e)
        pLiq += (symbol, e) -> newPLiq
        newPLiq
    }
    pL(qtd, price)
  }
  
  def updateAll():Unit = pLiq.values.map(pl => pl.update())
  
  class ProvideLiquidity(symbol: Stock, e: Event) extends Function2[Int, Double, Unit] {
    private var lastQtd: Int = 0
    private var lastPrice: Double = 0
    private var lastTicketStatus: TunnelTicketStatus = new Killed
    
    def update() = {
      lastTicketStatus = getStatus(lastTicketStatus)
    }
    def apply(qtd: Int, price: Double):Unit = {
      update()
      if(lastPrice == price && lastQtd == qtd) return
      lastPrice = price
      lastQtd = qtd
    
      e match {
        case Buy =>
         
          lastTicketStatus match {
            case k: Killed =>
             lastTicketStatus = buy(symbol, qtd, price)
            case w: Wait =>
              appendWaitOrder(w, qtd, price)
            case r: Ready =>
              lastTicketStatus = replaceBuy(r.ticket, qtd, price)
          }
          
        case Sell =>
           lastTicketStatus match {
            case k: Killed =>
             lastTicketStatus = sell(symbol, qtd, price)
            case w: Wait =>
              appendWaitOrder(w, qtd, price)
            case r: Ready =>
              lastTicketStatus = replaceSell(r.ticket, qtd, price)
          }
      }
    }
  }
}