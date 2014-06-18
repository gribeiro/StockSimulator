package com.stocksimulator.abs

import scala.collection.mutable.HashMap

import com.stocksimulator.abs.EventTC._
import com.stocksimulator.abs.RunningContextModule._

class StrategyProvideLiquidity(strategy: BuySellAdapter) extends BuySellAdopt(strategy) {

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

  def updateAll(): Unit = pLiq.values.map(pl => pl.update())

  class ProvideLiquidity(symbol: Stock, val e: Event) extends Function2[Int, Double, Unit] {
    private var lastQtd: Int = 0
    private var lastPrice: Double = 0
    private var lastTicketStatus: TunnelTicketStatus = Killed
    
    def extract(x: Iterable[(Stock, Double)]) = x.collectFirst {
      case f if(f._1 == symbol) => f._2
    }.get
    
    //val max = extract(rc.max)
    
    //val min = extract(rc.min)
    
    
    def update() = {
      lastTicketStatus = getStatus(lastTicketStatus)
      val newEvent = event(lastTicketStatus)
      if (!natureCompare(e, newEvent)) throw new IncompatibleNatureException
    }

    val (action, replace) = e match {
      case Buy => (buy _, replaceBuy _)
      case Sell => (sell _, replaceSell _)
    }

    def apply(qtd: Int, price: Double): Unit = {
      update()
     //
      if ((lastPrice == price && lastQtd == qtd && !(lastTicketStatus == Killed))) return
      lastPrice = price
      lastQtd = qtd

      lastTicketStatus match {
        case Killed =>
          lastTicketStatus = action(symbol, qtd, price)
        case w: Wait =>
          appendWaitOrder(w, qtd, price, e, symbol)
        case r: Ready =>
          lastTicketStatus = replace(r.ticket, qtd, price)
      }

    }
  }
}