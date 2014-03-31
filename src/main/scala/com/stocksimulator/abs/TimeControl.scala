package com.stocksimulator.abs
import org.joda.time._
import scala.collection.mutable.ListBuffer
import com.github.nscala_time.time.Imports._
import scala.collection.mutable.LinkedHashMap
import scala.collection.mutable.ListBuffer
import scala.collection.mutable.ArrayBuffer
import com.stocksimulator.debug._
import scala.collection.mutable.PriorityQueue
import com.stocksimulator.main.RBSFactory
import scala.collection.mutable.HashMap
import scala.concurrent._
import scala.concurrent.duration._

import ExecutionContext.Implicits.global
object TimeControl {
 implicit def timeControl2Boolean(t: TimeControl): Boolean = t()
}
class TimeControl(inst: Set[Stock]) {

  private val buffer: PriorityQueue[StockInfoHA] = PriorityQueue.empty[StockInfoHA]
  private var lastMup: Map[Stock, StockInfoHA] = Map.empty[Stock, StockInfoHA]
 private val lastQuoteOcorr: HashMap[Stock, Quote] = HashMap.empty[Stock, Quote]
  private val currentDelay = RBSFactory.delay
  private var lastTick:Option[DateTime] = None
  private var time: Int = 0
  private var nextTime: Int = 0
  
  def timePassed = time
  def hasTime: Boolean = time > 0
  
  def apply() = hasData()
  
  private def calculateTime(sI: StockInfo) = {
   val cLastTick = lastTick.getOrElse(sI.iDatetime)
   val cCurrent = sI.iDatetime
   val period = new Period(cLastTick, cCurrent)

   nextTime = if(buffer.size > 0) {
	   val peekNext = buffer.head
	   val nextPeriod = new Period(cCurrent, peekNext.unfold.iDatetime)
	    nextPeriod.getMillis()
   } else 0   
   
   if(nextTime > currentDelay) {
     val intDiv = nextTime/currentDelay
     //Log(intDiv)
     val resto = nextTime % currentDelay
     for(j <- 1 to intDiv by 1) {
       var sInfo = sI
       sI match {
         case t: Trade =>
           sInfo = lastQuoteOcorr.getOrElse(sI.iStock, sInfo)
           
         case _ => {}
       }
       
         val newSINfo = new StockInfoHA(StockInfo(sInfo, sInfo.iDatetime.plusMillis(currentDelay)))
         this <-- Map(sI.iStock -> newSINfo)
     }
   }
   
   time = period.getMillis()
    val debug = true
   lastTick = Some(sI.iDatetime)
  }
  def unary_! = receive()
  def receive() = {
      receiveWithoutMup()
  }

  private def receiveWithoutMup() = {
    val temp = LinkedHashMap.empty[Stock, StockInfoHA]
    val earlierInfo = buffer.dequeue()
    earlierInfo.unfold match {
      case q: Quote =>
        lastQuoteOcorr(q.stock) = q
      case _ => {}
    }
    val eiSt = earlierInfo.unfold.iStock
    
    calculateTime(earlierInfo.unfold)
    temp.put(eiSt, earlierInfo)

    for (stock <- inst; if (stock != eiSt)) {
      lastMup.get(stock) match {
        case Some(s) => temp.put(stock, s)
        case None => temp.put(stock, buffer.filter(si => si.unfold.iStock == stock).minBy(si => si.unfold.iDatetime))
      }
    }

    lastMup = temp.toMap
    
    lastMup
  }
  def <-- (mup: Map[Stock, StockInfoHA]) = save(mup) // Alias
  def save(mup: Map[Stock, StockInfoHA]) = {
     for (sinfo <- mup.values) {
      buffer.enqueue(sinfo)
    }
  }
  /*
  private def receiveWithMup(mup: Map[Stock, StockInfo]) = {
    //Log(mup.toString())
    val temp = LinkedHashMap.empty[Stock, StockInfo]
    save(mup)
    val earlierInfo = buffer.dequeue()
    calculateTime(earlierInfo)
    val eiSt = earlierInfo.iStock

    temp.put(eiSt, earlierInfo)
    for ((stock, sinfo) <- mup; if (stock != eiSt)) {
      lastMup.get(stock) match {
        case Some(s) => temp.put(stock, s)
        case None => temp.put(stock, sinfo)
      }
    }
    
    if(temp.size < inst.size) {
         for (stock <- inst; if (stock != eiSt)) {
      lastMup.get(stock) match {
        case Some(s) => temp.put(stock, s)
        case None => 
          if(buffer.size > 0) temp.put(stock, buffer.filter(si => si.iStock == stock).minBy(si => si.iDatetime))
      }
    }
    }
    lastMup = temp.toMap

    lastMup

  }
*/
  def hasData(): Boolean = buffer.size > 0

}