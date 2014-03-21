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

object TimeControl {
 implicit def timeControl2Boolean(t: TimeControl): Boolean = t()
}
class TimeControl(inst: Set[Stock]) {

  private val buffer: PriorityQueue[StockInfo] = PriorityQueue.empty[StockInfo]
  private var lastMup: Map[Stock, StockInfo] = Map.empty[Stock, StockInfo]
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
	   val nextPeriod = new Period(cCurrent, peekNext.iDatetime)
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
           sInfo = lastQuoteOcorr(sI.iStock)
         case _ => {}
       }
       
         val newSINfo = StockInfo(sInfo, sInfo.iDatetime.plusMillis(currentDelay))
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
    val temp = LinkedHashMap.empty[Stock, StockInfo]
    val earlierInfo = buffer.dequeue()
    earlierInfo match {
      case q: Quote =>
        lastQuoteOcorr(q.stock) = q
      case _ => {}
    }
    val eiSt = earlierInfo.iStock
    
    calculateTime(earlierInfo)
    temp.put(eiSt, earlierInfo)

    for (stock <- inst; if (stock != eiSt)) {
      lastMup.get(stock) match {
        case Some(s) => temp.put(stock, s)
        case None => temp.put(stock, buffer.filter(si => si.iStock == stock).minBy(si => si.iDatetime))
      }
    }

    lastMup = temp.toMap
    
    lastMup
  }
  def <-- (mup: Map[Stock, StockInfo]) = save(mup) // Alias
  def save(mup: Map[Stock, StockInfo]) = {
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