package com.stocksimulator.parallel
import org.joda.time._
import com.stocksimulator.reuters._
import com.stocksimulator.abs._
import com.stocksimulator.parallel._
import com.stocksimulator.debug._
import com.stocksimulator.debug.LogNames._
import scala.collection.mutable.ArrayBuffer
import akka.actor.actorRef2Scala
import com.stocksimulator.abs.Market

case class BootstrapConf(localWorkers: Int, name: String, inst: Set[Stock], components: List[MarketComponent], from: String, to: String)

object CommonBootstrap {
  val parametersAcc = new ArrayBuffer[(Parameters, Parameters)]
  var feed:Feed = null
  def setFeed(f: Feed) = {
    feed = f
  } 
}
class CommonBootstrap[T <: Strategy](conf: BootstrapConf, params: Array[Parameters], date: String, filename: String, generator: (Market, Parameters) => T) {

  val workers = new Workers(conf.localWorkers, createBundle, conf.name)
 
  def terminated = workers.terminated
  private def loadTime(timeStr: String) = {
   val timeComplete = Array(date, timeStr+".000") mkString " "
   val dateFormat = ReutersCommon.dateFormat
   
   val datez = DateTime.parse(timeComplete, dateFormat).toDateTime(DateTimeZone.forID("America/Sao_Paulo"))
   val datez2 = datez.minusHours(datez.getZone().getOffset(datez.getMillis())/(60*60*1000))
   this.log(datez2)
   datez2
  }
  val from = loadTime(conf.from)
  val to = loadTime(conf.to)
  val filefeed = (new FileFeedTimeFilter(new FileFeed(filename, conf.inst))).timeFiltering(from, to)
  CommonBootstrap.setFeed(filefeed)
  def createBundle(param: Parameters) = {
    val feed = filefeed
    val market = new ReutersMarket(feed, conf.components)
    
    val strategy = generator(market, param)
    //val strategy = strategyManifest.erasure.getConstructor(classOf[Market], classOf[Parameters]).newInstance(market, param).asInstanceOf[T]
    //val strategy = cStrat.getConstructor(classOf[Market], classOf[Parameters]).newInstance(market, param)
    strategy
  }


  def run() = {
    
    val uniqueParams = params.toArray.distinct
   
    this.log("Job count after filter: " +uniqueParams.length) 
    uniqueParams.foreach {
      p => workers.master ! spWork(p, date)
    }
    workers.master ! spLast
    workers.system.awaitTermination()
    this.log("Local worker system terminated...")
    val result = CommonBootstrap.parametersAcc
    val sendResult  = ArrayBuffer.empty ++ result
    result.clear
    sendResult
  }
  
}