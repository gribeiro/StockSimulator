package com.stocksimulator.remote
import org.joda.time._
import com.stocksimulator.reuters._
import com.stocksimulator.abs._
import com.stocksimulator.parallel._
import com.stocksimulator.debug._
import scala.collection.mutable.ArrayBuffer


object BoostrapConf {

  def createHourFilter(from: DateTime, to: DateTime) = HourFilter(from, to)

  def createHourFilter(from: String, to: String) = HourFilter(new DateTime(from), new DateTime(to))

}
case class BootstrapConf(filename: String, mongoConfig: MongoConfig, localWorkers: Int, name: String, inst: Set[Stock], components: List[MarketComponent], filter: Filter = EmptyFilter)

object CommonBootstrap {
  val parametersAcc = new ArrayBuffer[Parameters]

}
class CommonBootstrap[T <: Strategy](conf: BootstrapConf, params: List[Parameters], cStrat: Class[T], date: String) {
  val sharedMongo = new SharedMongo(conf.mongoConfig, conf.filter)
  val workers = new Workers(conf.localWorkers, createBundle, conf.name)
  
  
  private var instruments: Option[Set[Stock]] = None
  private var memory: Option[Array[Map[Stock, StockInfo]]] = None
  
  private def createFeed():Feed = {
		  new FeedFromClone(memory.get, instruments.get)
  }
 
  def terminated = workers.terminated
  
  def loadMongo() = sharedMongo
  
  def createBundle(param: Parameters) = {
     
    val feed = createFeed()
    val market = new ReutersMarket(feed, conf.components)
    val strategy = cStrat.getConstructor(classOf[Market], classOf[Parameters]).newInstance(market, param)
    strategy
  }
  
  
  private def bulkLoad = {
     val optionInst = conf.inst.map {
       inst => inst.checkOption(date)
     }
     val sharedFeed = new ReutersSharedMongoFeed(conf.inst, sharedMongo)
    memory = Some(sharedFeed.cloneContent)
    instruments = Some(sharedFeed.instruments)
  }
  def run() = {
    
   
    bulkLoad
    workers.master ! spMongoConfig(conf.mongoConfig)
    val uniqueParams = params.toArray.distinct
    Log("Job count after filter: " +uniqueParams.length) 
    uniqueParams.foreach {
      p => workers.master ! spWork(p, date)
    }
    workers.master ! spLast
    workers.system.awaitTermination()
  }
  
}