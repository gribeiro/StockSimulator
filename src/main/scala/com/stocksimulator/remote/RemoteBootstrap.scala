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
 
  val workers = new Workers(conf.localWorkers, createBundle, conf.name)
  
  def terminated = workers.terminated
  
  def loadMongo() = new SharedMongo(conf.mongoConfig, conf.filter)
  
  def createBundle(param: Parameters) = {
     val sharedMongo = new SharedMongo(conf.mongoConfig, conf.filter)
    val feed = new ReutersSharedMongoFeed(conf.inst, sharedMongo)
    val market = new ReutersMarket(feed, conf.components)
    val strategy = cStrat.getConstructor(classOf[Market], classOf[Parameters]).newInstance(market, param)
    strategy
  }
  
  def run() = {
    workers.master ! spMongoConfig(conf.mongoConfig)
    params.foreach {
      p => workers.master ! spWork(p, date)
    }
    workers.master ! spLast
    workers.system.awaitTermination()
  }
  
}