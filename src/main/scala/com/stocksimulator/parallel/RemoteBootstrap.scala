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

abstract class UmbraBootstrap[T <: Strategy](conf: BootstrapConf, params: Array[Parameters], date: String, filename: String, generator: (Market, Parameters) => T) {
  self: ResultAccComponent =>
  implicit val result2 = self.result
  def run: Array[(Parameters, Parameters)]
  protected def loadTime(timeStr: String) = {
    val timeComplete = Array(date, timeStr + ".000") mkString " "
    val dateFormat = ReutersCommon.dateFormat

    val datez = DateTime.parse(timeComplete, dateFormat).toDateTime(DateTimeZone.forID("America/Sao_Paulo"))
    val datez2 = datez.minusHours(datez.getZone().getOffset(datez.getMillis()) / (60 * 60 * 1000))
    this.log(datez2)
    datez2
  }

  val processedInst = conf.inst.map {
    stock => stock.checkOption(date)
  }
  val from = loadTime(conf.from)
  val to = loadTime(conf.to)
  val filefeed = (new FileFeedTimeFilter(new FileFeed(filename, processedInst))).timeFiltering(from, to)

  def createBundle(param: Parameters) = {
    val feed = filefeed
    val market = new ReutersMarket(feed, conf.components)

    val strategy = generator(market, param)
    strategy
  }

}
class CommonBootstrap[T <: Strategy](conf: BootstrapConf, params: Array[Parameters], date: String, filename: String, generator: (Market, Parameters) => T) extends UmbraBootstrap(conf, params, date, filename, generator) with ResultAccDefaultImpl {

  //val workers = new Workers(conf.localWorkers, createBundle, conf.name)

  def run() = {
    this.log("Run started..")
    val uniqueParams = params.toArray.distinct

    this.log("Job count after filter: " + uniqueParams.length)
    val results = uniqueParams.map {
      params =>
        params.set("_date", date)
        val strategy = createBundle(params)
        (params, strategy.init) // Block current actor flow.
      // p => workers.master ! spWork(p, date)
    }
    //workers.master ! spLast
    //workers.system.awaitTermination()
    this.log("Local worker system terminated...")
    //result.parametersResult
    results
  }

}

trait ResultAccDefaultImpl extends ResultAccComponent {
  implicit val result = new ResultAcc

  class ResultAcc extends ResultBuffer {
    private val parametersAcc = new ArrayBuffer[(Parameters, Parameters)]
    def add(par: (Parameters, Parameters)) = {
      parametersAcc += par
      this.log("Parameter will be on result:" + par)
    }
    def parametersResult = parametersAcc.toArray
  }
}

  trait ResultBuffer {
    def add(par: (Parameters, Parameters))
    def parametersResult: Array[(Parameters, Parameters)]
  }
trait ResultAccComponent {
  implicit val result: ResultBuffer

}