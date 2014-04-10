import com.stocksimulator.abs._
import com.stocksimulator.common_strategies._
import org.scalatest._

import Matchers._
import org.joda.time.DateTime
import scala.util.Random
import com.stocksimulator.reuters.ReutersMarket
import scala.collection.mutable.ListBuffer
import com.stocksimulator.reuters.ReutersMarketComponents
import com.stocksimulator.fancy_output.htmlCommon
import org.joda.time.Instant
import org.joda.time.LocalDate
import com.stocksimulator.main.Bootstrap._
import com.stocksimulator.debug.Log
import com.stocksimulator.remote.CommonBootstrap
import scalaz._
import Scalaz._

object TestFeed {
   val sTeste:Stock = "TESTE"
}
class TestFeed extends Feed {

 val basetime = new DateTime()
 private var lastTime = basetime
 val instruments = Set(TestFeed.sTeste)
 
 private def timeUpdate = {
   val nextRandom = Random.nextGaussian()
   val nextMillis = 100 + 10*nextRandom.toInt
   lastTime = lastTime.plusMillis(nextMillis)
   lastTime
 } 
 private def quote(bidPrice: Double, bidSz: Int, askPrice: Double, askSz: Int) = {
	val bid = PriceVol(bidPrice, bidSz)
	val ask = PriceVol(askPrice, askSz)
	Quote(TestFeed.sTeste, bid, ask, timeUpdate)
 }
 
 private def trade(price: Double, sz: Int) = {
   val priceVol = PriceVol(price, sz)
   Trade(TestFeed.sTeste, priceVol, timeUpdate)
 }
 val infos:List[StockInfo] = List(
     quote(100, 10, 112, 11), 
     quote(100, 8, 112, 11), 
     quote(100, 7, 101, 11), 
     quote(100, 8, 101, 10),
     trade(100, 1), 
     trade(100, 1),
     trade(101, 1), 
     quote(99, 9, 101, 10),
     trade(99, 1)
     )
 val totalDataSize = infos.size
 val pointer = infos.iterator
 def next(): Map[Stock, StockInfo] = Map(TestFeed.sTeste -> pointer.next) 
def hasNext(): Boolean	= pointer.hasNext
}

class EmptyStrategy(market:Market, param: Parameters) extends QuoteOnlyStrategy(market, param) {
  def onQuotes() = {}
}

class BuyAtStrategy(buyPrice: Double, market: Market, param: Parameters) extends QuoteOnlyStrategy(market, param) {
var volume = 0
var price = 0.0
  def onQuotes() = {
   provideBuyLiquidity(TestFeed.sTeste, 1, buyPrice)
  }
  
  override def onBuyReport(stock:Stock, volume:Int, price:Double) = {
 this.volume = volume
 this.price = price
  }
}
/*class StrategyTests extends FlatSpec  {
Log.stopActor
var midPrice: Double = 0.0
  "A Empty Strategy" should "end normally" in {
    val feed = new TestFeed
    val mc = List(ReutersMarketComponents.counterComponent(TestFeed.sTeste), ReutersMarketComponents.standardBookOrder(0))
    val market = new ReutersMarket(feed, mc, 100)
    val strategy = new EmptyStrategy(market, new Parameters)
    val result = strategy.init()
    val marketLast = result.get("marketLast").get.asInstanceOf[Map[Stock, StockInfo]]
    val sInfo = marketLast(TestFeed.sTeste)

  }

   "A agressive buy at 600 Strategy with no delay and 1-queue" should "buy some stock at 112" in {
    val feed = new TestFeed
    val mc = List(ReutersMarketComponents.counterComponent(TestFeed.sTeste), ReutersMarketComponents.standardBookOrder(1))
    val market = new ReutersMarket(feed, mc, 0)
    val strategy = new BuyAtStrategy(600 ,market, new Parameters)
    val result = strategy.init()
    val marketLast = result.get("marketLast").get.asInstanceOf[Map[Stock, StockInfo]]
    val sInfo = marketLast(TestFeed.sTeste)
    midPrice = Strategy.midPrice(sInfo)
    val pnl = htmlCommon.numericPnl(result) 
    
    //Tests
    pnl should be < 0.0
    strategy.volume should be (1)
    strategy.price should be (112.0)
  }
   
   "A buy at 101 Strategy with 0-queue" should "enter normal queue and buy nothing" in {
    val feed = new TestFeed
    val mc = List(ReutersMarketComponents.counterComponent(TestFeed.sTeste), ReutersMarketComponents.standardBookOrder(0))
    val market = new ReutersMarket(feed, mc, 100)
    val strategy = new BuyAtStrategy(100, market, new Parameters)
    val result = strategy.init()
    val marketLast = result.get("marketLast").get.asInstanceOf[Map[Stock, StockInfo]]
    val sInfo = marketLast(TestFeed.sTeste)
    val myMidPrice = Strategy.midPrice(sInfo)
    val pnl = htmlCommon.numericPnl(result) 
    
    //Tests
 
  
    pnl should be (0.0)
    strategy.volume should be (0)
    strategy.price should be (0.0)
  }

   it should "maintain last strategy final price" in {
     val feed = new TestFeed
    val mc = List(ReutersMarketComponents.counterComponent(TestFeed.sTeste), ReutersMarketComponents.standardBookOrder(0))
    val market = new ReutersMarket(feed, mc, 100)
    val strategy = new BuyAtStrategy(100, market, new Parameters)
    val result = strategy.init()
    val marketLast = result.get("marketLast").get.asInstanceOf[Map[Stock, StockInfo]]
    val sInfo = marketLast(TestFeed.sTeste)
    val myMidPrice = Strategy.midPrice(sInfo)
    val pnl = htmlCommon.numericPnl(result) 
    
     myMidPrice should be (midPrice)
   }
   
  "A WIN/IND testStrategy" should "not alter output" in {
     Log.setActive(false)
     val rubyBSAdapters = loadRuby("./teste.rb")
      val jobs = for (rubyBSAdapter <- rubyBSAdapters) yield {
        rubyBSAdapter.getBS
      }
      for (j <- jobs) {
        j.bootstrap.run()
      }
      val last = CommonBootstrap.parametersAcc.last
      htmlCommon.numericPnl(last) should be (255.0)
   }

}*/