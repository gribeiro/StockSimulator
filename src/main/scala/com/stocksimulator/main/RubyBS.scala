package com.stocksimulator.main

import com.github.nscala_time.time.Imports._
import com.stocksimulator.reuters._
import com.stocksimulator.abs._
import scala.util.{ Success, Failure }
import scala.util.Failure
import com.stocksimulator.reuters.ReutersMarket
import scala.concurrent._
import com.stocksimulator.debug._
import com.stocksimulator.java._
import scala.concurrent.ExecutionContext.Implicits.global
import akka.actor.{ Props, ActorSystem, ActorRef, Actor }
import com.typesafe.config.ConfigFactory
import akka.routing.ActorRefRoutee
import akka.routing.Router
import akka.routing.RoundRobinRoutingLogic
import akka.routing.ActorRefRoutee
import akka.actor.Terminated
import com.stocksimulator.parallel._
import scala.collection.mutable.ListBuffer
import com.stocksimulator.remote._
import com.stocksimulator.common_strategies.RubyStdStrategy
import com.stocksimulator.common_strategies.RubyStrategyLoader
import com.stocksimulator.common_strategies.RatioArbitrerStrategy
import com.stocksimulator.common_strategies.RubyRatioStrategy
import com.stocksimulator.data_manger._
import org.jruby._
import scala.collection.mutable.ArrayBuffer
import org.joda.time.format.DateTimeFormat
import com.mongodb.casbah.commons.MongoDBObject
import com.stocksimulator.helpers._
import com.stocksimulator.common_strategies.RubyDoubleRatioStrategy
import com.stocksimulator.common_strategies.RubyDoubleRatioStrategy

object RBSFactory {

  /*case class wList(list: List[wSeries]) {
       
     }
     
     trait wSeries {
       def aggregator(subNum: Int):wSeries
     }
     
     object wNull extends wSeries {
       def aggregator(subNum: Int):wSeries = this
     }
     
  	 case class wYear(year: Int) extends wSeries {
  	   
  	   def withMonth(month: Int):wMonth = {
  	     wMonth(year, month)
  	   }
  	   
  	   def withMonths(months: List[Int]) = {
  	    val cont =  months.map(f => { wMonth(year, f)})
  	    wList(cont)
  	   }
  	   
  	 }
  	 case class wMonth(year: Int, month: Int) extends wSeries {
  	   def withDay(day: Int) = {
  	     wDay(year, month, day)
  	   }
  	   
  	   def withDays(days: List[Int]) = {
  	     days.map {f =>
  	     wDay(year, month, f)  
  	     }
  	   }
  	 }
  	 case class wDay(year: Int, month: Int, day: Int) extends wSeries {
  	   def get(): String = {
  	     List(year, month, day).mkString("/")
  	   }
  	 }
	 def withYear(year: Int) = wYear(year)
	 
	 implicit def wDayList2Strings(w: List[wDay]):Array[String] = {
	   val result = w.map {
	     f =>
	       f.get()
	   }
	   result.toArray
	 }
	// 
	 val teste:Array[String] = withYear(2013).withMonth(2).withDays(List(1, 2, 3, 4))*/

  val symbols = ArrayBuffer.empty[String]
  var mongoOutputSymbol:Stock = ""
  var outputName = ""
  var log = true
  var delay = 100
  def setOutputName(s: String) = {
    outputName = s
  }

  def setDelay(a: Int) = {
    delay = a
  }
  def setMongoOutputSymbol(s: String) = {
    mongoOutputSymbol = Stock(s)
  }

  def setLog(b: Boolean) = {
    log = b
  }
  
  def cleanSymbols = symbols.clear
  def pushSymbol(s: String) = {
    symbols += s
  }
  def getFile(date: String) = {
    // val javaSymbols = symbols.toArray(new Array[String](symbols.size()))
    //val javaDate = date.asJavaString()
    hcReuters.apply(symbols.toArray, date)
  }

}

class RBSFactory {

}

class PreRubyBSAdapter[T <: RubyBSAdapter](val myFilename: String, val date: String, klass: Class[T]) {
  def unroll = klass.getConstructor(classOf[RubyString], classOf[RubyString]).newInstance(myFilename, date)
}

object RubyBSAdapter {
  val dateFormat = DateTimeFormat.forPattern("dd/MM/yyyy")


}
@serializable
abstract class RubyBSAdapter(val myFilename: String, val date: String) {
 val excludedHours = new ArrayBuffer[(String, String)]
 def getBS[T <: Strategy] = {
    val BS = this.strategyType match {
      case "RubyRatioAdapter" =>
        val rubyBS = new RubyBS[RubyRatioStrategy](this, classOf[RubyRatioStrategy])
        rubyBS
      case "RubyDoubleRatioAdapter" =>
        val rubyBS = new RubyBS[RubyDoubleRatioStrategy](this, classOf[RubyDoubleRatioStrategy])
        rubyBS
      case "TestStrategy" =>
        val rubyBS = new RubyBS[TestStrategy](this, classOf[TestStrategy])
        rubyBS
        
      case _ =>
        val rubyBS = new RubyBS[RubyStdStrategy](this, classOf[RubyStdStrategy])
        rubyBS
    }
  BS
  }
  val datetime = RubyBSAdapter.dateFormat.parseDateTime(date)

  def mConfig: MongoConfig
  def name: String
  def actorsQtd: Int
  def myInst: Set[Stock]
  def bookOrder: Int
  def watchSymbol: Array[String]
  def rbFilename: String
  def rbKlass: String
  def varParam: Array[Parameters]
  def from: String
  def to: String
  def strategyType: String
  def replace: Boolean
  def postRun(): Unit
  def instMaker(values: Array[String]) = {
    val preInst = for (v <- values) yield Stock(v)
    preInst.toSet
  }

  def mongoConfigMaker(hostname: String, port: Int, name: String, filename: String) = {
    MongoConfig(hostname, port, name, filename)
  }
  def pushExcludedHour(from: String, to: String):Unit = {
    val pair = (from, to)
    excludedHours += pair
  }
  
}

class RubyBS[T <: Strategy](rb: RubyBSAdapter, val klass: Class[T]) extends BSSet[T] {
  Log.setActive(RBSFactory.log)
  rb.postRun()
  val hourFormat = DateTimeFormat.forPattern("HH:mm:ss")
  val from = hourFormat.parseDateTime(rb.from).plus(rb.datetime.getMillis())
  val to = hourFormat.parseDateTime(rb.to).plus(rb.datetime.getMillis())
  val excluded = for((mFrom, mTo) <- rb.excludedHours) yield {
      val transformedFrom = hourFormat.parseDateTime(mFrom).plus(rb.datetime.getMillis())
      val transformedTo = hourFormat.parseDateTime(mTo).plus(rb.datetime.getMillis())
    HourFilter(transformedFrom, transformedTo)
  }
  def myFilename: String = rb.myFilename
  def mConfig: MongoConfig = rb.mConfig
  def name: String = rb.name + "-" + rb.date.filter(p => p != '/')
  def actorsQtd: Int = rb.actorsQtd
  def myInst: Set[Stock] = rb.myInst
  def bookOrder: Int = rb.bookOrder
  val tradingSymbol = rb.watchSymbol
  val date = rb.date

  if(rb.replace) {
    val mongo = MongoClientSingleton(rb.mConfig)
    val db = mongo("stockSimulator")
    val toRemove = MongoDBObject("date" -> Functions.changePatterns(date))
    Log("It'll remove: " + date)
    val coll = db(RBSFactory.outputName)
    val removeResult = coll.remove(toRemove)
  }
  // val from = new DateTime(rb.from)
  //val to = new DateTime(rb.to)

  val miniHourFilter = HourFilter(from, to)
  override val hourFilter:Filter = if(excluded.size == 0) miniHourFilter else ExtendedHourFilter(miniHourFilter, excluded.toArray)

  def rbFilename: String = rb.rbFilename
  def rbKlass: String = rb.rbKlass
  def varParam: Array[Parameters] = rb.varParam

  RubyStrategyLoader.filename = rbFilename
  RubyStrategyLoader.klass = rbKlass

  val filename = myFilename
  val inst = myInst
  val logActive = true
  val mongoConfig = mConfig
  override lazy val sharedMongo = new SharedMongo(mongoConfig, hourFilter)

  val mc = new ListBuffer[MarketComponent]
  if(tradingSymbol.size == 1) {
	  mc += ReutersMarketComponents.counterComponent(tradingSymbol.head)
  	}
  else if(tradingSymbol.size > 1) {
    val multArg = tradingSymbol.map(tS => Stock(tS)).toSet 
    mc += ReutersMarketComponents.multiCounterComponent(multArg)
  }
  mc += ReutersMarketComponents.standardBookOrder(bookOrder)

  val varParamSz = varParam.size
  Log(s"It'll run $varParamSz simulations...")
  val conf = BootstrapConf(filename, mongoConfig, actorsQtd, name, inst, mc.toList, hourFilter)
  val varParamList = varParam.toList

  Log("Starting bootstrap...")
}


