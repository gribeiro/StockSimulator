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

class RBSFactory {}
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


class PreRubyBSAdapter[T <: RubyBSAdapter](val myFilename: String, val date: String, klass: Class[T]) {
  def unroll = klass.getConstructor(classOf[RubyString], classOf[RubyString]).newInstance(myFilename, date)
}


trait RubyStrategyTypes  {
  def strategyTypes = Map("RubyRatioAdapter" -> classOf[RubyRatioStrategy],
      "RubyDoubleRatioAdapter" -> classOf[RubyDoubleRatioStrategy],
      "TestStrategy" -> classOf[TestStrategy])
  def generic = classOf[RubyStdStrategy]
}
abstract class RubyBSAdapter(val myFilename: String, date: String) extends BSAdapter(date) with RubyStrategyTypes {
  def getBS[T <: Strategy] = {
    strategyTypes.get(this.strategyType) match {
      case Some(klass) => new RubyBS(this, klass)
      case None => new RubyBS(this, generic)
    }
  }
  def rbFilename: String
  def rbKlass: String
}


class RubyBS[T <: Strategy](rb: RubyBSAdapter, klass: Class[T]) extends AdapterBSSet[T](rb, klass) {

  def myFilename: String = rb.myFilename
  val filename = myFilename
  def rbFilename: String = rb.rbFilename
  def rbKlass: String = rb.rbKlass
  RubyStrategyLoader.filename = rbFilename
  RubyStrategyLoader.klass = rbKlass

}


