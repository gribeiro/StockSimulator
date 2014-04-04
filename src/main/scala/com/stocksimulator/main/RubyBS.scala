package com.stocksimulator.main

import java.io.File
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
import com.stocksimulator.java_loader._
import scala.concurrent._
import scala.concurrent.duration._
import ExecutionContext.Implicits.global
import com.stocksimulator.helpers._
import com.stocksimulator.helpers.DateComposable._
import com.stocksimulator.abs.AutoDemotion._

class RBSFactory {}
object RBSFactory {

  
  
  val symbols = ArrayBuffer.empty[String]
  val futureDays = ArrayBuffer.empty[Future[String]]
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
  /*  val symb = symbols.toArray
    //val futureResult = //Future {

   
    val all = symb.map {
      str => Stock(str).checkOption(date).name
    }
    all.foreach {
      str => println(str)
    }
    hcReuters.apply(all, date)
    
    */
    //}
    
    //futureDays += futureResult
    ""
  }
  
  def waitForFiles = {
    /*
    futureDays.foreach {
      futureString =>
        val filename = Await.result(futureString, scala.concurrent.duration.Duration(500,"seconds"))
        Log(filename)
    }*/
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
  private def javaFilenameExt = javaFilename + ".java"
  def getBS[T <: Strategy] = {
    if(javaFilename == "") {
      strategyTypes.get(this.strategyType) match {
        case Some(klass) => new RubyBS(this, klass)
        case None => new RubyBS(this, generic)
      }
    }
    else {
      MongoClientSingleton(mConfig)
      val fileExistence = new File(javaFilenameExt).exists
      
      
      
      if(fileExistence) {
        MongoClientSingleton.saveFile(javaFilenameExt)
      }
       
      val tryOnBank = MongoClientSingleton.openFile(javaFilenameExt)
      tryOnBank match {
    	  case Some(file) =>
    	   // println(file)
    	    MemoryCompiler(javaFilename, file) 
    	  case None =>
    	     MongoClientSingleton.saveFile(javaFilename)
    	     val recentlySaved = MongoClientSingleton.openFile(javaFilenameExt).get
    	      MemoryCompiler(javaFilename, recentlySaved)
    	}
      
       
      

      new RubyBS(this, classOf[JavaStdStrategy])
    }
  }
  def javaFilename: String = ""
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


