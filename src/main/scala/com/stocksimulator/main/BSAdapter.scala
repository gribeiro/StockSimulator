package com.stocksimulator.main

import com.stocksimulator.reuters.MongoConfig
import com.stocksimulator.abs.Stock
import com.stocksimulator.abs.Parameters
import com.stocksimulator.common_strategies.RubyRatioStrategy
import com.stocksimulator.common_strategies.RubyDoubleRatioStrategy
import com.stocksimulator.java.TestStrategy
import com.stocksimulator.common_strategies.RubyStdStrategy
import scala.collection.mutable.ArrayBuffer
import org.joda.time.format.DateTimeFormat
import com.stocksimulator.abs.Strategy
import com.stocksimulator.debug.Log
import com.stocksimulator.reuters.HourFilter
import com.stocksimulator.reuters.MongoClientSingleton
import com.mongodb.casbah.commons.MongoDBObject
import com.stocksimulator.helpers._
import com.stocksimulator.reuters.Filter
import com.stocksimulator.reuters.SharedMongo
import scala.collection.mutable.ListBuffer
import com.stocksimulator.remote.BootstrapConf
import com.stocksimulator.reuters.ExtendedHourFilter
import com.stocksimulator.abs.MarketComponent
import com.stocksimulator.reuters.ReutersMarketComponents

import scala.collection.mutable.MutableList

object BSAdapter {
  val dateFormat = DateTimeFormat.forPattern("dd/MM/yyyy")

}

abstract class BSAdapter(val date: String) {
import com.stocksimulator.helpers.ParamGen
import com.stocksimulator.helpers.ParamGen._
  private val paramHelperStorage = MutableList.empty[(Double, Double, Double, String)]

  def addParam(from: Double, to: Double, by: Double, name: String) = {
    val newElem = (from, to, by, name)
    paramHelperStorage += newElem
  }

  def generateParams = paramHelperStorage.toList.getParamArray
  def mConfig: MongoConfig
  def name: String
  def actorsQtd: Int
  def myInst: Set[Stock]
  def bookOrder: Int
  def watchSymbol: Array[String]
 
  def varParam: Array[Parameters]
  def from: String
  def to: String
  def strategyType: String
  def replace: Boolean
  def postRun(): Unit
    val excludedHours = new ArrayBuffer[(String, String)]
 
  val datetime = BSAdapter.dateFormat.parseDateTime(date)


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

abstract class AdapterBSSet[T <: Strategy](rb: BSAdapter, val klass: Class[T]) extends BSSet[T] {
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
  //def myFilename: String = rb.myFilename
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


  def varParam: Array[Parameters] = rb.varParam

  //RubyStrategyLoader.filename = rbFilename
  //RubyStrategyLoader.klass = rbKlass

  //val filename = myFilename
  val inst = myInst
  val logActive = true
  val mongoConfig = mConfig
  override lazy val sharedMongo = new SharedMongo(mongoConfig, date, hourFilter)

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