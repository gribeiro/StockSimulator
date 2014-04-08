package com.stocksimulator.main
import com.github.nscala_time.time.Imports._
import com.stocksimulator.reuters._
import com.stocksimulator.abs._
import scala.util.{Success, Failure}
import scala.util.Failure
import com.stocksimulator.reuters.ReutersMarket
import scala.concurrent._
import com.stocksimulator.debug._
import com.stocksimulator.java._
import scala.concurrent.ExecutionContext.Implicits.global
import akka.actor.{Props, ActorSystem, ActorRef, Actor}
import com.typesafe.config.ConfigFactory
import akka.routing.ActorRefRoutee
import akka.routing.Router
import akka.routing.RoundRobinRoutingLogic
import akka.routing.ActorRefRoutee
import akka.actor.Terminated
import com.stocksimulator.parallel._
import scala.collection.mutable.ListBuffer
import com.stocksimulator.remote._
import scala.collection.mutable.HashMap

trait LogMe { 
  Log.setActive(true)
  Log("Starting bootstrap...")
  
}

abstract class BSSet[T <: Strategy] {
	
	protected val filename: String
	val hourFilter:Filter = EmptyFilter
	protected val mongoConfig: MongoConfig
	
    
	protected val inst: Set[Stock]
	protected val mc: ListBuffer[MarketComponent]
	protected val varParamList: List[Parameters]
	protected val conf: BootstrapConf
	protected val klass: Class[T]
	protected val date: String


	lazy val bootstrap = new CommonBootstrap(conf, varParamList, klass, date)
	
	
	def loadMongo() = bootstrap.loadMongo()
	
	
	
}