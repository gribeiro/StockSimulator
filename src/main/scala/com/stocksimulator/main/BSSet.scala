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
import com.stocksimulator.parallel.CommonBootstrap
import com.stocksimulator.parallel.BootstrapConf
import com.stocksimulator.helpers.ParamGen._
import scala.io.Source
import com.stocksimulator.java_loader._
import com.stocksimulator.main.ConfigurationModule._
trait LogMe { 
  Log.setActive(true)
  Log("Starting bootstrap...")
  
}

abstract class BSSet[T <: Strategy] {

	val filename: String
    
	protected val inst: Set[Stock]
	protected val mc: ListBuffer[MarketComponent]
	protected val varParamList: Array[Parameters]
	protected val conf: BootstrapConf
	protected val date: String


	def bootstrap: CommonBootstrap[T] 
	def run = bootstrap.run	

}

case object VarParam { 
  def apply(conf: Configuration):Array[Parameters] = conf.parameters.map(p => (p.base, p.to.getOrElse(p.base), p.by.getOrElse(1.0), p.name)).getParamArray
  def apply(conf: Configuration, filter: List[String]):Array[Parameters] = {
    val varparam = apply(conf)
    varparam.filter {
      vp =>
        filter.contains(vp.inputStr)
    }
  }
}

class RemoteJavaBSSet(configuration: Configuration, date: String, filename: String, javafs: String, filters: Option[List[String]]) extends JavaBSSet(configuration, date, filename) {
  override def javaFile = javafs
  
  override val varParamList = filters match {
    case Some(filter) => VarParam(configuration, filter)
    case None => VarParam(configuration)
  }

}

class JavaBSSet(configuration: Configuration, val date: String, val filename: String) extends BSSet[JavaStdStrategy] {
  
  val inst = configuration.symbols.map(Stock(_)).toSet
  
  protected val mc:ListBuffer[MarketComponent] = ListBuffer(ReutersMarketComponents.standardBookOrder(configuration.bookOrder))
  val varParamList = VarParam(configuration)
  val conf = BootstrapConf(configuration.workers, configuration.name, inst, mc.toList, configuration.from, configuration.to)
  protected def javaFile = Source.fromFile(configuration.javaFilename+".java", "utf-8").getLines mkString "\n"

  def bootstrap = new CommonBootstrap[JavaStdStrategy](conf, varParamList, date, filename, CreateStrategyForAdapter(configuration.javaFilename,javaFile))
}