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
import scala.collection.mutable.HashMap
import com.stocksimulator.parallel.UmbraBootstrap
import com.stocksimulator.parallel.CommonBootstrap
import com.stocksimulator.parallel.BootstrapConf
import com.stocksimulator.helpers.ParamGen._
import scala.io.Source
import com.stocksimulator.java_loader._
import com.stocksimulator.main.ConfigurationModule._

import com.stocksimulator.abs.RunningContextModule._


abstract class BSSet(configuration: Configuration, date: String, filename: String) {
  val inst = configuration.symbols.map(Stock(_)).toSet
  val varParamList = VarParam(configuration)
  protected val mc: ListBuffer[MarketComponent] = ListBuffer(ReutersMarketComponents.standardBookOrder(configuration.bookOrder))
  val conf = BootstrapConf(configuration.workers, configuration.name, inst, mc.toList, configuration.from, configuration.to)
  def generator: (Market, Parameters) => Strategy
  val bootstrap: UmbraBootstrap
  protected def javaFile = ""
}

class RemoteJavaBSSet(configuration: Configuration, date: String, filename: String, javafs: String, filters: Option[List[String]])(implicit pc: PreContext) extends JavaBSSet(configuration, date, filename) {
  
  //implicit val runningContext: RunningContext = bootstrap.runningContext
  override def javaFile = javafs
  
  override val varParamList = filters match {
    case Some(filter) => VarParam(configuration, filter)
    case None => VarParam(configuration)
  }

  override implicit lazy val runningContext:RunningContext =  {
   
   val currentRc = bootstrap.runningContext
   val rcBuilder = RunningContextModule.contextBuilder withRunningContext(currentRc) withId pc.id
  rcBuilder build
  }
}

class JavaBSSet(configuration: Configuration, val date: String, val filename: String) extends BSSet(configuration, date, filename) {
  lazy val bootstrap = new CommonBootstrap(conf, varParamList, date, filename)
  implicit lazy val runningContext:RunningContext = bootstrap.runningContext
  def generator = CreateStrategyForAdapter(configuration.javaFilename, javaFile)
  override protected def javaFile = Source.fromFile(configuration.javaFilename + ".java", "utf-8").getLines mkString "\n"
}