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


object BSTypeClass {
  trait BSLike[T] {
    def bootstrap(bootstrapable: T):List[(Parameters, Parameters)]
  }
  
  object BSLike {
    implicit object BSLikeRemoteJava extends BSLike[RemoteJavaBSSet] {
      def bootstrap(bootstrapable: RemoteJavaBSSet) = bootstrapable.bootstrap.run.toList
    }
    implicit object BSLikeJavaBSSet extends BSLike[JavaBSSet] {
      def bootstrap(bootstrapable: JavaBSSet) = bootstrapable.bootstrap.run.toList
    }
  }
}


case object VarParam { 
  def apply(confPar: List[ConfigParam]): Array[Parameters] = confPar.map(p => (p.base, p.to.getOrElse(p.base), p.by.getOrElse(1.0), p.name)).getParamArray
  def apply(conf: Configuration):Array[Parameters] = conf.parameters.map(p => (p.base, p.to.getOrElse(p.base), p.by.getOrElse(1.0), p.name)).getParamArray
  def apply(conf: Configuration, filter: List[String]):Array[Parameters] = {
    val varparam = apply(conf)
    varparam.filter {
      vp =>
        filter.contains(vp.inputStr)
    }
  }
}

abstract class BSSet(configuration: Configuration, date: String, filename: String) {
   val inst = configuration.symbols.map(Stock(_)).toSet
   val varParamList = VarParam(configuration)
  protected val mc:ListBuffer[MarketComponent] = ListBuffer(ReutersMarketComponents.standardBookOrder(configuration.bookOrder))
   val conf = BootstrapConf(configuration.workers, configuration.name, inst, mc.toList, configuration.from, configuration.to)
   def generator: (Market, Parameters) => Strategy 

   
   
}


class RemoteJavaBSSet(configuration: Configuration, date: String, filename: String, javafs: String, filters: Option[List[String]]) extends JavaBSSet(configuration, date, filename) {
  override def javaFile = javafs
  
  override val varParamList = filters match {
    case Some(filter) => VarParam(configuration, filter)
    case None => VarParam(configuration)
  }

}

class JavaBSSet(configuration: Configuration, val date: String, val filename: String) extends BSSet(configuration, date, filename) {
  def generator = CreateStrategyForAdapter(configuration.javaFilename,javaFile)
  protected def javaFile = Source.fromFile(configuration.javaFilename+".java", "utf-8").getLines mkString "\n"
  def bootstrap = new CommonBootstrap(conf, varParamList, date, filename, generator)
}