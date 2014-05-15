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
import com.stocksimulator.parallel.CommonBootstrap
import com.stocksimulator.parallel.BootstrapConf
import com.stocksimulator.helpers.ParamGen._
import scala.io.Source
import com.stocksimulator.java_loader._
import com.stocksimulator.main.ConfigurationModule._

object BSTypeClass {
  trait BSLike[T] {
    def bootstrap(bootstrapable: T): List[(Parameters, Parameters)]
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
  implicit def strStr2Parameters(strStr: (String, String)): Parameters = {
    val newP = new Parameters
    newP.set(strStr._1, strStr._1)
    newP
  }
  def addParam(multi: List[Array[Parameters]]): Array[Parameters] = {
    multi.reduce(addParam(_, _))
  }
  def addParam(a: Array[Parameters], b: Array[Parameters]) = {
    for (aP <- a; bP <- b) yield {
      val newP = new Parameters
      List(aP.mem, bP.mem).foreach {
        all =>
          all.foreach {
            xs => newP.set(xs._1, xs._2)
          }
      }
      newP
    }
  }
  def choose(strParam: Map[String, List[String]]) = {
    val firstVals = strParam.map {
      case (k, v :: vs) => (k, v.head)
      case (k, _) =>
    }
  }
  def apply(confPar: List[ConfigParam]): Array[Parameters] = confPar.map(p => (p.base, p.to.getOrElse(p.base), p.by.getOrElse(1.0), p.name)).getParamArray
  
  def getStrs(stringPar: List[StringParam]): Array[Parameters] = {
    
        val mapParams = Utils.perm(stringPar.map {
          case StringParam(k, list) => list.map { (k, _) }
        }.flatten)
        val paramsMapped = mapParams.map {
          subList => 
          val newParam = new Parameters
            subList.foreach {
            case (k,v) => newParam.set(k,v) 
          }
          newParam
        }.toArray
        paramsMapped
  }
  
  def apply(confPar: List[ConfigParam], stringPar: List[StringParam]): Array[Parameters] = {
    addParam(apply(confPar), getStrs(stringPar))
  }
  def apply(conf: Configuration): Array[Parameters] = {
    val numericParams = conf.parameters.map(p => (p.base, p.to.getOrElse(p.base), p.by.getOrElse(1.0), p.name)).getParamArray
    conf.stringParam match {
      case Some(strPrm) =>
      val paramsMapped = getStrs(strPrm)
        addParam(numericParams, paramsMapped)

      case None => numericParams
    }
  }
  def apply(conf: Configuration, filter: List[String]): Array[Parameters] = {
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
  protected val mc: ListBuffer[MarketComponent] = ListBuffer(ReutersMarketComponents.standardBookOrder(configuration.bookOrder))
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
  def generator = CreateStrategyForAdapter(configuration.javaFilename, javaFile)
  protected def javaFile = Source.fromFile(configuration.javaFilename + ".java", "utf-8").getLines mkString "\n"
  def bootstrap = new CommonBootstrap(conf, varParamList, date, filename, generator)
}