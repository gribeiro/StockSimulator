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
import com.stocksimulator.common_strategies.RubyStrategyLoader
import javax.script.ScriptEngineManager
import java.io.FileReader
import com.stocksimulator.common_strategies.RubyRatioStrategy
import com.stocksimulator.common_strategies.RubyStdStrategy
import java.io.File

object Bootstrap {
  var localWorkerQtd: Option[Int] = None
  def loadRuby(filename: String) = {
    val manager = new ScriptEngineManager
    val engine = manager.getEngineByName("jruby")
    val filereader = new FileReader(filename)
    engine.eval(filereader)

    engine.eval("Start.run").asInstanceOf[Array[RubyBSAdapter]]

  }

  def run[T <: Strategy](b: BSSet[T]) = b.bootstrap.run
  def main(args: Array[String]): Unit = {
    val iType = args(0)
    val rWorkers = new RemoteWorkers
    if (iType == "local") {
      val rubyBSAdapters = loadRuby(args(1))
      val jobs = for (rubyBSAdapter <- rubyBSAdapters) yield {
        rubyBSAdapter.getBS
      }
      for (j <- jobs) {
        j.bootstrap.run()
      }
    } else if (iType == "remote") {

      val nodeType = args(1)
      
      if (nodeType == "master") {
    	ParCommon.hostname = args(2)
        rWorkers.localMaster
      } else if (nodeType == "worker") {
        val masterIP = args(2)
        val masterPort: Int = args(3).toInt
        localWorkerQtd = if(args.size >4) Some(args(4).toInt) else None
        rWorkers.localWorker(masterIP, masterPort)
      }
    } else if (iType == "job") {
      println("Sending remote job..")
      val masterIP = args(1)
      val masterPort: Int = args(2).toInt
      val file = args(3)
      val rubyBSAdapters = loadRuby(file)
      val jobs = for (rubyBSAdapter <- rubyBSAdapters) yield {
        rubyBSAdapter.getBS
      }
      val fileContents = scala.io.Source.fromFile(file).mkString
      val masterJob = MasterJob(fileContents)
      rWorkers.emitWork(masterIP, masterPort, masterJob)

    }
    Log.stopActor
  }

}