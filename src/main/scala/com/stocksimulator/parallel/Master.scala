package com.stocksimulator.parallel
import com.github.nscala_time.time.Imports._
import com.stocksimulator.reuters._
import com.stocksimulator.abs._
import scala.util.{ Success, Failure }
import scala.util.Failure
import com.stocksimulator.reuters.ReutersMarket
import scala.concurrent._
import com.stocksimulator.debug._
import com.stocksimulator.java.CommonStrategy
import scala.concurrent.ExecutionContext.Implicits.global
import com.stocksimulator.java._
import akka.actor.{ Props, ActorSystem, ActorRef, Actor }
import com.typesafe.config.ConfigFactory
import akka.routing.ActorRefRoutee
import akka.routing.Router
import akka.routing.RoundRobinRoutingLogic
import akka.routing.ActorRefRoutee
import akka.actor.Terminated
import java.io._
import akka.actor.PoisonPill
import com.stocksimulator.fancy_output._
import scala.collection.mutable.ArrayBuffer

import scala.{ Some, None }

class Workers[T <: ResultActor](n: Int, bundle: (Parameters) => Strategy, sId: String, resultKlass: Class[T] = classOf[MongoResultActor]) {

  val config = ParCommon.config
  val system = ActorSystem("spSystem" + sId, config)
  def terminated = system.isTerminated
  val master = system.actorOf(Props(new MasterActor(n, bundle, resultKlass, sId)), "spManager")
}

class MasterActor[T <: ResultActor](nWorkers: Int, createBundle: (Parameters) => Strategy, resultActor: Class[T], sId: String) extends Actor {
  var counter = 0
  var doneCounter = 0
  var resultToBeDone = false
  var mongoConfig: Option[MongoConfig] = None
  def workerName() = {
    counter += 1
    "Worker_" + counter
  }
  val resultA = context.actorOf(Props(resultActor, sId))

  def resultCheck(in: Parameters, date: String): Boolean = {
    mongoConfig match {
      case Some(config) =>
        ResultUtils.checkResult(config, sId, in, date)
      case None => false
    }

  }

  var router = {
    val routees = Vector.fill(nWorkers) {
      val r = context.actorOf(Props(classOf[WorkerActor], createBundle), workerName())
      context watch r
      ActorRefRoutee(r)
    }
    Router(RoundRobinRoutingLogic(), routees)
  }

  def broadcast = Router(akka.routing.BroadcastRoutingLogic(), router.routees)
  def receive = {
    case spMongoConfig(conf) => {
      mongoConfig = Some(conf)
      Log("Mongo config received!")
    }
    case `spRequestMongoConfig` => {
      mongoConfig match {
        case Some(conf) => sender ! spMongoConfig(conf)
        case None =>
          sender ! spMongoConfigUnavailable
          Log("Mongo config requested!")
      }
    }
    case Terminated(worker) => {
      Log("Worker died :(")
      router = router.removeRoutee(worker)
      val newWorker = context.actorOf(Props(classOf[WorkerActor], createBundle), workerName())
      context watch newWorker
      router = router.addRoutee(newWorker)
      Log("New worker :D")
    }

    case w: spWork =>
      if (!resultCheck(w.bundleInfo, w.date)) {
        router.route(w, self)
        resultToBeDone = true
      }

    case a: spResult => {
      resultA ! a
      Log("Result was sent!")

    }
    case `spWorkInProgress` => Log("Worker allocated!")
    case `spLast` => {
      Log("Received all work for this day.")
      if (!resultToBeDone) broadcast.route(spMasterChecking, self)
    }
    case `spReportDone` => {
      Log("Report Done")
      broadcast.route(spMasterChecking, self)
    }

    case `spAllWorkDone` => {
      doneCounter += 1
      if (doneCounter == nWorkers) {
        Log("Work Done!")
        context.system.shutdown()
        //Log.stopActor
      }
    }
  }
}