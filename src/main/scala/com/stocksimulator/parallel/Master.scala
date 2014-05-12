package com.stocksimulator.parallel
import com.github.nscala_time.time.Imports._
import com.stocksimulator.reuters._
import com.stocksimulator.abs._
import scala.util.{ Success, Failure }
import scala.util.Failure
import com.stocksimulator.reuters.ReutersMarket
import scala.concurrent._
import com.stocksimulator.debug._
import com.stocksimulator.debug.LogNames._
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
import com.stocksimulator.output._
import scala.collection.mutable.ArrayBuffer
import scala.{ Some, None }
import com.stocksimulator.main.Bootstrap


class Workers(n: Int, bundle: (Parameters) => Strategy, sId: String)(implicit res: ResultBuffer) {

  val config = ParCommon.config
  val system = ActorSystem("spSystem" + sId, config)
  def terminated = system.isTerminated
  val newWorkerNumber:Int = 3
  val master = system.actorOf(Props(new MasterActor(newWorkerNumber, bundle, sId)), "spManager")
}

class MasterActor(nWorkers: Int, createBundle: (Parameters) => Strategy, sId: String)(implicit res: ResultBuffer) extends Actor {
  var counter = 0
  var doneCounter = 0
  var resultToBeDone = false


  def workerName() = {
    counter += 1
    "Worker_" + counter
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

    case Terminated(worker) => 
      Log("Worker died :(")
      router = router.removeRoutee(worker)
      val newWorker = context.actorOf(Props(classOf[WorkerActor], createBundle), workerName())
      context watch newWorker
      router = router.addRoutee(newWorker)
      Log("New worker :D")
    

    case w: spWork =>
       router.route(w, self)
       resultToBeDone = true
      

    case result: spResult => 
      val saveParam = (result.a, result.b)
      res.add(saveParam)
      this.log(result)
      this.log("Result received!!")
      broadcast.route(spMasterChecking, self)

    
    case `spWorkInProgress` => Log("Worker allocated!")
    case `spLast` => 
      this.log("Received all work for this day.")
      if (!resultToBeDone) broadcast.route(spMasterChecking, self)
    
    case `spReportDone` => 
      this.log("Report Done")
      broadcast.route(spMasterChecking, self)
    

    case `spAllWorkDone` => 
      doneCounter += 1
      if (doneCounter == nWorkers) {
        this.log("Work Done!")
        context.system.shutdown()
        //Log.stopActor
      }
    
  }
}