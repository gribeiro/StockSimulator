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

class WorkerActor(createBundle: (Parameters) => Strategy) extends Actor {
  val myName = self.path.name
  var once = true
  def receive = {
    case spWork(params, date) =>

      val master = sender
      this.log(s"$myName : Received command for date $date")
      this.log(s"$myName : Starting bundle with argument: $params ...")
      master ! spWorkInProgress
      val strategy = createBundle(params)
      val result = strategy.init // Block current actor flow.
      this.log(s"$myName : Done!")
      master ! spResult(params, result)

    case `spMasterChecking` => {
      if (once) {
        sender ! spAllWorkDone
        once = false
      }
    }
  }
}

