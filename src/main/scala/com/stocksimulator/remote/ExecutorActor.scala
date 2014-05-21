package com.stocksimulator.remote

import akka.actor.Actor
import com.stocksimulator.main.ConfigurationModule.Configuration
import com.stocksimulator.main.ConfigurationModule.RunConfigurationRemote
import akka.routing.Router




trait ExecutorCompound {
  import akka.actor.OneForOneStrategy
  import akka.actor.SupervisorStrategy._
  import scala.concurrent.duration._
  import akka.actor.ActorContext
  import akka.actor.ActorRef
  val context: ActorContext
  val self: ActorRef
  val executorSupervisorStrategy =
    OneForOneStrategy(maxNrOfRetries = 10, withinTimeRange = 1 minute) {
      case _: Exception => Restart
    }
  val router: Router
  def runJob(job: Job, conf: Configuration, runner: RunConfigurationRemote)
  val workers: Int
  def addSpot:Unit
  def request:Unit

}

class CommonExecutorActor extends Actor {

  def receive = {
   /* case RunnableJob(job, conf, runner) => sender ! ResultJob(runner(conf), job, conf)*/
    case _ => {}
  }
}

trait CommonExecutorCompound extends ExecutorCompound {
  
  import akka.actor.Props
  import akka.routing.ActorRefRoutee
  import akka.routing.RoundRobinRoutingLogic
  var maxWorkers = workers
  
  def addSpot = {
    maxWorkers += 1
  }
  
  val router = {
    val routees = Vector.fill(workers) {
      val r = context.actorOf(Props(classOf[CommonExecutorActor]))
      context watch r
      ActorRefRoutee(r)
    }
    Router(RoundRobinRoutingLogic(), routees)
  }
  
   def runJob(job: Job, conf: Configuration, runner: RunConfigurationRemote) = {
    maxWorkers -= 1
    router.route(RunnableJob(job, conf, runner), self)
    if (maxWorkers > 0) request
  }

}