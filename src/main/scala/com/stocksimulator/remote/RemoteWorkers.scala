package com.stocksimulator.remote

import akka.actor.ActorSystem
import com.typesafe.config.ConfigFactory
import com.stocksimulator.abs.Parameters
import akka.actor.Actor
import com.stocksimulator.abs.Strategy
import com.stocksimulator.reuters.MongoConfig
import com.stocksimulator.reuters.ReutersMongoSave
import com.stocksimulator.main.BSSet
import akka.routing.Router
import akka.routing.RoundRobinRoutingLogic
import akka.actor.ActorRef
import akka.actor.Props
import com.stocksimulator.parallel._
import akka.actor.ActorSelection
import com.stocksimulator.main.RubyBSAdapter
import javax.script.ScriptEngineManager
import java.io.FileReader
import com.stocksimulator.main.PreRubyBSAdapter
import java.io.File
import com.stocksimulator.main.LogMe
import com.stocksimulator.main.RubyBS
import com.stocksimulator.common_strategies.RubyRatioStrategy
import com.stocksimulator.common_strategies.RubyStdStrategy
import java.util.ArrayList
import scala.collection.mutable.ArrayBuffer
import scala.concurrent.duration.FiniteDuration
import akka.util.Timeout
import java.util.concurrent.TimeUnit
import scala.collection.mutable.HashMap
import akka.actor.Terminated
import io.jvm.uuid._

class JobSender extends Actor {
  def receive = {
    case MasterJobSender(actor, j) =>
      println("Sending job..")
      actor ! j
    case JobAck =>
      println("Job sent..")
      context.system.shutdown
  }
}

class RemoteJobActor extends Actor {

  def loadOneAdapter(filename: String, date: String, str: String) = {
    val manager = new ScriptEngineManager
    val engine = manager.getEngineByName("jruby")
    engine.eval(str)
    engine.eval("Start.setVar()")
    engine.eval("RubyConfFactory.getNew(\"" + filename + "\", \"" + date + "\")").asInstanceOf[RubyBSAdapter]

  }

  def receive = {
    case Job(filename, date, fs) =>
      context watch sender
      sender ! JobAckSmall(Job(filename, date, fs))
      val adapter = loadOneAdapter(filename, date, fs)
      adapter.getBS.bootstrap.run
      sender ! Idle(Job(filename, date, fs))
    case Master(m) =>
      m ! Register
    case NoJob =>
    //context.system.shutdown
  }
}

class MasterRemoteActor extends Actor {

  var router = Router(RoundRobinRoutingLogic())
  val jobMap = new HashMap[Job, JobState]
  def loadRubyObj(str: String) = {
    val manager = new ScriptEngineManager
    val engine = manager.getEngineByName("jruby")

    engine.eval(str)

    engine.eval("Start.run").asInstanceOf[Array[RubyBSAdapter]]

  }
  def getJobsToDo = {
    (for ((job, status) <- jobMap; if (status == JobToDo || status == JobConfirm)) yield job).to[ArrayBuffer]
  }
  def simpleJobSend(sender: ActorRef) = {
    val jobList: ArrayBuffer[Job] = getJobsToDo
    if (jobList.length > 0) {
      val job = jobList.head
      sender ! job
      jobList -= job
      jobMap(job) match {
        case JobConfirm => jobMap(job) = JobSent(true)
        case _ => jobMap(job) = JobSent(false)
      }

    } else {
      sender ! NoJob
    }
  }
  def receive = {

    case Register =>
      context watch sender
      router = router.addRoutee(sender)
      println("Routee added:" + sender)
      val jobList: ArrayBuffer[Job] = getJobsToDo
      simpleJobSend(sender)
    case Idle(job) =>
      jobMap(job) match {
        case JobRunning(actor, false) =>
          jobMap(job) = JobConfirm
        case _ =>
          jobMap(job) = JobDone
      }

      simpleJobSend(sender)

    case MasterJob(fs) =>
      sender ! JobAck
      val allAdapters = loadRubyObj(fs)

      for (adapter <- allAdapters) {
        adapter.getBS.sharedMongo.raw
        val filename = adapter.myFilename
        val date = adapter.date
        jobMap += Job(filename, date, fs) -> JobToDo
      }

      val jobList: ArrayBuffer[Job] = getJobsToDo
      for (i <- 1 to router.routees.size) {
        val job = jobList.head
        val a = router.route(job, self)
        jobList -= job

        jobMap(job) match {
          case JobConfirm => jobMap(job) = JobSent(true)
          case _ => jobMap(job) = JobSent(false)
        }
      }

    case JobAckSmall(job) =>
      val jobStatus = jobMap.get(job) match {
        case Some(jS) => jS
        case None => jobMap(job) = JobSent(false)
      }
      jobStatus match {
        case JobSent(true) => jobMap(job) = JobRunning(sender, true)
        case _ => jobMap(job) = JobRunning(sender, false)
      }
    case Terminated(actor) =>
      println(actor + ": Terminated!")
      for (jobDouble <- jobMap) {
        jobDouble match {
          case (job, JobRunning(byActor,_) ) =>
            if (byActor == actor) {
              println("Job returned to list...")
              jobMap(job) = JobToDo
            }
          case _ =>

        }
      }
    case Ping =>
      println("Ping? Pong!")
  }
}

class RemoteWorkers {
  var jSystem: Option[ActorSystem] = None
  def localMaster = {
    val config = ParCommon.remoteConfig(2552)

    val listenerSystem = ActorSystem("masterSystem", config)
    listenerSystem.actorOf(Props(classOf[MasterRemoteActor]), "Master")

    listenerSystem.awaitTermination()

  }

  def localWorker(ip: String, port: Int) = {
    val emitterSystem = ActorSystem("workerSystem", ParCommon.remoteConfigWithoutPort)
    val master = emitterSystem.actorSelection(s"akka.tcp://masterSystem@$ip:$port/user/Master")
    val worker = emitterSystem.actorOf(Props(classOf[RemoteJobActor]), "Worker")
    master ! Ping
    worker ! Master(master)
    emitterSystem.awaitTermination()
  }

  def emitWork(ip: String, port: Int, j: MasterJob) = {
    val sys = jSystem match {
      case None => {
        jSystem = Some(ActorSystem("jobSystem", ParCommon.remoteConfigWithoutPort))
        jSystem.get
      }
      case Some(s) => s
    }
    val master = sys.actorSelection(s"akka.tcp://masterSystem@$ip:$port/user/Master")
    val jobSender = sys.actorOf(Props(classOf[JobSender]), "JobSender")
    master ! Ping
    jobSender ! MasterJobSender(master, j)
  }

}