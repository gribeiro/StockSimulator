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
import com.stocksimulator.helpers.RingBuffer
import com.stocksimulator.helpers.RingBuffer
import akka.routing.Routee
import scala.collection.mutable.PriorityQueue
import scala.concurrent._
import ExecutionContext.Implicits.global

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
	var running = false
  def loadOneAdapter(filename: String, date: String, str: String) = {
    val manager = new ScriptEngineManager
    val engine = manager.getEngineByName("jruby")
    engine.eval(str)
    engine.eval("Start.setVar()")
    engine.eval("RubyConfFactory.getNew(\"" + filename + "\", \"" + date + "\")").asInstanceOf[RubyBSAdapter]

  }

  def receive = {
    case NewJobArrived =>
      if(!running) sender ! WorkerJobRequest
    case job: Job =>
      running = true
      context watch sender
      sender ! JobAckSmall(job)
      val adapter = loadOneAdapter(job.filename, job.date, job.fs)
      val BS = adapter.getBS
     val paramToRun = adapter.varParam.filter {
        p => job.parameter.contains(p.inputStr)
      }
      if(paramToRun.size > 0) {
           paramToRun.foreach(p => BS.addExplicitParameter(p))
           BS.bootstrap.run
      }
        else {
          println("No param has been sent")
          BS.bootstrap.run
      }
     
      //.bootstrap.run
      sender ! Idle(job)
    case Master(m) =>
      m ! Register
    case NoJob =>
      running = false
    //context.system.shutdown
  }
}


class MasterRemoteActor extends Actor {
   
  var router = Router(RoundRobinRoutingLogic())
  def broadcast = Router(akka.routing.BroadcastRoutingLogic(), router.routees)
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
  
  def getJobStats(name: String) = {
    val jobs = (for ((job, status) <- jobMap; if( job.name == name &&  (status == JobRunning || status == JobDone))) yield job).to[ArrayBuffer]
    jobs.size
  }
  
  
  def simpleJobSend(sender: ActorRef) = {
    val jobList = groupedJobList
    if (jobList.length > 0) {
      val job = jobList.head
       println("Sending job for name: " + job.name + " parameters: ")
      job.parameter.foreach(f => println(f))
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
  
  
    
  val newStringOrd = new Ordering[String] {
        def compare(x:String, y: String) = {
          if(getJobStats(x) < getJobStats(y)) 1
          else if(getJobStats(x) > getJobStats(y)) -1
          else 0
        }
      }
  private def groupedJobList = {
      val prejobList: ArrayBuffer[Job] = getJobsToDo
      val jobList = new ArrayBuffer[Job](prejobList.size)
      val grouped = prejobList.groupBy(f => f.name)
    
      val keys = grouped.keys
      val kPriorityQ = new PriorityQueue[String]()(newStringOrd)
      keys.foreach { key =>
        kPriorityQ.enqueue(key)
      }
      def pickOne = {
        for(key <- kPriorityQ; elem <- grouped(key); if(prejobList.contains(elem))) yield {
          prejobList -= elem
          elem
        }
      }
      while(prejobList.size > 0) {
        jobList ++= pickOne
      }
      jobList
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
     
    case WorkerJobRequest =>
      simpleJobSend(sender)
    case MasterJob(fs) =>
      sender ! JobAck
      val allAdapters = loadRubyObj(fs)

      for (adapter <- allAdapters) {
   
        val filename = adapter.myFilename
        val date = adapter.date
        val varParam = adapter.varParam
        val name = adapter.name
        future {
        if(!adapter.getBS.bootstrap.loadMongo.haveData)
          adapter.getBS.bootstrap.loadMongo.raw
        val varParamGrouped = varParam.grouped(3)
        for(paramGroup <- varParamGrouped) {
          val inputStrs = paramGroup.map(f => f.inputStr).toArray
          jobMap += Job(filename, date, fs, inputStrs, name) -> JobToDo
        }
        broadcast.route(NewJobArrived, self)
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