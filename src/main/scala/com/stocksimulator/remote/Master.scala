package com.stocksimulator.remote

import akka.actor.ActorSystem
import com.typesafe.config.ConfigFactory
import com.stocksimulator.abs.Parameters
import akka.actor.Actor
import com.stocksimulator.abs.Strategy
import akka.routing.Router
import akka.routing.RoundRobinRoutingLogic
import akka.actor.ActorRef
import akka.actor.Props
import com.stocksimulator.parallel._
import akka.actor.ActorSelection
import javax.script.ScriptEngineManager
import java.io.FileReader
import java.io.File
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
import com.stocksimulator.reuters._
import com.stocksimulator.output.MongoOutput
import com.mongodb.DBObject
import com.stocksimulator.main.ConfigurationModule._
import com.stocksimulator.main.VarParam
import akka.actor.OneForOneStrategy


class MasterRemoteActor extends Actor with BinaryHandler[Job] { 
  import akka.actor.OneForOneStrategy
  import akka.actor.SupervisorStrategy._
  import scala.concurrent.duration._
  
  override val supervisorStrategy =
    OneForOneStrategy(maxNrOfRetries = 10, withinTimeRange = 1 minute) {
      case _: Exception => Restart
    }
  var router = Router(RoundRobinRoutingLogic())
  def broadcast = Router(akka.routing.BroadcastRoutingLogic(), router.routees)
  val jobMap = new HashMap[Job, JobState]

  def getJobsToDo = {
    (for ((job, status) <- jobMap; if (status == JobToDo || status == JobConfirm)) yield job).to[ArrayBuffer]
  }

  def getJobStats(name: String) = {
    val jobs = (for ((job, status) <- jobMap; if (job.name == name && (status == JobRunning || status == JobDone))) yield job).to[ArrayBuffer]
    jobs.size
  }
  
  def extraFinishReceive(name: String, arr: Array[Byte], extraData: Option[Job]) = {
      println("Saving to mongo...")
      val job = extraData.get
      val resultB = ByteArrayToObject[DBObject](arr)
      val conf = ConfigurationLoadJson(job.fs).get
      val symbols = conf.symbols mkString "."
      //val saveMongo = new SaveMongo(conf.name + "_" + symbols, conf.name, conf.name)
      //saveMongo.withMongoObject(resultB)
  }
  
  def simpleJobSend(sender: ActorRef) = {
    val jobList = groupedJobList
    if (jobList.length > 0) {
      val job = jobList.head
      println("Sending job, name: " + job.name + " parameters: ")
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
    def compare(x: String, y: String) = {
      if (getJobStats(x) < getJobStats(y)) 1
      else if (getJobStats(x) > getJobStats(y)) -1
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
      for (key <- kPriorityQ; elem <- grouped(key); if (prejobList.contains(elem))) yield {
        prejobList -= elem
        elem
      }
    }
    while (prejobList.size > 0) {
      jobList ++= pickOne
    }
    jobList
  }

 
  def receive = {
    case ResultChunk(name, chunk) =>
    	receiveChunk(name, chunk)
    case ResultEnd(name, job) =>
    	finishReceive(name, Some(job))

    case Register =>
      context watch sender
      router = router.addRoutee(sender)
      println("Routee added:" + sender)
      val jobList: ArrayBuffer[Job] = getJobsToDo
      simpleJobSend(sender)
      sender ! RegisterOk
    case Idle(job) =>
      jobMap(job) match {
        case JobRunning(actor, false) =>
          jobMap(job) = JobDone //JobConfirm
        case _ =>
          jobMap(job) = JobDone
      }

    //simpleJobSend(sender)
    case FileRequest(filename) =>
      println("Requested:" +filename)
      val loadfile = FileManager.loadOnMemory(filename)
      val partMessage = (part: Array[Byte]) => FileChunk(filename, part)
      val endMessage = () => FileEnd(filename)
      val chopper = ChopBinary(partMessage, endMessage, sender)
      chopper(loadfile)
    case WorkerJobRequest =>
      simpleJobSend(sender)
    case MasterJob(fs, javaFs) =>
      val configOption = ConfigurationLoadJson(fs)
      sender ! JobAck
      future {
        for (config <- configOption; (date, filename) <- config.loadedCSVs(false)) {
          println(s"$date -> $filename")
          val varParam = VarParam(config)
          val name = config.name
          FileManager(filename)
          val varParamGrouped = varParam.grouped(3)
          for (paramGroup <- varParamGrouped) {
            val inputStrs = paramGroup.map(f => f.inputStr).toArray
            val newJob = Job(filename, date, fs, inputStrs, name, javaFs) -> JobToDo
            
            jobMap += newJob
            broadcast.route(NewJobArrived, self)
          }
     

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
    case JobFailed(job) =>
      jobMap(job) = JobToDo
    case Terminated(actor) =>
      println(actor + ": Terminated!")
      for (jobDouble <- jobMap) {
        jobDouble match {
          case (job, JobRunning(byActor, _)) =>
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

