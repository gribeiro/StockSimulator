package com.stocksimulator.remote

import akka.actor.ActorSystem
import com.typesafe.config.ConfigFactory
import com.stocksimulator.abs.Parameters
import akka.actor.Actor
import com.stocksimulator.abs.Strategy
import com.stocksimulator.main.BSSet
import akka.routing.Router
import akka.routing.RoundRobinRoutingLogic
import akka.actor.ActorRef
import akka.actor.Props
import com.stocksimulator.parallel._
import akka.actor.ActorSelection
import javax.script.ScriptEngineManager
import java.io.FileReader
import java.io.File
import com.stocksimulator.main.LogMe
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
  def fileExists(filename: String) = FileManager.fileExists(filename)
  val waitingJobs = ArrayBuffer.empty[(Job, Configuration, RunConfigurationRemote, String)]
  val files = HashMap.empty[String, ArrayBuffer[Byte]]
  
  def runJob(job: Job, conf: Configuration, runner: RunConfigurationRemote) = {
    val result = runner(conf)
    val mongos = result.map {
      case (a,b) =>
        (new MongoOutput(a, b, conf.name, conf.name)).output
    }
    mongos.foreach {
      mong => 
           val metalist = List(mong)
           val binary = ObjectToByteArray(mongos)
           val uuid =  java.util.UUID.randomUUID.toString
           
           val chopped = binary.grouped(4096)
           chopped.foreach {
             chop =>
               sender ! ResultChunk(uuid, chop)
           }
           sender ! ResultEnd(uuid, job)
    }
 
    sender ! Idle(job)
  }

  def receive = {
	 

    case FileChunk(filename, chunk) =>
      files(filename) ++= chunk
    case FileEnd(filename) =>
      val data = files(filename).toArray
      FileManager.saveFromMemory(data, filename)
      files.remove(filename)
      waitingJobs.collectFirst {
        case element @ (job, conf, runner, elementFN) if (filename == elementFN) =>
          waitingJobs -= element
          runJob(job, conf, runner)
          sender ! Idle(job)
      }
    case NewJobArrived =>
      if (!running) sender ! WorkerJobRequest
    case job @ Job(jFilename, date, fs, parameter, name, javaFs) => //(filename: String, date: String, fs: String, parameter: Array[String], name: String)
      running = true
      val filename = FileManager.datExtension(jFilename)
      context watch sender
      val configOption = ConfigurationLoadJson(fs)
      sender ! JobAckSmall(job)
      for (config <- configOption) {
        
        val runner = RunConfigurationRemote(javaFs, Some((parameter.toList)))
        if (fileExists(filename)) {
          runJob(job, config, runner)
          
        } else {
          val enuple = (job, config, runner, filename)
      println(enuple)
          files += (filename -> ArrayBuffer.empty[Byte])
          sender ! FileRequest(filename)

          waitingJobs += enuple

        }
      }

    case Master(m) =>
      m ! Register
    case NoJob =>
      running = false

  }
}

class MasterRemoteActor extends Actor {

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
  
  
  
   val result = HashMap.empty[String, ArrayBuffer[Byte]]
  def receive = {
    case ResultChunk(name, chunk) =>
   	result.get(name) match {
   	  case Some(buffer) => buffer ++= chunk
   	  case None => result(name) = (ArrayBuffer.empty[Byte] ++ chunk)
   	}
    case ResultEnd(name, job) => 
      println("Saving to mongo...")
      val arr = result(name).toArray
      result -= name
      val resultB = ByteArrayToObject[List[DBObject]](arr)
      val conf = ConfigurationLoadJson(job.fs).get
      val symbols = conf.symbols mkString "."
      val saveMongo = new SaveMongo(conf.name+"_"+symbols, conf.name, conf.name)
      resultB.foreach(saveMongo.withMongoObject(_))


    case Register =>
      context watch sender
      router = router.addRoutee(sender)
      println("Routee added:" + sender)
      val jobList: ArrayBuffer[Job] = getJobsToDo
      simpleJobSend(sender)
    case Idle(job) =>
      jobMap(job) match {
        case JobRunning(actor, false) =>
          jobMap(job) = JobDone //JobConfirm
        case _ =>
          jobMap(job) = JobDone
      }

      simpleJobSend(sender)
    case FileRequest(filename) => 
      val loadfile = FileManager.loadOnMemory(filename)
      val bytebuffer = ArrayBuffer.empty[Byte] ++ loadfile
      val grouped = bytebuffer.grouped(4096)
      grouped.foreach {
        byteGroup => sender ! FileChunk(filename, byteGroup.toArray)
      }
      sender ! FileEnd(filename)
    case WorkerJobRequest =>
      simpleJobSend(sender)
    case MasterJob(fs, javaFs) =>
      val configOption = ConfigurationLoadJson(fs)
      sender ! JobAck
      future {
        for (config <- configOption; (date, filename) <- config.loadedCSVs) {

          val varParam = VarParam(config)
          val name = config.name
          FileManager(filename)
          val varParamGrouped = varParam.grouped(3)
          for (paramGroup <- varParamGrouped) {
            val inputStrs = paramGroup.map(f => f.inputStr).toArray
            jobMap += Job(filename, date, fs, inputStrs, name, javaFs) -> JobToDo
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

object RemoteWorkers {
  var jSystem: Option[ActorSystem] = None
  def localMaster(host: String) = {
    val config = ParCommon.remoteConfig(2552, host)

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