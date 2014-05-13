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
import com.stocksimulator.reuters._
import com.stocksimulator.output.MongoOutput
import com.mongodb.DBObject
import com.stocksimulator.main.ConfigurationModule._
import com.stocksimulator.main.VarParam
import scala.concurrent._
import scala.util.Success
import akka.routing.ActorRefRoutee
import scala.concurrent.duration._
case class RunnableJob(job: Job, conf: Configuration, runner: RunConfigurationRemote)
case class ResultJob(result: List[(Parameters, Parameters)], job: Job, conf: Configuration)


object RemoteJobActor {
  val myUuid = ""
}

class RemoteJobActor(val workers: Int) extends Actor with BinaryHandler[Unit] with CommonExecutorCompound {
 exec: ExecutorCompound =>
   import context.dispatcher
   override val supervisorStrategy = exec.executorSupervisorStrategy
   def extraFinishReceive(name: String, arr: Array[Byte], extraData: Option[Unit]) = {
    val filename = name
    println("Received file:" + filename)
    val data = arr
    FileManager.saveFromMemory(data, filename)
    waitingJobs.collectFirst {
      case element @ (job, conf, runner, elementFN) if (filename == elementFN) =>
        waitingJobs -= element
        runJob(job, conf, runner)
    }
  }

  val system = akka.actor.ActorSystem("system")
  var ticker = context.system.scheduler.schedule(1000 millis, 1000 millis, self, "TryAgainBogus")
  var masterSelection: Option[ActorSelection] = None
  var master = self
  val myUuid = RemoteJobActor.myUuid
  var running = false
  def fileExists(filename: String) = FileManager.fileExists(filename)
  val waitingJobs = ArrayBuffer.empty[(Job, Configuration, RunConfigurationRemote, String)]

  def request = {
    master ! WorkerJobRequest
  }
  def receive = {
    case "TryAgainBogus" => {}
    
    case ResultJob(result, job, conf) =>
      addSpot
      val mongos = result.map {
        case (a, b) =>
          (new MongoOutput(a, b, conf.name, conf.name)).output
      }

      mongos.foreach {
        mong =>
          val binary = ObjectToByteArray(mong)
          val uuid = java.util.UUID.randomUUID.toString
          val partMessage = (chop: Array[Byte]) => ResultChunk(uuid, chop)
          val lastMessage = () => ResultEnd(uuid, job)
          val chopper = ChopBinary(partMessage, lastMessage, master)
          chopper(binary)
      }
      master ! Idle(job)
      request
      
    case FileChunk(filename, chunk) =>
      receiveChunk(filename, chunk)
    case FileEnd(filename) =>
      finishReceive(filename)
      
    case NewJobArrived =>
      if (!running) sender ! WorkerJobRequest
    case job @ Job(jFilename, date, fs, parameter, name, javaFs) => //(filename: String, date: String, fs: String, parameter: Array[String], name: String)
      println("Received Job")
      master = sender
      running = true
      val filename = FileManager.datExtension(jFilename)
      context watch sender
      val configOption = ConfigurationLoadJson(fs)
      sender ! JobAckSmall(job)
      for (config <- configOption) {

        val runner = RunConfigurationRemote(javaFs, Some((parameter.toList)), filename)
        if (fileExists(filename)) {
          runJob(job, config, runner)

        } else {
          val enuple = (job, config, runner, filename)
          sender ! FileRequest(filename)
          waitingJobs += enuple
          
        }
      }
    case "TryAgain" =>
      masterSelection match {
        case Some(ms) => ms ! Register
        case None => 
          println("No master?!")
          context.system.shutdown
      }
    case Master(m) =>
      masterSelection = Some(m)
      m ! Register
    case RegisterOk =>
      context watch sender
      ticker.cancel
    case NoJob =>
      running = false
    case Terminated(actor) =>
      if(!router.routees.contains(actor)) {
        ticker = context.system.scheduler.schedule(1000 millis, 1000 millis, self, "TryAgain")
      }
      else addSpot
  }
}