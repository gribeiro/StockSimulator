package com.stocksimulator.aws

import akka.actor.Actor
import argonaut._
import Argonaut._
import scalaz._
import Scalaz._
import com.stocksimulator.main.ConfigurationModule._
import com.stocksimulator.reuters._
import com.stocksimulator.abs._
import akka.actor.Props
import akka.actor.ActorSystem
import com.stocksimulator.debug.LogNames._
import com.stocksimulator.debug._
import org.jboss.netty.util.HashedWheelTimer
import java.util.concurrent.TimeUnit
import scala.concurrent.duration.Duration
import org.jboss.netty.util.TimerTask
import org.jboss.netty.util.Timeout
import scala.concurrent.Promise
import org.jboss.netty.handler.timeout.TimeoutException
import scala.concurrent.ExecutionContext
import scala.parallel._
import com.amazonaws.services.s3.model.ObjectMetadata

case class ProcessSimulation(workInfo: WorkInfo, p: awscala.sqs.Message)

case class SimulationResult(a: Parameters, b: Parameters, message: awscala.sqs.Message, id: String)

class RunnerService extends Service("RunneService") {
  self: ConfigComponent =>
  val receiveQueue = self.queueNames.runnerInputQueue
  val sendQueue = self.queueNames.outputInputQueue
  val bucketName = self.queueNames.bucketName
  val errorQueue = self.queueNames.errorQueueName
  
  def actorGen(system: ActorSystem) = system.actorOf(Props(classOf[RunnerActor], receiveQueue, sendQueue, bucketName, errorQueue))
}

class RunnerActor(val receiveQueue: String, val sendQueue: String, val bucketName: String, errorQueueName: String) extends PrimaryServiceActor(errorQueueName) with SQSSendReceiveQueue with S3UserWithBucket {
  import com.stocksimulator.output._
  import com.stocksimulator.remote.ObjectToByteArray
  import org.apache.commons.codec.binary._
  import com.stocksimulator.aws.Result._

  def simulResult(a: Parameters, b: Parameters, message: awscala.sqs.Message, id: String) {
    val output = (new MongoOutput(a, b, id, id)).output
    val binary = ObjectToByteArray(output)
    val stringB64 = new String(Base64.encodeBase64(binary))
    val md5Title = "result/" + id + Utils.md5Hash(stringB64)
    val tryAdd = for (queue <- queueOption; outputQ <- sendQueueOption; bucket <- bucketOption) yield {
      bucket.putObject(md5Title, stringB64.toCharArray().map(_.toByte), new ObjectMetadata)
      outputQ.add(md5Title)
      queue.remove(message)
    }
    tryAdd match {
      case Some(_) => this.log("Queues are all ok.")
      case None => this.log("Error! Queues are not ok!!!")
    }
  }
  def receive = {
    case "checkQueue" =>
      this.log("Bidding")
      val receiveOption = receiveFromQueueAndMap(1) {
        p =>

          def error(message: String) = {
            removeMessage(p)
            errorQueue.sendMessage(message + p.toString())
            this.log(message)
          }
          
          val maybeConfig = WorkInfo.load(p.body)
          val sendMessage = maybeConfig.map {
            preInfo =>
              val okMessage = ProcessSimulation(preInfo, p)

              okMessage match {
                case ProcessSimulation(WorkInfo(id, day, symbols, param, datFile), message) =>
                  val worked = for {
                    bucket <- bucketOption;
                    json <- bucket.get("jobs/" + id + ".json");
                    javafile <- bucket.get("jobs/" + id + ".java");
                    dat <- bucket.get("data/" + datFile)
                  } yield {

                    val jsonStr = scala.io.Source.fromInputStream(json.content).getLines().mkString("\n")
                    val javaStr = scala.io.Source.fromInputStream(javafile.content).getLines().mkString("\n")
                    val jsonOption = jsonStr.decodeOption[Configuration]

                    if (!FileManager.fileExists(datFile)) {
                      val datBA = org.apache.commons.io.IOUtils.toByteArray(dat.content)
                      FileManager.saveFromMemory(datBA, datFile)
                    }
                    for (conf <- jsonOption) {
                      this.log(param + " " + conf)
                      val configOneDay = conf.copy(dates = List(day))
                      try {
                        val result = RunConfigurationRemote(javaStr, Some(List(param)), datFile)(configOneDay)
                        this.log("Done..")
                        result.foreach { res =>
                          simulResult(res._1, res._2, message, id)
                        }
                      } catch {
                        case e: ItWouldRunForeverException =>
                          error("Infinite loop would occur, maybe the instruments names are wrong.\n")

                        case e: Exception =>
                          val stTr = e.getStackTrace() mkString "\n"
                          error("Exception occurred: " + e.getMessage() + stTr)

                      }

                    }
                    try {
                    json.close()
                    javafile.close()
                    dat.close()
                  	} catch {
                    case e: Exception =>  this.log("Error when closing S3 objects.")
                  }
                  	true
                  }
                  worked match {
                    case Some(_) => this.log("No errors.")
                    case None => error("S3 Save exception.")
                  }
              }
          }

      }

      receiveOption.gatherErr

  }

}
