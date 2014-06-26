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
import scala.collection.mutable.HashMap

case class ProcessSimulation(workInfo: WorkInfo, p: awscala.sqs.Message)

case class SimulationResult(a: Parameters, b: Parameters, message: awscala.sqs.Message, id: String)

case class ResultPath(id: String, path: String)

object ResultPath {
  implicit def paramCoded: CodecJson[ResultPath] = casecodec2(ResultPath.apply, ResultPath.unapply)("id", "path")
  def load(s: String) = s.decodeOption[ResultPath]

}
/*
object PreProcessInfo {
  implicit def paramCodec: CodecJson[PreProcessInfo] = casecodec5(PreProcessInfo.apply, PreProcessInfo.unapply)("id", "days", "symbols", "param", "stringParam")
  def load(s: String) = s.decodeOption[PreProcessInfo]
}
*/

class RunnerService extends Service("RunnerService") {
  self: ConfigComponent =>
  val receiveQueue = self.queueNames.runnerInputQueue
  val sendQueueNames = List(self.queueNames.outputInputQueue, self.queueNames.mysqlInputQueue)
  val bucketName = self.bucketNames.bucketName
  val errorQueue = self.queueNames.errorQueueName

  def actorGen(system: ActorSystem) = system.actorOf(Props(classOf[RunnerActor], receiveQueue, sendQueueNames, bucketName, errorQueue))
}

object RunnerActor {
  
  val errorProne = (new HashMap[(String, List[String]), Boolean]).withDefaultValue(true)
}
class RunnerActor(val receiveQueue: String, val sendQueueNames: List[String], val bucketName: String, errorQueueName: String) extends PrimaryServiceActor(errorQueueName) with SQSMultiSendReceiveQueue with S3UserWithBucket {
  import com.stocksimulator.output._
  import com.stocksimulator.remote.ObjectToByteArray
  import org.apache.commons.codec.binary._
  import com.stocksimulator.aws.Result._
  import com.stocksimulator.abs.RunningContextModule._
  
  case class SimplePath(path: List[String]) {
    override def toString() = path mkString "/"
  }

  private def sendStr(id: String, resName: String): String = {
    ResultPath(id, resName).asJson.toString
  }
  def putAlreadyCalculated(message: awscala.sqs.Message, nextMessage: String) = {
    for (queue <- queueOption) yield {
      for (outputQs <- sendQueues; outputQ <- outputQs) yield outputQ.add(nextMessage)
      queue.remove(message)
    }
  }
  def simulResult(a: Parameters, b: Parameters, message: awscala.sqs.Message, id: String, nextMessage: String, s3Path: SimplePath) = {
    val output = (new MongoOutput(a, b, id, id)).output
    val binary = ObjectToByteArray(output)
    val stringB64 = new String(Base64.encodeBase64(binary))
    for (queue <- queueOption; bucket <- bucketOption) yield {
      bucket.putObject(s3Path.toString, stringB64.toCharArray().map(_.toByte), new ObjectMetadata)
      for (outputQs <- sendQueues; outputQ <- outputQs) yield outputQ.add(nextMessage)
      queue.remove(message)
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
                  val xyz = RunnerActor.errorProne((day, symbols))
                  if(xyz) {
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
                      val md5 = Utils.md5Hash _
                      this.log(configOneDay)
                      val s3Path = SimplePath(List("result", "1.3.4", md5(javaStr), md5(datFile), md5(param)))
                      val s3ResName = s3Path.toString
                      val nextMessage = sendStr(id, s3Path.toString)

                      try {
                       val tryConnection = if (!s3FileExists(s3ResName)) {
                          val sid = id
                          implicit val preContext = new PreContext {val id = sid}
                          val runner = RunConfigurationRemote(javaStr, Some(List(param)), datFile)
                          val result = runner(configOneDay)
                          this.log("Done..")
                          result.map { res =>
                            simulResult(res._1, res._2, message, id, nextMessage, s3Path)
                          }.reduce {
                            (a, b) => a.map(_ => b)
                          }
                        } else {
                          putAlreadyCalculated(message, nextMessage)
                        }
                       tryConnection match {
                         case Some(_) => this.log("INFO: No AWS connection problems")
                         case None => this.log("ERROR: AWS CONNECTION PROBLEMS")
                       }
                      } catch {
                        case e: ItWouldRunForeverException =>
                          error("Infinite loop would occur, maybe the instruments names are wrong.\n")
                          RunnerActor.errorProne((day, symbols)) = false
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
                      case e: Exception => this.log("Error when closing S3 objects.")
                    }
                    true
                  }
                  worked match {
                    case Some(_) => this.log("No errors.")
                    case None => error("S3 Save exception.")
                  } 
                  } else removeMessage(p)
              } // Here
          }

      }

      receiveOption.gatherErr

  }

}
