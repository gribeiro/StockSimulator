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

case class ProcessSimulation(workInfo: WorkInfo, p: awscala.sqs.Message)

case class SimulationResult(a: Parameters, b: Parameters, message: awscala.sqs.Message, id: String)

class RunnerService(receiveQueue: String, sendQueue: String, bucketName: String, workers: Int) extends Service("RunneService") {
  def actorGen(system: ActorSystem) = system.actorOf(Props(classOf[RunnerActor], receiveQueue, sendQueue, bucketName, workers))
}

class RunnerActor(val receiveQueue: String, val sendQueue: String, val bucketName: String, workers: Int = 1) extends PrimaryServiceActor(workers) with SQSSendReceiveQueue with S3UserWithBucket {
  import com.stocksimulator.output._
  import com.stocksimulator.remote.ObjectToByteArray
  import org.apache.commons.codec.binary._
  import com.stocksimulator.aws.Result._

  def secondaryActor = {
    context.actorOf(Props(classOf[JobWorker], bucketName))
  }
  
  def simulResult(a: Parameters, b: Parameters, message: awscala.sqs.Message, id: String) {
         val output = (new MongoOutput(a, b, id, id)).output
      val binary = ObjectToByteArray(output)
      val stringB64 = new String(Base64.encodeBase64(binary))

      for (queue <- queueOption; outputQ <- sendQueueOption) {
        outputQ.add(stringB64)
        queue.remove(message)
      }
  }
  def receive = {
    case "checkQueue" =>
      this.log("Bidding")
      val receiveOption = receiveFromQueueAndMap (1) {
        p =>
          val maybeConfig = WorkInfo.load(p.body)
          val sendMessage = maybeConfig.map {
            preInfo =>
              val okMessage = ProcessSimulation(preInfo, p)

              okMessage match {
                case ProcessSimulation(WorkInfo(id, day, symbols, param, datFile), message) =>
                  for {
                    bucket <- bucketOption;
                    json <- bucket.get("jobs/" + id + ".json");
                    javafile <- bucket.get("jobs/" + id + ".java");
                    dat <- bucket.get("data/" + datFile)
                  } {
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
                      val result = RunConfigurationRemote(javaStr, Some(List(param)), datFile)(configOneDay)
                      this.log("Done..")
                      result.foreach { res =>
                        simulResult(res._1, res._2, message, id)
                      }
                    }
                  }
              }
          }

      }

      receiveOption.gatherErr

    case SimulationResult(a, b, message, id) =>
      this.log(b)
      val output = (new MongoOutput(a, b, id, id)).output
      val binary = ObjectToByteArray(output)
      val stringB64 = new String(Base64.encodeBase64(binary))

      for (queue <- queueOption; outputQ <- sendQueueOption) {
        outputQ.add(stringB64)
        queue.remove(message)
      }
  }

}

class JobWorker(val bucketName: String) extends Actor {
  def receive = {
    case _ =>
  }
}