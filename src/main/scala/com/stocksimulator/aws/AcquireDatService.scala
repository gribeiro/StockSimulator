package com.stocksimulator.aws

import com.stocksimulator.parallel.ParCommon
import akka.actor._
import akka.routing._
import com.stocksimulator.main.ConfigurationModule._
import com.stocksimulator.reuters.FileManager
import argonaut._
import Argonaut._
import scalaz._
import Scalaz._
import com.stocksimulator.main.VarParam
import com.stocksimulator.debug.LogNames._
import com.stocksimulator.debug._
class AcquireDatService(receiveQueue: String, sendQueue: String, bucketName: String) extends Service("acquireDatService") {
  def actorGen(system: ActorSystem) = {
    system.actorOf(Props(classOf[AcquireDatActor], receiveQueue, sendQueue, bucketName, 1))
  }
}

case class ProcessDat(info: PreProcessInfo, a: awscala.sqs.Message)

case class SendWorkInfo(wInfo: WorkInfo, message: awscala.sqs.Message)

case class WorkInfo(id: String, day: String, symbols: List[String], param: String, dat: String)

object WorkInfo {
  implicit def paramCodec: CodecJson[WorkInfo] = casecodec5(WorkInfo.apply, WorkInfo.unapply)("id", "day", "symbols", "param", "dat")
  def load(s: String) = s.decodeOption[WorkInfo]
}

class AcquireDatActor(val receiveQueue: String, val sendQueue: String, val bucketName: String, workers: Int = 1) extends PrimaryServiceActor(workers) with SQSSendReceiveQueue with S3UserWithBucket {
  import scala.concurrent.duration._
  import context.dispatcher

  def secondaryActor = {
    context.actorOf(Props(classOf[GetFileActor], bucketName))
  }
  def receive = {
    case "checkQueue" =>
      this.log("Bidding")
      val receiveOption = receiveFromQueueAndMap (1) {
        p =>
          val maybeInfo = PreProcessInfo.load(p.body)
          maybeInfo.map { preInfo =>
            this.log(preInfo)
            val okMessage = ProcessDat(preInfo, p)

            okMessage match {
              case ProcessDat(preInfo @ PreProcessInfo(id, days, symbols, param), message) =>
                for {
                  bucket <- bucketOption;
                  date <- days;
                  filename <- FileManager.downloadReutersOption(symbols.toArray, date)
                } {
                  this.log("Getting " + date + " file")

                  val file = FileManager.datFileIO(filename)
                  val crossName = "data/" + FileManager.datExtension(filename)

                  val fileExists = bucket.keys().toList.exists(_ == crossName)
                  if (!fileExists) {
                    bucket.put(crossName, file)
                  }
                  this.log("Making new job...")
                  VarParam(param).foreach {
                    p =>
                      val nWork = WorkInfo(id, date, symbols, p.inputStr, FileManager.datExtension(filename))
                      val newWork = nWork.asJson.toString
                      for (queue <- sendQueueOption) queue.add(newWork)
                  }

                }
            }
            for (receiveQueue <- queueOption) {
              receiveQueue.remove(p)
            }
          }
      }

    case SendWorkInfo(work @ WorkInfo(_, _, _, _, _), message) =>
      this.log(work)

  }

}
class GetFileActor(val bucketName: String) extends Actor {
  import context.dispatcher
  def receive = {
    case _ =>

  }

}
