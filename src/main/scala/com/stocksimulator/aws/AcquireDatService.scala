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
class AcquireDatService extends Service("acquireDatService") {
  self: ConfigComponent =>
  val receiveQueue = self.queueNames.preprocessorInputQueue
  val sendQueue = self.queueNames.runnerInputQueue
  val bucketName = self.queueNames.bucketName
  val errorQueueName = self.queueNames.errorQueueName
  def actorGen(system: ActorSystem) = {
    system.actorOf(Props(classOf[AcquireDatActor], receiveQueue, sendQueue, bucketName, errorQueueName))
  }
}

case class ProcessDat(info: PreProcessInfo, a: awscala.sqs.Message)

case class SendWorkInfo(wInfo: WorkInfo, message: awscala.sqs.Message)

case class WorkInfo(id: String, day: String, symbols: List[String], param: String, dat: String)

object WorkInfo {
  implicit def paramCodec: CodecJson[WorkInfo] = casecodec5(WorkInfo.apply, WorkInfo.unapply)("id", "day", "symbols", "param", "dat")
  def load(s: String) = s.decodeOption[WorkInfo]
}

class AcquireDatActor(val receiveQueue: String, val sendQueue: String, val bucketName: String, errorQueue: String) extends PrimaryServiceActor(errorQueue) with SQSSendReceiveQueue with S3UserWithBucket {
  import scala.concurrent.duration._
  import context.dispatcher

  def receive = {
    case "checkQueue" =>
      this.log("Bidding")
      val receiveOption = receiveFromQueueAndMap(1) {
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
                  queue <- sendQueueOption
                } {
                  this.log("Getting " + date + " file")
                  val filename = FileManager.generatedFilename(symbols.toArray, date)
                  val crossName = "data/" + filename
                  def addJob = {
                    this.log("Making new job...")
                    VarParam(param).foreach {
                      p =>
                        val nWork = WorkInfo(id, date, symbols, p.inputStr, FileManager.datExtension(filename))
                        val newWork = nWork.asJson.toString
                        queue.add(newWork)
                    }
                  }


                  val fileExists = bucket.keys().toList.exists(_ == crossName)
                  if (!fileExists) {
                    
                    for (filenameR <- FileManager.downloadReutersOption(symbols.toArray, date)) {
                      val file = FileManager.datFileIO(filenameR)
                     val teste= bucket.put(crossName, file)
                      
                      addJob
                    }
                  }
                  else {
                    addJob
                  }

                }
            }
            this.log("Removendo mensagem:" + p.id)
            removeMessage(p)
            this.log("Mensagem supostamente removida")
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
