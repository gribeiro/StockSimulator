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
import scala.concurrent._
class AcquireDatService extends Service("acquireDatService") {
  self: ConfigComponent =>
  val receiveQueue = self.queueNames.preprocessorInputQueue
  val sendQueue = self.queueNames.runnerInputQueue
  val bucketName = self.queueNames.bucketName
  val errorQueueName = self.queueNames.errorQueueName
  val acquireQueueName = self.queueNames.reutersInputQueue
  def actorGen(system: ActorSystem) = {
    system.actorOf(Props(classOf[AcquireDatActor], receiveQueue, sendQueue, bucketName, errorQueueName, acquireQueueName))
  }
}

case class ProcessDat(info: PreProcessInfo, a: awscala.sqs.Message)

case class SendWorkInfo(wInfo: WorkInfo, message: awscala.sqs.Message)

case class WorkInfo(id: String, day: String, symbols: List[String], param: String, dat: String)

object WorkInfo {
  implicit def paramCodec: CodecJson[WorkInfo] = casecodec5(WorkInfo.apply, WorkInfo.unapply)("id", "day", "symbols", "param", "dat")
  def load(s: String) = s.decodeOption[WorkInfo]
}

abstract class PreprocessActor(val receiveQueue: String, val sendQueue: String, val bucketName: String, errorQueue: String) extends PrimaryServiceActor(errorQueue) with SQSSendReceiveQueue with S3UserWithBucket {
  import scala.concurrent.duration._
  import context.dispatcher

  sealed trait NextAction
  object EndNote extends NextAction
  case class Enqueue(q: String) extends NextAction

  def preprocess(id: String, days: List[String], symbols: List[String], param: List[ConfigParam], queue: awscala.sqs.Queue, date: String, bucket: awscala.s3.Bucket, filename: String, crossname: String, addJob: => Unit): NextAction
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
                  val next = preprocess(id, days, symbols, param, queue, date, bucket, filename, crossName, addJob)
                  next match {
                    case Enqueue(q) =>
                      val nPreInfo = PreProcessInfo(id, List(date), symbols, param)
                      val extraQ = ExtraQueue(q)
                      extraQ.sendMessage(nPreInfo.asJson.toString)
                    case EndNote =>
                  }
                }
            }
            this.log("Removendo mensagem:" + p.id)
            removeMessage(p)
          }
      }

  }
}
class AcquireDatActor(_receiveQueue: String, _sendQueue: String, _bucketName: String, _errorQueue: String, acquireQueue: String) extends PreprocessActor(_receiveQueue, _sendQueue, _bucketName, _errorQueue) {
  import scala.concurrent.duration._
  import context.dispatcher
  def preprocess(id: String, days: List[String], symbols: List[String], param: List[ConfigParam], queue: awscala.sqs.Queue, date: String, bucket: awscala.s3.Bucket, filename: String, crossName: String, addJob: => Unit): NextAction = {
    this.log("Checking " + date + " file")

    val fileExists = bucket.keys().toList.exists(_ == crossName)
    if (!fileExists) {
      Enqueue(acquireQueue)
    } else {
      future {
      addJob
      }
      EndNote
    }

  }
}

