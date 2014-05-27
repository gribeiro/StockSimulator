package com.stocksimulator.aws

import com.stocksimulator.main.ConfigurationModule._
import com.stocksimulator.main.VarParam
import argonaut._
import Argonaut._
import scalaz._
import Scalaz._
import akka.actor._

class NewDatService extends Service("newDatService") {
  self: ConfigComponent =>
  val receiveQueue = self.queueNames.reutersInputQueue
  val sendQueue = self.queueNames.runnerInputQueue
  val bucketName = self.queueNames.bucketName
  val errorQueueName = self.queueNames.errorQueueName

  def actorGen(system: ActorSystem) = {
    system.actorOf(Props(classOf[NewDatActor], receiveQueue, sendQueue, bucketName, errorQueueName))
  }
}

class NewDatActor(_receiveQueue: String, _sendQueue: String, _bucketName: String, errorQueueName: String) extends PreprocessActor(_receiveQueue, _sendQueue, _bucketName, errorQueueName) {
  import com.stocksimulator.reuters.FileManager

  def preprocess(id: String, days: List[String], symbols: List[String], param: List[ConfigParam], queue: awscala.sqs.Queue, date: String, bucket: awscala.s3.Bucket, filename: String, crossName: String, addJob: => Unit): NextAction = {
    for (filenameR <- FileManager.downloadReutersOption(symbols.toArray, date)) {
     val file = FileManager.datFileIO(filenameR)
     val teste = bucket.put(crossName, file)
     addJob
    }
    EndNote
  }

}