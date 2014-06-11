
package com.stocksimulator.aws
import akka.actor._
import com.stocksimulator.parallel.ParCommon
import akka.routing.ActorRefRoutee
import akka.routing.Router
import akka.routing.RoundRobinRoutingLogic
import scalaz._
import Scalaz._
import com.stocksimulator.debug.LogNames._

abstract class Service(name: String) {
    val config = ParCommon.config
	val system = ActorSystem(name, config)
  def run() = {
	
	  val actor = actorGen(system)
  }
  protected def actorGen(system: ActorSystem): ActorRef
}

trait DynamoDBUser {
  import ServicesManagement._
  implicit def dynamo = SL_DynamoDB()
}

trait DynamobDBUserWithTable extends DynamoDBUser {
  val tableName: String
  val tableOption = dynamo.table(tableName)
}

trait S3UserWithBucket {
  import ServicesManagement._
  val bucketName: String
  implicit def s3 = SL_S3()
  def bucketOption = s3.bucket(bucketName)
  def s3FileExists(filename: String):Boolean = {
    bucketOption match {
      case Some(bucket) => //bucket.keys().toList.exists(_ == filename)
      bucket.get(filename).map(f => true).getOrElse(false)
      case None => false
    }
  }
}

trait SQSUser {
  import ServicesManagement._
  implicit def sqs = SL_SQS()
}

trait SQSReceiveQueue extends SQSUser {
  import Result._
  val receiveQueue: String
  this.log(s"Queue Name: $receiveQueue")
  def queueOption = sqs.queue(receiveQueue)
  def tryQueue = queueOption.result("Queue not found")
  def removeMessage(m: awscala.sqs.Message) = {
    queueOption match {
      case Some(queue) => queue.remove(m)
      case None => println("Error: Queue not available")
    }
 }
  def receiveFromQueue[T](count: Int)(fun: Seq[awscala.sqs.Message] => T): Result[T] = {
    val option = queueOption.map {
      queue =>
        val messageSequence = sqs.receiveMessage(queue, count)
        fun(messageSequence)
    }

    option match { 
      case Some(res) => ok(res)
      case None => err("Queue inexistent")
    }

  }

  def receiveFromQueueAndMap[T](count: Int)(fun: awscala.sqs.Message => T) = {

    receiveFromQueue(count) {
      messages =>
        this.log("Receiving... ")
        this.log(messages)
        def removeMessage(e: awscala.sqs.Message) = {
          sqs.delete(e)
        }
        messages.map(fun)
    }
  }

}

trait SQSSendQueue extends SQSUser {
  import Result._
  val sendQueue: String
  def sendQueueOption = sqs.queue(sendQueue)
  val tryOutputQueue = sendQueueOption.result("Output Queue not found!")
}

trait SQSSendReceiveQueue extends SQSReceiveQueue with SQSSendQueue

case class ExtraQueue(val sendQueue: String) extends SQSSendQueue {
  
  def sendMessage(str: String) {
    for(sendQueue <- sendQueueOption) {
      sendQueue.add(str)
    }
  }
}

trait SecondaryActors {
  import scala.concurrent.duration._
  import context.dispatcher
  import akka.actor.OneForOneStrategy
  import akka.actor.SupervisorStrategy._
  val workers: Int
  val context: ActorContext
    def secondaryActor: ActorRef
    val router = {
    val routees = Vector.fill(workers) {
      val r = secondaryActor
      context watch r
      ActorRefRoutee(r)
    }
    Router(RoundRobinRoutingLogic(), routees)
  }
  
  val errorQueue: ExtraQueue
   val supervisorStrategy =
    OneForOneStrategy(maxNrOfRetries = 3, withinTimeRange = 1 minute) {
      case e: Exception => 
        errorQueue.sendMessage(e.getMessage())
        Restart
    }
}

abstract class PrimaryServiceActor(errorQueueName: String) extends Actor {
  import scala.concurrent.duration._
  import context.dispatcher
  val callName = "checkQueue" 
  val firstTick = 10 
  val nextTicks = 20000 
  val ticker = context.system.scheduler.schedule(firstTick millis, nextTicks millis, self, callName)
  
  protected def errorQueue = ExtraQueue(errorQueueName)
 


}