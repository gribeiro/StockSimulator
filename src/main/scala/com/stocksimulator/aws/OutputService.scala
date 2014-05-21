package com.stocksimulator.aws
import com.stocksimulator.main._
import argonaut._
import Argonaut._
import scalaz._
import Scalaz._
import akka.actor._
import akka.routing._
import com.stocksimulator.debug.LogNames._
import com.stocksimulator.debug._
import com.mongodb.casbah.commons.MongoDBObject
import com.mongodb.BasicDBObject
import awscala.dynamodbv2.AttributeType
import awscala.dynamodbv2.LocalSecondaryIndex
import awscala.dynamodbv2.KeySchema
import awscala.dynamodbv2.KeyType
import awscala.dynamodbv2.Projection

class OutputService extends Service("OutputService") {
  self: ConfigComponent =>
  val receiveQueue = self.queueNames.outputInputQueue
  val errorQueueName = self.queueNames.errorQueueName
  val bucketName = self.queueNames.bucketName
  def actorGen(system: ActorSystem) = system.actorOf(Props(classOf[OutputActor], receiveQueue, bucketName, errorQueueName))
}

case class OutputFormated(pnl: Double, sortino: Double, sharpe: Double, orders: String)
object OutputFormated {
  implicit def paramCodec: CodecJson[OutputFormated] = casecodec4(OutputFormated.apply, OutputFormated.unapply)("pnl", "sortino", "sharpe", "orders")
  def load(s: String) = s.decodeOption[OutputFormated]
}

class OutputActor(val receiveQueue: String, val bucketName: String, errorQueue: String) extends PrimaryServiceActor(errorQueue) with SQSReceiveQueue with S3UserWithBucket {
  import org.apache.commons.codec.binary._
  import com.stocksimulator.remote.ByteArrayToObject
  import awscala.dynamodbv2.TableMeta

  def receive = {

    case "checkQueue" =>
      this.log("Bidding")
      for (bucket <- bucketOption) {
        val receiveOption = receiveFromQueueAndMap(10) {
          p =>
            val idAndPathOption = ResultPath.load(p.body)

            for(idAndPath <- idAndPathOption; s3Obj <- bucket.get(idAndPath.path)) {
            val datBA = new String(org.apache.commons.io.IOUtils.toByteArray(s3Obj.content).map(_.toChar))
            val body = datBA.map(_.toByte).toArray
            val decoded = Base64.decodeBase64(body)
            val mongo = ByteArrayToObject[BasicDBObject](decoded)

            val pnl = mongo.get("PNL").asInstanceOf[Double]
            val sortino = mongo.get("sortino").asInstanceOf[Double]
            this.log(idAndPath.id + ": " + pnl.toString)
            val sharpe = mongo.get("sharpe").asInstanceOf[Double]
            val id = mongo.get("sID").asInstanceOf[String]
            val mongoDB = SaveMongo(idAndPath.id, id, id)
            mongoDB.withMongoObject(mongo)
            val orders = mongo.get("Orders")
            this.log("Saving data to db...")
            removeMessage(p)
           }
        }
      }
  }
}