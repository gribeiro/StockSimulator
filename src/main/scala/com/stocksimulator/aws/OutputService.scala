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
import com.stocksimulator.output.DataTransformModule._
import com.stocksimulator.output.MySqlModule._
import com.mongodb.casbah.commons.Imports.BasicDBList
import com.fasterxml.jackson.databind.module.SimpleModule

import scala.reflect._
import scala.reflect.api._

abstract class OutputService[T: ClassTag] extends Service("OutputService") {
  self: ConfigComponent =>
  //val receiveQueue = self.queueNames.outputInputQueue
  val receiveQueue: String
  val errorQueueName = self.queueNames.errorQueueName
  val bucketName = self.bucketNames.bucketName
  
  
  private def multiActorGen(system: ActorSystem) = system.actorOf(Props(classTag[T].runtimeClass, receiveQueue, bucketName, errorQueueName))
  def actorGen(system: ActorSystem) = multiActorGen(system)
}

class MongoOutputService extends OutputService[OutputActorMongo] {
  self: ConfigComponent =>
   val receiveQueue = self.queueNames.outputInputQueue
}

class MySQLOutputService extends OutputService[OutputActorMySQL] {
	self: ConfigComponent =>
	  val receiveQueue = self.queueNames.mysqlInputQueue
}

case class OutputFormated(pnl: Double, sortino: Double, sharpe: Double, orders: String)
object OutputFormated {
  implicit def paramCodec: CodecJson[OutputFormated] = casecodec4(OutputFormated.apply, OutputFormated.unapply)("pnl", "sortino", "sharpe", "orders")
  def load(s: String) = s.decodeOption[OutputFormated]
}
import com.stocksimulator.module.ModuleMonoid._

trait SaveMethodComponent {
  
  def save(resultPath: ResultPath, mongo: BasicDBObject): ComposableModule
  abstract class SaveMethod(val resultPath: ResultPath, val mongo: BasicDBObject) extends ComposableModule {
  
    val pnl = mongo.get("PNL").asInstanceOf[Double]
    val sortino = mongo.get("sortino").asInstanceOf[Double]
    val sharpe = mongo.get("sharpe").asInstanceOf[Double]
    val date = mongo.get("date").asInstanceOf[String]
    val paramStr = mongo.get("inputStr").asInstanceOf[String]
    val id = mongo.get("sID").asInstanceOf[String]
    val otherId = resultPath.id
    def run: Unit
  }

}

trait MongoSaveMethod extends SaveMethodComponent {

  def save(rP: ResultPath, m: BasicDBObject) = new SaveMethod(rP, m) {
    def run: Unit = {
      val mongoDB = SaveMongo(otherId, id, id)
      mongoDB.withMongoObject(mongo)
    }
  }
}

trait MySQLSaveMethod extends SaveMethodComponent {
  import com.stocksimulator.output.MongoToSql._
  def save(rP: ResultPath, m: BasicDBObject) = new SaveMethod(rP, m) {
    def run: Unit = {
      println("MySQL save")
      defaultSaveAllTables(resultPath.path, mongo, otherId)
    }
  }
}

trait SumMethods extends SaveMethodComponent {
  val a: SaveMethodComponent
  val b: SaveMethodComponent
  def save(rP: ResultPath, m: BasicDBObject) = a.save(rP, m) |+| b.save(rP,m)
 
}

class OutputActorMongo(receiveQueue: String, bucketName: String, errorQueue: String) extends OutputActor(receiveQueue, bucketName, errorQueue) with MongoSaveMethod
class OutputActorMySQL(receiveQueue: String, bucketName: String, errorQueue: String) extends OutputActor(receiveQueue, bucketName, errorQueue) with MySQLSaveMethod

class OutputActor(val receiveQueue: String, val bucketName: String, errorQueue: String) extends PrimaryServiceActor(errorQueue) with SQSReceiveQueue with S3UserWithBucket {
  localSelf: SaveMethodComponent =>
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

            for (idAndPath <- idAndPathOption; s3Obj <- bucket.get(idAndPath.path)) {
              val mongo = readBase64(s3Obj.content)
              save(idAndPath, mongo).run
              this.log("Saving data to db...")
              removeMessage(p)
            }
        }
      }
  }
}