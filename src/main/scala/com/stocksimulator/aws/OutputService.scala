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



class OutputService(receiveQueue: String, tableName: String) extends Service("OutputService") {
  def actorGen(system: ActorSystem) = system.actorOf(Props(classOf[OutputActor], receiveQueue, tableName))
}

case class OutputFormated(pnl: Double, sortino: Double, sharpe: Double, orders: String)
object OutputFormated {
  implicit def paramCodec: CodecJson[OutputFormated] = casecodec4(OutputFormated.apply, OutputFormated.unapply)("pnl", "sortino", "sharpe", "orders")
  def load(s: String) = s.decodeOption[OutputFormated]
}



class OutputActor(val receiveQueue: String, val tableName: String) extends PrimaryServiceActor(1) with SQSReceiveQueue {
  import org.apache.commons.codec.binary._
  import com.stocksimulator.remote.ByteArrayToObject
  import awscala.dynamodbv2.TableMeta
  
  
  
  def receive = {

    case "checkQueue" =>
      this.log("Bidding")
      
      val receiveOption = receiveFromQueueAndMap (10) {
        p =>
          val body = p.body.map(_.toByte).toArray
          val decoded = Base64.decodeBase64(body)
          val mongo = ByteArrayToObject[BasicDBObject](decoded)
          
          val pnl = mongo.get("PNL").asInstanceOf[Double]
          val sortino = mongo.get("sortino").asInstanceOf[Double]
          val sharpe = mongo.get("sharpe").asInstanceOf[Double]
          val id = mongo.get("sID").asInstanceOf[String]
          val mongoDB = SaveMongo(id, id, id)
          mongoDB.withMongoObject(mongo)
          val orders = mongo.get("Orders")
           this.log("Saving data to db...")
          removeMessage(p)
         /* for (table <- tableOption) {
           
            val teste = 22.0
          //  table.put(id, "pnl" -> pnl, "sharpe" -> sharpe, "sortino" -> sortino, "orders" -> orders)
            
          }*/

      }
  }
}