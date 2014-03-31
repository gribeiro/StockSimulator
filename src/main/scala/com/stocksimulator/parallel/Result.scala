package com.stocksimulator.parallel
import com.github.nscala_time.time.Imports._
import com.stocksimulator.reuters._
import com.stocksimulator.abs._
import scala.util.{ Success, Failure }
import scala.util.Failure
import com.stocksimulator.reuters.ReutersMarket
import scala.concurrent._
import com.stocksimulator.debug._
import com.stocksimulator.java.CommonStrategy
import scala.concurrent.ExecutionContext.Implicits.global
import com.stocksimulator.java._
import akka.actor.{ Props, ActorSystem, ActorRef, Actor }
import com.typesafe.config.ConfigFactory
import akka.routing.ActorRefRoutee
import akka.routing.Router
import akka.routing.RoundRobinRoutingLogic
import akka.routing.ActorRefRoutee
import akka.actor.Terminated
import java.io._
import akka.actor.PoisonPill
import com.stocksimulator.fancy_output._
import scala.collection.mutable.ArrayBuffer
import com.mongodb.casbah.Imports._
import com.stocksimulator.main.RBSFactory
import org.joda.time.format.DateTimeFormat

abstract class ResultActor(sId: String) extends Actor {
  def uuid = java.util.UUID.randomUUID.toString
}

trait MongoReceive extends Actor {

  def mongoReceive(conf: MongoConfig)
  override def receive = {
    case spMongoConfig(conf: MongoConfig) => {
      Log("MongoReceive touched...")
      mongoReceive(conf)
    }
  }
}

trait SaveParam extends Actor {

  def saveParameters(a: Parameters, b: Parameters)
  var done: () => Unit = () => {}
  def receive = {
    case spResult(a, b) => {
      Log("SaveParam touched...")
      saveParameters(a, b)
      done = () => {
        sender ! spReportDone
      }
    }

  }
}

object ResultUtils {
  private def fileWriter(filename: String, contents: String) = {
    val fw = new FileWriter(filename, true)
    try {
      fw.write(contents)
    } finally fw.close()
  }
  def writeXML(a: Parameters, b: Parameters, id: String, sId: String) = {
    /*val json = new XMLFormat(a, b, id, sId)
    val output = json.output
    fileWriter("output\\res_" + id + "_" + sId + ".xml", output)*/
  }

  def writerHTML(a: Parameters, b: Parameters, id: String, sId: String) = {
    val toWrite = htmlCommon.header + htmlCommon.input(a) + htmlCommon.orders(b) + htmlCommon.pnl(b) + htmlCommon.footer
    fileWriter("output\\res_" + id + "_" + sId + ".html", toWrite)
  }
  
  def nameFactory(dbName: String) = if (RBSFactory.outputName == "") dbName + "Output" else RBSFactory.outputName
  def checkResult(conf: MongoConfig, sId: String, in: Parameters, date: String):Boolean = {
    //if(date == "N/A") return false
    val mongoClient = MongoClientSingleton(conf.hostname, conf.port)
    val cDB = mongoClient("stockSimulator")
    val coll = cDB(nameFactory(sId))
    val strToFind = in.inputStr
    val dateFormat = DateTimeFormat.forPattern("dd/MM/yyyy")
    val partA = dateFormat.parseDateTime(date)
    val dateStr = (List(partA.dayOfMonth().get(), partA.monthOfYear().get(), partA.year().get()).mkString("/"))
    val query = MongoDBObject("inputStr" -> strToFind, "date" -> dateStr)
    val queryExec = coll.find(query)
 
    if(queryExec.size > 1) {
      coll.remove(query)
      Log("Results Duplicated, refactoring...")
      false
    }
    else {
    	if(queryExec.size > 0) true else false
    }
  }
  def writeMongo(host: String, port: Int, dbName: String, what: MongoDBObject): Unit = {
    val mongoClient = MongoClientSingleton(host, port)
    val cDB = mongoClient("stockSimulator")
    Log(RBSFactory.outputName)
    val coll = cDB(nameFactory(dbName))
    Log("Writing mongo... : " + what)
    coll.insert(what)
  }

  def writeMongo(conf: MongoConfig, what: MongoDBObject): Unit = {
    writeMongo(conf.hostname, conf.port, conf.dbname, what)
  }
}
class FileResultActor(sId: String) extends ResultActor(sId) with SaveParam {

  def saveParameters(a: Parameters, b: Parameters) = {
    val uuid_local = this.uuid
    ResultUtils.writeXML(a, b, uuid_local, sId)
    done()
  }

}

class MongoResultActor(sId: String) extends ResultActor(sId) {
  val in = new ArrayBuffer[Parameters]
  val out = new ArrayBuffer[Parameters]
  var mConf: Option[MongoConfig] = None
  def done() = {
    sender ! spReportDone
  }
  Log("Mongo result actor created...")
  def receive = {
    case spResult(a, b) => {
      saveParameters(a, b)

    }
    case spMongoConfig(conf: MongoConfig) => {
      mConf = Some(conf)
      mongoReceive(conf)
    }
  }

  def saveParameters(a: Parameters, b: Parameters) = {
    in += a
    out += b
    
    mConf match {
      case Some(conf) => mongoReceive(conf)
      case None =>  sender ! spRequestMongoConfig
    }
   
  }

  def mongoReceive(conf: MongoConfig) = {
    for (a <- in; b <- out) {
      val uuid_local = this.uuid
     
      val mongOO = (new MongoOutput(a, b, uuid_local, sId))
      val chk = ResultUtils.checkResult(conf, sId, a, mongOO.date)
      if(!chk) {
    	  val what = mongOO.output
    	Log("Requested to write: " + what)
      ResultUtils.writeMongo(conf, what)
      }
      in -= a
      out -= b
      
    }
    done
  }

}


