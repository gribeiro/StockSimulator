package com.stocksimulator.reuters

import com.mongodb.casbah.Imports._
import com.stocksimulator.abs.Stock
import com.mongodb.casbah.Imports._
import org.joda.time._
import com.stocksimulator.debug._
import com.stocksimulator.parallel._
import java.io._
import org.joda.time.format.DateTimeFormat
import scala.io._
import scala.collection.mutable.ArrayBuffer
import scala.concurrent._
import ExecutionContext.Implicits.global
import scala.concurrent.duration.Duration
object ReutersMongoSave {
  val dbColName = MongoClientSingleton.dbName
  def openFile(f: String) = {
    println(f)

  }

  def createMongoObj(list: Array[String]) = {
    val vect = list.toVector
    val dateFormat = ReutersCommon.dateFormat
    val iCurStr = List(vect(1), vect(2)) mkString (" ")
    val iCurrent = DateTime.parse(iCurStr, dateFormat).getMillis()
    MongoDBObject("datetime" -> iCurrent, "data" -> vect)

  }

  def apply(host: String, port: Int, filename: String, dbName: String): Unit = {
    val mongoClient = MongoClient(host, port)
    val cDB = mongoClient(dbColName)
    val coll = cDB(dbName)
    val lines = Source.fromFile(filename, "utf-8").getLines
    //val size = lines.length
    // val meaningfullLines = lines.drop(1)
    //val splitted = meaningfullLines.map { s => s.split(",", -1) }
    //val rawInfo = splitted.toStream
    //coll.drop()
    var i = 0
    lines.drop(1)
    val futures = ArrayBuffer.empty[Future[Boolean]]
    while (lines.hasNext) {
     
      val buffer = new ArrayBuffer[(Int, String)]
      while (lines.hasNext && buffer.size < 100000) {
        val teste = lines.next
        val par = (i, teste)
        buffer += par
        i += 1
      }
     // futures += future {
        buffer.par.foreach {
          elem =>
            val index = elem._1
            val content = elem._2.split(",", -1)
            val mObj = MongoDBObject("key" -> index) ++ createMongoObj(content)
            coll.insert(mObj)
        }
    /*    true
      }*/

    }
 /* futures.foreach {
     fut =>
        Await.ready(fut, Duration.Inf)
   }*/
    /* for(i <- 1 to size/1000) {
      println(i)
      val until =  1000*i
      val arr = new Array[Array[String]](1000)
      val chunck = rawInfo.copyToArray(arr, 0, 1000)
      val siz = rawInfo(10)
      println(s"$siz")
	/* chunck.foreach {      
	      elem => 
	      //val builder = coll.initializeUnorderedBulkOperation  
	      val mObj = MongoDBObject("key" -> rawInfo.indexOf(elem)) ++ createMongoObj(elem)
	      coll.insert(mObj)
	    }*/
      seek = 1000*i
    }
*/
    coll.createIndex(MongoDBObject("key" -> 1, "datetime" -> 1))
    
    Log("Finished...")
    mongoClient.close
  }

  def apply(config: MongoConfig): Unit = {
    apply(config.hostname, config.port, config.filename, config.dbname)
  }
}


