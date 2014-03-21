package com.stocksimulator.reuters

import com.mongodb.casbah.Imports._
import com.stocksimulator.abs.Stock
import com.mongodb.casbah.Imports._
import org.joda.time._
import com.stocksimulator.debug._
import com.stocksimulator.parallel._
import java.io._
import org.joda.time.format.DateTimeFormat

object ReutersMongoSave {
val dbColName = MongoClientSingleton.dbName
  def openFile(f: String) = {
    val lines = scala.io.Source.fromFile(f, "utf-8").getLines
    val meaningfullLines = lines.drop(1)
    val splitted = meaningfullLines.map { s => s.split(",", -1) }
    splitted.toArray
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
    val rawInfo = openFile(filename)
    //coll.drop()
    val builder = coll.initializeUnorderedBulkOperation

    val mongos = rawInfo.par.map {
      elem => MongoDBObject("key" -> rawInfo.indexOf(elem)) ++ createMongoObj(elem)
    }

    mongos.toList.foreach {
      mObj =>
        builder.insert(mObj)
        //Log(mObj)
    }

    builder.execute()
    coll.createIndex(MongoDBObject("key" -> 1, "datetime" -> 1))
    Log("Finished...")
    mongoClient.close
  }

  def apply(config: MongoConfig): Unit = {
    apply(config.hostname, config.port, config.filename, config.dbname)
  }
}


