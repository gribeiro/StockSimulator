package com.stocksimulator.reuters

import com.stocksimulator.abs._
import com.stocksimulator.debug._
import java.io.File
import scala.annotation.tailrec
import scala.collection.Parallel
import scala.collection.parallel._
import scala.concurrent.ExecutionContext.Implicits.global
import org.joda.time.format.DateTimeFormat
import com.mongodb.casbah.Imports._
import scala.{ None, Some, Option }
import com.mongodb.MongoException
import scala.collection.mutable.ArrayBuffer
import java.io.FileInputStream
import java.io.OutputStream
case class MongoConfig(hostname: String, port: Int, dbname: String, filename: String = "")

object MongoClientSingleton {
  import com.mongodb.casbah.Imports._
  import com.mongodb.casbah.gridfs.Imports._
  val dbName = "sRemote"
  var mongoClient: Option[MongoClient] = None
  
  
  private def gridFS = {
     val client = mongoClient.get
    GridFS(client("files"))
  }
  def saveFile(filename: String) = {
    println(s"Saving $filename to gridFS")
    val gridfs = gridFS
    val file = new FileInputStream(filename)
    val id = gridfs(file) {f =>
      f.filename = filename
    }
  }
  
  def openFile(filename: String):Option[String] = {
	  val gridfs = gridFS
	  val fileList = gridfs.find(filename)
	  if(fileList.size > 0) {
	    val file = fileList.maxBy(f => f.getUploadDate())
	    println("File " + filename + " found in database, last date: " + file.getUploadDate())
	    val byteArrayOutputStream = new java.io.ByteArrayOutputStream()
	    file.writeTo(byteArrayOutputStream)
	    val byteArray =  byteArrayOutputStream.toByteArray()
	    Some(new String(byteArray))
	  }  else None

  }
  def apply(host: String, port: Int) = {
    mongoClient match {
      case Some(m) => m
      case None => {
        val connection = new ServerAddress(host, port)
        
        //val tryLocalConn = new ServerAddress("127.0.0.1", 27017)
        val mC = MongoClient(host, port)
        mongoClient = Some(mC)
        mC
      }
    }
  }
  
  def apply(mConfig: MongoConfig):MongoClient = {
    apply(mConfig.hostname, mConfig.port)
  }
}

object ReutersMongoLoad {
  import org.joda.time._
  val dbColName = MongoClientSingleton.dbName
  def getOffset(host: String, port: Int, dbName: String) = {
    val it = ReutersMongoLoad(host, port, dbName)

    if (it.size > 0) {
      val dataElem = SharedMongo.getList(it.drop(1).next)
      dataElem._2(3).toInt
    } else 0

  }
  
  def preApply(host: String, port: Int, dbName: String) = {
    val mongoClient = MongoClientSingleton(host, port)
    val cDB = mongoClient(dbColName)
    cDB(dbName)
  }
  
  def findOne(host: String, port: Int, dbName: String) = {
    val coll = preApply(host, port, dbName)
    coll.findOne()
  }
  def apply(host: String, port: Int, dbName: String) = {
    val coll = preApply(host, port, dbName)
    val iterator = coll.find()
    iterator.sort(MongoDBObject("key" -> 1))
    iterator
  }

  def apply(host: String, port: Int, dbName: String, from: DateTime, to: DateTime) = {
    val coll = preApply(host, port, dbName)
    val iterator = coll.find($and("datetime" $gte from.getMillis(), "datetime" $lte to.getMillis()))
    iterator.sort(MongoDBObject("key" -> 1))
    
    iterator
  }
  
    def apply(host: String, port: Int, dbName: String, from: DateTime, to: DateTime, excluded: Array[HourFilter]) = {
    val coll = preApply(host, port, dbName)
    val firstCondition = $and("datetime" $gte from.getMillis(), "datetime" $lte to.getMillis())
    val condition = excluded.foldLeft(firstCondition) {
      (cond, filter) =>
        val nCondition = $or("datetime" $lte filter.from.getMillis(), "datetime" $gte filter.to.getMillis())
        $and(cond, nCondition)
    }
    Log(condition)
    val iterator = coll.find(condition)
    //Log(iterator)
    iterator.sort(MongoDBObject("key" -> 1))
    iterator
  }
}

object SharedMongo {
  def getList(a: DBObject) = {
    val key = a.get("key").asInstanceOf[Int]
    val ret = (a.get("data")).asInstanceOf[com.mongodb.BasicDBList].toArray().toList.asInstanceOf[List[String]]
    val arrBuffer = ArrayBuffer.empty[String]
    ret.foreach { f =>
      arrBuffer += f
    }
    //Log(ret)
    key -> arrBuffer.toArray
  }
}

class SharedMongo(config: MongoConfig, date: String, hourFilter: Filter = EmptyFilter ) {
  import org.joda.time._
  val dateFormat = ReutersCommon.dateFormat
  private val now = new DateTime(DateTimeZone.getDefault())
  private val localOffset = now.getZone().getStandardOffset(0) / 1000 / 60 / 60
  
  private def hourCheck(data: List[String]) = {
    hourFilter match {
      case EmptyFilter => true
      case HourFilter(from, to) => {
        val iCurStr = List(data(1), data(2)) mkString (" ")
        val iCurrent = DateTime.parse(iCurStr, dateFormat)
        iCurrent.isAfter(from) && iCurrent.isBefore(to)
      }
    }
  }


  private def hourFilterCorrect(hFilter: HourFilter):HourFilter = {
    val from = hFilter.from
    val to = hFilter.to
        val fileOffset = ReutersMongoLoad.getOffset(config.hostname, config.port, config.dbname)
        val (correctedFrom, correctedTo) = if (fileOffset == -2) {
          val diff = fileOffset - localOffset
          (from.minusHours(diff), to.minusHours(diff))
        } else (from, to)
      HourFilter(correctedFrom, correctedTo)
  }
  
  def loadData() = {
    hourFilter match {
      case h: HourFilter =>
      	val correct = hourFilterCorrect(h)
        ReutersMongoLoad(config.hostname, config.port, config.dbname, correct.from, correct.to)
      case ExtendedHourFilter(hourFilter, excluded) =>
        val correct = hourFilterCorrect(hourFilter)
        val exclude = excluded.map(hF => hourFilterCorrect(hF))
        ReutersMongoLoad(config.hostname, config.port, config.dbname, correct.from, correct.to, exclude)
      case _ =>
        ReutersMongoLoad(config.hostname, config.port, config.dbname)
    }
  }

  lazy val size = loadData().size
  lazy val haveData = ReutersMongoLoad.findOne(config.hostname, config.port, config.dbname).size > 0
  def raw = {
    Log("Loading MongoDB raw data...")

    val load = loadData()
    Log(load.size)
    if (load.size <= 0) {
      Log("Database not found, making a new one")
      ReutersMongoSave(config, date)
      val stream =  loadData().toStream
      for (a <- stream; val b = SharedMongo.getList(a)) yield b
    } else {
      val stream = load.toStream
      for (a <- stream; val b = SharedMongo.getList(a)) yield b
    }
    
  }
  
  def loaded = raw.toArray
 
}

class ReutersMongoFeed(knownInstruments: Set[Stock], config: MongoConfig, date: String) extends ReutersCsvFeed("", knownInstruments) {
  val shMongo = new SharedMongo(config, date)
  val sorted =  shMongo.raw.sortBy {
    case (key, value) => key
  }
  val valueArr = sorted.map {
    case (key, value) => value
  }
  override def getMeRaw() = valueArr.toArray
}

class ReutersSharedMongoFeed(knownInstruments: Set[Stock], shMongo: SharedMongo) extends ReutersCsvFeed("", knownInstruments) {


  override def getMeRaw() = {
   val myCopy = shMongo.loaded
    val sorted =  myCopy.sortBy {
    case (key, value) => key
  }
  val valueArr = sorted.map {
    case (key, value) => value
  }
  valueArr.toArray
  }
}

