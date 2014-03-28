package com.stocksimulator.reuters

import com.stocksimulator.abs._
import com.stocksimulator.debug._
import org.joda.time._
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
    val gridfs = gridFS
    val file = new FileInputStream(filename)
    val id = gridfs(file) {f =>
      f.filename = filename
    }
  }
  
  def openFile(filename: String):Option[String] = {
	  val gridfs = gridFS
	  val maybeFile = gridfs.findOne(filename)
	
	  maybeFile match {
	    case Some(file) =>
	      val byteArrayOutputStream = new java.io.ByteArrayOutputStream()
	      val str = file.source.mkString
	      Some(str)
	    case None => None
	      
	  }
  }
  def apply(host: String, port: Int) = {
    mongoClient match {
      case Some(m) => m
      case None => {
        val connection = new ServerAddress(host, port)

        val tryLocalConn = new ServerAddress("127.0.0.1", 27017)
        val mC = MongoClient(List( connection))
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
  val dbColName = MongoClientSingleton.dbName
  def getOffset(host: String, port: Int, dbName: String) = {
    val it = ReutersMongoLoad(host, port, dbName)

    if (it.size > 0) {
      val dataElem = SharedMongo.getList(it.drop(1).next)
      dataElem(3).toInt
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
    iterator
  }

  def apply(host: String, port: Int, dbName: String, from: DateTime, to: DateTime) = {
    val coll = preApply(host, port, dbName)
    val iterator = coll.find($and("datetime" $gte from.getMillis(), "datetime" $lte to.getMillis()))
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
    iterator
  }
}

object SharedMongo {
  def getList(a: DBObject) = {

    val ret = (a.get("data")).asInstanceOf[com.mongodb.BasicDBList].toArray().toList.asInstanceOf[List[String]]
    //Log(ret)
    ret
  }
}

class SharedMongo(config: MongoConfig, hourFilter: Filter = EmptyFilter) {
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
  val haveData = ReutersMongoLoad.findOne(config.hostname, config.port, config.dbname).size > 0
  lazy val raw = {
    Log("Loading MongoDB raw data...")

    val load = loadData()
    Log(load.size)
    if (load.size <= 0) {
      Log("Database not found, making a new one")
      ReutersMongoSave(config)
      for (a <- loadData().toStream; val b = SharedMongo.getList(a)) yield b
    } else {
      for (a <- load.toStream; val b = SharedMongo.getList(a)) yield b
    }
    
  }
  
  lazy val loaded = raw.toArray
 
}

class ReutersMongoFeed(knownInstruments: Set[Stock], config: MongoConfig) extends ReutersCsvFeed("", knownInstruments) {
  val shMongo = new SharedMongo(config)
  override def getMeRaw() = shMongo.raw.toArray
}

class ReutersSharedMongoFeed(knownInstruments: Set[Stock], shMongo: SharedMongo) extends ReutersCsvFeed("", knownInstruments) {


  override def getMeRaw() = {
   val myCopy = shMongo.loaded
    myCopy//shMongo.raw
  }
}

