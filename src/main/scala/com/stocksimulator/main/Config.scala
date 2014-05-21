package com.stocksimulator.main

import argonaut._
import Argonaut._
import scalaz._
import Scalaz._
import com.stocksimulator.ff._
import com.stocksimulator.reuters._
import com.stocksimulator.abs._
import com.stocksimulator.output._
import com.mongodb.casbah.Imports._
import com.stocksimulator.java_loader.JavaStdStrategy
import com.stocksimulator.debug._
import com.stocksimulator.debug.LogNames._
import ConfigurationModule._

case class SaveMongo(collNamePre: String, id: String, sId: String)(implicit mConf: MongoConfig) extends SaveResult {
  private val collName = collNamePre.replace('$', '_')
   def withMongoObject(mongoData: DBObject) = {
    val host = mConf.host
    val port = mConf.port
    val connection = MongoClient(host, port)
    val db = connection(mConf.coll)
      val ignoreDates = List("31/12/1969", "01/01/1970")
      val coll = db(collName)
      val inputStr = mongoData("inputStr").asInstanceOf[String]
      val date = mongoData("date").asInstanceOf[String]
      val pnl = mongoData("PNL").asInstanceOf[Double]
     if(!ignoreDates.exists(_==date) && pnl != 0) {
      val mobj = MongoDBObject("inputStr" -> inputStr, "date" -> date)
      val find = coll.findOne(mobj)
      find match {
        case Some(obj) => {}
        case None => coll.insert(mongoData)
      }
     } else this.log("Bogus save attempted..")
     connection.close()
    }
    def apply(result: Result): Unit = {

      for ((a, b) <- result) {
        val mongoFormat = new MongoOutput(a, b, id, sId)
        withMongoObject(mongoFormat.output)
      }

    }
  }
object ConfigurationModule {
  import com.stocksimulator.main.CurrentMongoConfig
  import com.stocksimulator.main.BSTypeClass.BSLike
  import com.stocksimulator.main.BSTypeClass._
  implicit val config = CurrentMongoConfig
  

  
  trait ConfigurationTransform[T]
  trait MongoConfig {
    val host: String
    val port: Int
    val coll: String
  }
  type Result = List[(Parameters, Parameters)]
  
  trait ConfigurationLoadMethod {
    def load(filename: String): Option[Configuration]
  }

  abstract class RunConfiguration[U: BSLike](implicit ev: BSLike[U]) {
    
    def apply(conf: Configuration): Result
    def BSSGen(conf: Configuration, date: String, file: String): U
    val remote = false
    val filename = ""
    protected def runBSS(conf: Configuration) = {
      val runs = for ((date, file) <- conf.loadedCSVs(remote, filename)) yield {
        val bss = BSSGen(conf, date, file)
        ev.bootstrap(bss)
      }
      runs.flatten
    }
  }

  trait RunFromFile {
    def apply(file: String)
  }

  object RunFromFileJson extends RunFromFile {
    def apply(json: String) = {
      val config = ConfigurationLoad(ConfigurationLoadJson)(json)
      config match {
        case Some(c) => RunConfigurationStd(c)
        case None => this.log("Erro ao carregar json...")
      }

    }
  }

  trait SaveResult {
    def apply(result: Result): Unit
  }


  trait MongoDataSave {
    protected def saveToMongo(result: Result, conf: Configuration) 
  }
  
  trait MongoStdSave extends MongoDataSave  {
    protected def saveToMongo(result: Result, conf: Configuration) = {
      val symbols = conf.symbols mkString "."
      val saveMongo = new SaveMongo(conf.name + "_" + symbols, conf.name, conf.name)
      saveMongo(result)
    }
  }
  
  object RunConfigurationStd extends RunConfiguration[JavaBSSet] with MongoStdSave {
    
    def BSSGen(conf: Configuration, date: String, file: String) = new JavaBSSet(conf, date, file)
    def apply(conf: Configuration) = {
      val result = runBSS(conf)
      saveToMongo(result, conf)
      result
    }
  }

  case class RunConfigurationRemote(javafs: String, filter: Option[List[String]] = None, override val filename:String) extends RunConfiguration[RemoteJavaBSSet] {
    override val remote = true
    def BSSGen(conf: Configuration, date: String, file: String) = new RemoteJavaBSSet(conf, date, file, javafs, filter)
    def apply(conf: Configuration) = runBSS(conf)

  }

  case object ConfigurationLoadJson extends ConfigurationLoadMethod {
    def load(filename: String): Option[Configuration] = {
      val getFile = new FiletoStringIO(filename)
      apply(getFile.run)
    }

    def apply(data: String) = data.decodeOption[Configuration]
  }

  case class ConfigurationLoad(method: ConfigurationLoadMethod) {

    def apply(filename: String) = {
      method.load(filename)
      
    }
  }

  case class Configuration(symbols: List[String], dates: List[String], from: String, to: String, name: String, bookOrder: Int, workers: Int, javaFilename: String, parameters: List[ConfigParam], stringParam:Option[List[StringParam]]) {
    def filterName = Configuration(symbols, dates, from, to, name.replace('_', '-'), bookOrder: Int, workers: Int, javaFilename: String, parameters: List[ConfigParam], stringParam)
    def loadedCSVs(remote:Boolean = false, filename: String = ""): List[(String, String)] = {
      val IOs = this.dates.map {
        date =>
          (date, new IOSideEffect[String] {
            def run: String = {
              if(!remote) FileManager.downloadReuters(symbols.toArray, date) else filename
            }
          })
      }
      IOs.map {
        case (date, side) => (date, side.run)
      }

    }
  }

  case class ConfigParam(base: Double, to: Option[Double], by: Option[Double], name: String)
  case class StringParam(name: String, params: List[String])
  
  object StringParam {
    implicit def paramCodec: CodecJson[StringParam] = casecodec2(StringParam.apply, StringParam.unapply)("name", "params")
  }
  object ConfigParam {
    implicit def paramCodec: CodecJson[ConfigParam] = casecodec4(ConfigParam.apply, ConfigParam.unapply)("base", "to", "by", "name")
  }

  object Configuration {
    implicit def confParam: CodecJson[Configuration] = casecodec10(Configuration.apply, Configuration.unapply)("symbols", "dates", "from", "to", "name", "bookOrder", "workers", "javaFilename", "parameters", "stringParam")

  }
}