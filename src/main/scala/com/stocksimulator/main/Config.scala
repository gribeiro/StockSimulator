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

object ConfigurationModule {
  import com.stocksimulator.main.CurrentMongoConfig
  implicit val config = CurrentMongoConfig
  
  trait MongoConfig {
    val host: String
    val port: Int
    val coll: String
  }
  type Result = List[(Parameters, Parameters)]
  
  trait ConfigurationLoadMethod {
    def load(filename: String): Option[Configuration]
  }

  trait RunConfiguration[U <: Strategy] {
    def apply(conf: Configuration): Result
    def BSSGen(conf: Configuration, date: String, file: String): BSSet[U]
    protected def runBSS(conf: Configuration) = {
      val runs = for ((date, file) <- conf.loadedCSVs) yield {
        val bss = BSSGen(conf, date, file)
        bss.run
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
        case None => new PrintIO("Erro ao carregar json...")
      }

    }
  }

  trait SaveResult {
    def apply(result: Result): Unit
  }
  class SaveMongo(collName: String, id: String, sId: String)(implicit mConf: MongoConfig) extends SaveResult {
    val host = mConf.host
    val port = mConf.port
    val connection = MongoClient(host, port)
    val db = connection(mConf.coll)
    def withMongoObject(mongoData: DBObject) = {

      val coll = db(collName)
      val inputStr = mongoData("inputStr")
      val date = mongoData("date")
      val mobj = MongoDBObject("inputStr" -> inputStr, "date" -> date)
      val find = coll.findOne(mobj)
      find match {
        case Some(obj) => {}
        case None => coll.insert(mongoData)
      }

    }
    def apply(result: Result): Unit = {

      for ((a, b) <- result) {
        val mongoFormat = new MongoOutput(a, b, id, sId)
        withMongoObject(mongoFormat.output)
      }

    }
  }

  trait MongoDataSave {
    protected def saveToMongo(result: Result, conf: Configuration) = {
      val symbols = conf.symbols mkString "."
      val saveMongo = new SaveMongo(conf.name + "_" + symbols, conf.name, conf.name)
      saveMongo(result)
    }
  }
  object RunConfigurationStd extends RunConfiguration[JavaStdStrategy] {
    def BSSGen(conf: Configuration, date: String, file: String) = new JavaBSSet(conf, date, file)
    def apply(conf: Configuration) = {
      val result = runBSS(conf)
      val symbols = conf.symbols mkString "."
      val saveMongo = new SaveMongo(conf.name + "_" + symbols, conf.name, conf.name)
      saveMongo(result)
      result
    }
  }

  case class RunConfigurationRemote(javafs: String, filter: Option[List[String]] = None) extends RunConfiguration[JavaStdStrategy] {
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

  case class Configuration(symbols: List[String], dates: List[String], from: String, to: String, name: String, bookOrder: Int, workers: Int, javaFilename: String, parameters: List[ConfigParam]) {
    def loadedCSVs: List[(String, String)] = {
      val IOs = this.dates.map {
        date =>
          (date, new IOSideEffect[String] {
            def run: String = {
              FileManager.downloadReuters(symbols.toArray, date)
            }
          })
      }
      IOs.map {
        case (date, side) => (date, side.run)
      }

    }
  }

  case class ConfigParam(base: Double, to: Option[Double], by: Option[Double], name: String)

  object ConfigParam {
    implicit def paramCodec: CodecJson[ConfigParam] = casecodec4(ConfigParam.apply, ConfigParam.unapply)("base", "to", "by", "name")
  }

  object Configuration {
    implicit def confParam: CodecJson[Configuration] = casecodec9(Configuration.apply, Configuration.unapply)("symbols", "dates", "from", "to", "name", "bookOrder", "workers", "javaFilename", "parameters")

  }
}