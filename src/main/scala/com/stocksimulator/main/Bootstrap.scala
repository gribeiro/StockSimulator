package com.stocksimulator.main
import com.github.nscala_time.time.Imports._
import com.stocksimulator.reuters._
import com.stocksimulator.abs._
import scala.util._
import scala.concurrent._
import com.stocksimulator.debug._
import com.stocksimulator.java._
import scala.concurrent.ExecutionContext.Implicits.global
import akka.actor._
import com.typesafe.config.ConfigFactory
import akka.routing._
import akka.actor.Terminated
import com.stocksimulator.parallel._
import scala.collection.mutable.ListBuffer
import com.stocksimulator.remote._
import javax.script.ScriptEngineManager
import java.io.FileReader
import java.io.File
import com.stocksimulator.reuters._
import argonaut._
import Argonaut._
import scalaz._
import Scalaz._
import scala.io.Source
import scala.collection.mutable.ArrayBuffer
import com.stocksimulator.ff._
import com.stocksimulator.main.ConfigurationModule._
object Bootstrap {
  import com.stocksimulator.debug.LogNames._
  trait CommandLineOption

  case class Local(filename: String) extends CommandLineOption
  case class Job(ip: String, port: String, filename: String) extends CommandLineOption
  case class Worker(ip: String, port: String, workers: Int) extends CommandLineOption
  case class Master(ip: String) extends CommandLineOption

  case class DefferedGet[T](arr: Array[T], idx: Int, isEqualTo: Option[T] = None) {
    private val convertedArray = ArrayBuffer.empty[T]
    arr.foreach {
      elem => convertedArray += elem
    }
    def get: Option[T] = {
      
      isEqualTo match {  
      
        case Some(iet) =>
          convertedArray.collectFirst {

            case element if (iet == element && convertedArray.indexOf(element) == idx) => element
          }
        case None =>
          convertedArray.collectFirst {
            case element if (convertedArray.indexOf(element) == idx) => element
          }
      }
    }
  }

  case class CommandLineStatus(keys: List[(Option[String], Int, String)], args: Array[String]) {

    def getOption[Option[CommandLineOption]] = {
      val options = keys.map {
        case (name, position, givenName) => (name, DefferedGet(args, position, name).get, givenName)
      }
      val filtered = options.filter {
        case (name, Some(option), givenName) => true
        case _ => false
      }.map {
        case (name, option, givenName) => (givenName, option.get)
      }
     val map = filtered.toMap
    def isCase(list: List[String]) = {
     list.map {
       attr => map.get(attr) match {
         case Some(_) => true
         case _ => false
       }
     }.reduce(_&&_)
    }
    val cases = List(
        ("local",List("local", "filename")),
        ("worker", List("remote", "worker", "workerhost", "workerport", "workers")), 
        ("master", List("remote", "master", "host")),
        ("job",List("job", "ip", "port", "filename"))
    	)
   
    cases.map {
      case ("local", content) if(isCase(content)) => 
       Local(map("filename")).some
      case ("worker", content) if(isCase(content)) =>
        val workers = map("workers").toInt
        Worker(map("workerhost"), map("workerport"), workers).some
      case ("master", content) if(isCase(content)) => 
       Master(map("host")).some
      case ("job", content) if(isCase(content)) =>
        Job(map("ip"), map("port"), map("filename")).some
      case _ => None
    }.collectFirst {
      case Some(el) => el
    }
    
    
    
    }
    
    
  }

  def main(args: Array[String]): Unit = {
   
    Log.setActive(true)
   
    val keys = List(
        ("local".some, 0, "local"), 
        ("remote".some, 0, "remote"), 
        ("job".some, 0, "job"), 
        ("worker".some, 1, "worker"), 
        ("master".some, 1, "master"), 
        (None, 2, "host"),
        (None, 2, "ip"), 
        (None, 2, "workerhost"),
        (None, 3,"workerport"), 
        (None, 1, "ip"), 
        (None, 2,"port"), 
        (None, 4, "filename"), 
        (None, 1,"filename"), 
        (None, 3,"filename"), 
        (None, 4, "workers"))
        
    CommandLineStatus(keys, args).getOption match {
      case Some(status) =>
        this.log(status)
        status match {
          case Local(filename) => RunFromFileJson(filename)
          case Job(ip, port, filename) =>
            val config = ConfigurationLoadJson.load(filename)
            val fs = (new FiletoStringIO(filename)).run
            config match {
              case Some(conf) => 
                val jfs = (new FiletoStringIO(config.get.javaFilename+".java")).run
                val mJob = MasterJob(fs, jfs)
                RemoteWorkers.emitWork(ip, port.toInt, mJob)
              case None =>
                this.log("Erro ao carregar json..")
            }
          case Worker(ip, port, workers) =>
            RemoteWorkers.localWorker(ip, port.toInt)
          case Master(host) =>
           RemoteWorkers.localMaster(host)
           
        }
      case None => 
        this.log("Erro ao processar argumentos.")
    }
   Log.stopActor
  }

}