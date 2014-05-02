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
  sealed trait CommandLineOption

  case class Local(filename: String) extends CommandLineOption
  case class Job(filename: String) extends CommandLineOption
  case class Worker(qtd: Int) extends CommandLineOption
  case object Preprocessor extends CommandLineOption
  case object Resulter extends CommandLineOption

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
          attr =>
            map.get(attr) match {
              case Some(_) => true
              case _ => false
            }
        }.reduce(_ && _)
      }

      def singleCase(cas: String) = {
        (cas, List(cas))
      }

      val cases = List(
        ("local", List("local", "arg1")),
        ("worker", List("worker", "arg1")),
        singleCase("preprocessor"),
        singleCase("result"),
        ("job", List("job", "arg1")))

      cases.map {
        case ("local", content) if (isCase(content)) =>
          Local(map("arg1")).some
        case ("worker", content) if (isCase(content)) =>
          val workersQtd = map("arg1").toInt
          Worker(workersQtd).some
        case ("preprocessor", content) if (isCase(content)) =>
          Preprocessor.some
        case ("result", content) if (isCase(content)) =>
          Resulter.some
        case ("job", content) if (isCase(content)) =>
          Job(map("arg1")).some
        case _ => None
      }.collectFirst {
        case Some(el) => el
      }

    }

  }

  def firstArg(arg: String) = (arg.some, 0, arg)

  def supportArgs(n: Int) = {
    val seq = for (i <- 1 to n) yield (None, i, "arg" + i)
    seq.toList
  }

  def main(args: Array[String]): Unit = {
    import com.stocksimulator.aws._
    import com.stocksimulator.main.ConfigurationModule._
    Log.setActive(true)

    val firstArgs = List(
      firstArg("local"),
      firstArg("job"),
      firstArg("worker"),
      firstArg("preprocessor"),
      firstArg("result"))

    val keys = firstArgs ++ supportArgs(4)

    this.log("Starting...")
    CommandLineStatus(keys, args).getOption match {
      case Some(status) =>

        status match {
          case Local(filename) => RunFromFileJson(filename)
          case Job(filename) =>
            val passo1 = new SendJobService("simulacoes-etp", "simul-preproces")
            val config = ConfigurationLoadJson.load(filename).get
            passo1(config)
          case Worker(qtd: Int) =>
            for(i <- 1 to qtd) {
            val workerService = new RunnerService("simul-jobs", "simul-result", "simulacoes-etp", 1)
            workerService.run
            }
          case Preprocessor =>
            val preprocessor = new AcquireDatService("simul-preproces", "simul-jobs", "simulacoes-etp")
            preprocessor.run
          case Resulter =>
            val resulter = new OutputService("simul-result", "Resultados")
            resulter.run
        }
      case None =>
        this.log("Erro ao processar argumentos.")
    }
    Log.stopActor
  }

}