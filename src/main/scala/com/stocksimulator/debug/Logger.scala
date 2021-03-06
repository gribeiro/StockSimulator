package com.stocksimulator.debug
import com.github.nscala_time.time.Imports._
import com.stocksimulator.abs._
import akka.actor._
import com.typesafe.config.ConfigFactory

object LogNames {
  implicit class LogAny(t: Any) {
    def log(message: Object) = {
      val klass = t.getClass()
      val messageStr = message.toString()
      Log(s"$messageStr")
    }
  }
}

object Log {
  object LogLevel extends Enumeration {
    type LogLevel = Value
    val Info, Warning, Error = Value
  }

  import LogLevel._
  abstract class LogProtocol
  case class LogMessage(s: Any, noTime: Boolean) extends LogProtocol
  case object LogBye
  class LogWorker extends Actor {
    var lastTime = new DateTime(0)
    def receive = {
      case LogMessage(s, noTime) =>
        val tNow = DateTime.now
        val tDiff = (new Period(lastTime, tNow)).toString()
        lastTime = tNow
        val elapsed = new Period(Log.startTime, tNow).toString()
        val sStr = if (null == s) "null" else s.toString()
        val message = if (!noTime) s"[$tNow] - $s - Elapsed Total: $elapsed - Partial [$tDiff]" else s"$s"
        println(message)
      case `LogBye` =>
        context.system.shutdown()
    }
  }

  val config = ConfigFactory.parseString("" +
    "akka.loglevel=INFO\n" +
    "akka.debug.lifecycle=on\n" +
    "akka.debug.receive=on\n" +
    "akka.debug.event-stream=on\n" +
    "akka.debug.unhandled=on\n" +
    "")
  private var active: Boolean = true
  private var level: Set[LogLevel] = Set(Info, Warning, Error)
  val startTime = DateTime.now
  val system = ActorSystem("logSystem", config)
  val logActor = system.actorOf(Props[LogWorker], "lgLogger")
  def stopActor = {
    logActor ! LogBye
  }
  def setActive(b: Boolean) = {
    active = b
  }

  def setLevel(s: Set[LogLevel]) = {
    level = s
  }

  def apply(s: Any, noTime: Boolean = false, ll: LogLevel = Info) = {
    if (active && level.contains(ll)) {
      println(s)
     // logActor ! LogMessage(s, noTime)
    }
  }
}