package com.stocksimulator.debug
import com.github.nscala_time.time.Imports._
import akka.actor._


import com.typesafe.config.ConfigFactory
abstract class LogProtocol
case class LogMessage(s: Any, noTime:Boolean) extends LogProtocol
case object LogBye
class LogWorker extends Actor {
  def receive = {
    case LogMessage(s, noTime) =>
         val tNow = DateTime.now
         val elapsed = new Period(Log.startTime, tNow).toString()
         val sStr = if (null == s) "null" else s.toString()
         val message = if(!noTime)  s"[$tNow] - $s - Elapsed Total: $elapsed" else s"$s"
         println(message)
    case `LogBye` =>
      context.system.shutdown()
  }
}


object LogLevel extends Enumeration {
  type LogLevel = Value
  val Info, Warning, Error = Value
}

import LogLevel._

object Log {
    val config = ConfigFactory.parseString("" +
      "akka.loglevel=INFO\n" +
      "akka.debug.lifecycle=on\n" +
      "akka.debug.receive=on\n" +
      "akka.debug.event-stream=on\n" +
      "akka.debug.unhandled=on\n" +
      ""
    )
  private var active: Boolean = false
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
  
  def apply(s: Any, noTime:Boolean = false, ll: LogLevel = Info) = {
    if(active && level.contains(ll)) {
     logActor ! LogMessage(s, noTime)
    }
  }
}