package com.stocksimulator.parallel

import com.stocksimulator.abs.Parameters
import com.typesafe.config.ConfigFactory
import com.stocksimulator.main.BSSet
import com.stocksimulator.abs.Strategy
import com.stocksimulator.reuters.MongoConfig

trait stockSimProtocol
case class spWork(bundleInfo: Parameters, date: String) extends stockSimProtocol
case class spResult(a:Parameters, b:Parameters) extends stockSimProtocol
case class spMongoConfig(conf: MongoConfig) extends stockSimProtocol
case object spRequestMongoConfig extends stockSimProtocol
case object spMongoConfigUnavailable extends stockSimProtocol
case object spWorkInProgress extends stockSimProtocol
case object spReportDone extends stockSimProtocol
case object spMasterChecking extends stockSimProtocol
case object spAllWorkDone extends stockSimProtocol
case object spLast extends stockSimProtocol

object ParCommon {
     var hostname = "192.168.90.15"
     val config = ConfigFactory.parseString("" +
      "akka.loglevel=INFO\n" +
      "akka.debug.lifecycle=on\n" +
      "akka.debug.receive=on\n" +
      "akka.debug.event-stream=on\n" +
      "akka.debug.unhandled=on\n" +
      ""
    )

    def remoteConfig(port: Int) = ConfigFactory.parseString("" +
      "akka.loglevel=INFO\n" +
      "akka.debug.lifecycle=on\n" +
      "akka.debug.receive=on\n" +
      "akka.debug.event-stream=on\n" +
      "akka.debug.unhandled=on\n" +
      "akka.actor.provider=\"akka.remote.RemoteActorRefProvider\"\n" +
      "akka.remote.enabled-transports=[\"akka.remote.netty.tcp\"]\n" +
      "akka.remote.netty.tcp.hostname=\""+hostname+"\"\n" +
      "akka.remote.netty.tcp.port="+port+"\n" +
    "")
    
    def remoteConfigWithoutPort = ConfigFactory.parseString("" +
      "akka.loglevel=INFO\n" +
      "akka.debug.lifecycle=on\n" +
      "akka.debug.receive=on\n" +
      "akka.debug.event-stream=on\n" +
      "akka.debug.unhandled=on\n" +
      "akka.actor.provider=\"akka.remote.RemoteActorRefProvider\"\n" +
      "akka.remote.enabled-transports=[\"akka.remote.netty.tcp\"]\n" +
   
      "akka.remote.netty.tcp.port=0\n" +

    "")
}

trait BootstrapProtocol
case class bsWork[T <: Strategy](set: BSSet[T])
case class bsLoadClass(filename: String, pack: String)
