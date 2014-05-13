package com.stocksimulator.parallel

import com.stocksimulator.abs.Parameters
import com.typesafe.config.ConfigFactory
import com.stocksimulator.abs.Strategy

trait stockSimProtocol
case class spWork(bundleInfo: Parameters, date: String) extends stockSimProtocol
case class spResult(a:Parameters, b:Parameters) extends stockSimProtocol
case object spRequestMongoConfig extends stockSimProtocol
case object spMongoConfigUnavailable extends stockSimProtocol
case object spWorkInProgress extends stockSimProtocol
case object spReportDone extends stockSimProtocol
case object spMasterChecking extends stockSimProtocol
case object spAllWorkDone extends stockSimProtocol
case object spLast extends stockSimProtocol

object ParCommon {

     val config = ConfigFactory.parseString("" +
      "akka.loglevel=INFO\n" +
      "akka.debug.lifecycle=on\n" +
      "akka.debug.receive=on\n" +
      "akka.debug.event-stream=on\n" +
      "akka.debug.unhandled=on\n" +
      ""
    )

    def remoteConfig(port: Int, hostname: String) = ConfigFactory.parseString("" +
      "akka.loglevel=INFO\n" +
      "akka.debug.lifecycle=on\n" +
      "akka.debug.receive=on\n" +
      "akka.debug.event-stream=on\n" +
      "akka.debug.unhandled=on\n" +
      "akka.actor.provider=\"akka.remote.RemoteActorRefProvider\"\n" +
      "akka.remote.enabled-transports=[\"akka.remote.netty.tcp\"]\n" +
      "akka.remote.netty.tcp.hostname=\""+hostname+"\"\n" +
      "akka.remote.netty.maximum-frame-size = 9128000b\n" +
      "akka.remote.watch-failure-detector.acceptable-heartbeat-pause=20s\n" +
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
      "akka.remote.netty.tcp.maximum-frame-size = 9128000b\n" +
      "akka.remote.watch-failure-detector.acceptable-heartbeat-pause=20s\n" +
      "akka.remote.netty.tcp.port=0\n" +

    "")
}

