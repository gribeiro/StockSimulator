package com.stocksimulator.remote

import akka.actor.ActorSystem
import akka.actor.Props
import com.stocksimulator.parallel.ParCommon

object Roles {
  import com.typesafe.config._

  
  trait RoleOf[T] {
    val commonConfig = {
      ConfigFactory.parseString("" +
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
    def defineRole(role: T): Unit
    def config(configurable: T): Config = commonConfig
  }
  object RoleOf {
    implicit object RoleOfWorker extends RoleOf[WorkerParam] {
      def defineRole(role: WorkerParam) = {
        val port = role.port
        val ip = role.ip
        val roleWorkers = role.workers
        val emitterSystem = ActorSystem("workerSystem", config(role))
        val master = emitterSystem.actorSelection(s"akka.tcp://masterSystem@$ip:$port/user/Master")
        val worker = emitterSystem.actorOf(Props(classOf[RemoteJobActor], roleWorkers), "Worker")       
        master ! Ping
        worker ! Master(master)
        emitterSystem.awaitTermination()
      }
    }

    implicit object RoleOfEmitter extends RoleOf[EmitterParam] {
      def defineRole(role: EmitterParam) = {
        val ip = role.ip
        val j = role.j
        val port = role.port
        val sys = ActorSystem("jobSystem", config(role))
        val master = sys.actorSelection(s"akka.tcp://masterSystem@$ip:$port/user/Master")
        val jobSender = sys.actorOf(Props(classOf[JobSender]), "JobSender")
        jobSender ! MasterJobSender(master, j)
      }
    }
    implicit object RoleOfMaster extends RoleOf[MasterParam] {
      def defineRole(role: MasterParam) = {
        val host = role.host
        val config = ParCommon.remoteConfig(2552, host)

        val listenerSystem = ActorSystem("masterSystem", config)
        listenerSystem.actorOf(Props(classOf[MasterRemoteActor]), "Master")

        listenerSystem.awaitTermination()

      }

      override def config(configurable: MasterParam) = {
        ConfigFactory.parseString("" +
          "akka.loglevel=INFO\n" +
          "akka.debug.lifecycle=on\n" +
          "akka.debug.receive=on\n" +
          "akka.debug.event-stream=on\n" +
          "akka.debug.unhandled=on\n" +
          "akka.actor.provider=\"akka.remote.RemoteActorRefProvider\"\n" +
          "akka.remote.enabled-transports=[\"akka.remote.netty.tcp\"]\n" +
          "akka.remote.netty.tcp.hostname=\"" + configurable.host + "\"\n" +
          "akka.remote.netty.maximum-frame-size = 9128000b\n" +
          "akka.remote.watch-failure-detector.acceptable-heartbeat-pause=20s\n" +
          "akka.remote.netty.tcp.port=2552\n" +
          "")
      }
    }
  }
  case class MasterParam(host: String)
  case class WorkerParam(ip: String, port: Int, workers: Int)
  case class EmitterParam(ip: String, port: Int, j: MasterJob)

  object LoadRole {
    def apply[T](role: T)(implicit ev: RoleOf[T])= {
      ev.defineRole(role)
    }
  }

}
object RemoteWorkers {
 import Roles._

 
  def localMaster(host: String) = LoadRole(MasterParam(host))
  def localWorker(ip: String, port: Int, workers: Int) = LoadRole(WorkerParam(ip, port, workers))
  def emitWork(ip: String, port: Int, j: MasterJob) = LoadRole(EmitterParam(ip, port, j))

}