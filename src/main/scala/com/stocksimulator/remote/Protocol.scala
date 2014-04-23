package com.stocksimulator.remote

import akka.actor.ActorSelection
import akka.actor.ActorRef
import com.stocksimulator.abs.Parameters
import java.io.ByteArrayOutputStream
import resource._
import java.io.ObjectOutputStream
import java.io.ByteArrayInputStream
import java.io.ObjectInputStream

sealed trait RemoteProtocol

case class Job(filename: String, date: String, fs: String, parameter: Array[String], name: String, javaFs: String) extends RemoteProtocol {
  val uuid =  java.util.UUID.randomUUID.toString
  
  override def equals(o: Any) = o match {
    case j: Job => j.uuid == uuid
    case _ => o.equals(this) 
  }
  override def hashCode = uuid.hashCode()
}



case class FileInit(filename: String) extends RemoteProtocol
case class FileChunk(filename: String, file: Array[Byte]) extends RemoteProtocol
case class FileEnd(filename: String) extends RemoteProtocol


case class ResultChunk(name: String, data: Array[Byte]) extends RemoteProtocol
case class ResultEnd(name: String, job: Job) extends RemoteProtocol

case class FileRequest(filename: String) extends RemoteProtocol
case class SaveResult(result: Array[Byte], job: Job) extends RemoteProtocol
case object RegisterOk extends RemoteProtocol
case object NewJobArrived extends RemoteProtocol
case object WorkerJobRequest extends RemoteProtocol
case class JobFailed(job: Job) extends RemoteProtocol 
case class MasterJob(fs: String, javaFs: String) extends RemoteProtocol
case class Idle(jobDone: Job) extends RemoteProtocol
case object QueueStatus extends RemoteProtocol
case class Register() extends RemoteProtocol
case class Master(actor: ActorSelection) extends RemoteProtocol
case class MasterJobSender(actor: ActorSelection, j: MasterJob) extends RemoteProtocol
case object Ping extends RemoteProtocol
case object JobAck extends RemoteProtocol
case object NoJob extends RemoteProtocol
case class JobAckSmall(job: Job) extends RemoteProtocol

sealed trait JobState
case class JobRunning(a: ActorRef, confirm: Boolean) extends JobState
case object JobDone extends JobState
case object JobToDo extends JobState
case class JobSent(confirm: Boolean) extends JobState
case object JobConfirm extends JobState