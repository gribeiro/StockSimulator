package com.stocksimulator.remote

import akka.actor.ActorSelection
import akka.actor.ActorRef
import com.stocksimulator.abs.Parameters

abstract class RemoteProtocol

case class Job(filename: String, date: String, fs: String, parameter: Array[String], name: String) extends RemoteProtocol {
  val uuid =  java.util.UUID.randomUUID.toString
  
  override def equals(o: Any) = o match {
    case j: Job => j.uuid == uuid
    case _ => o.equals(this) 
  }
  override def hashCode = uuid.hashCode()
}
case object NewJobArrived extends RemoteProtocol
case object WorkerJobRequest extends RemoteProtocol
case class MasterJob(fs: String) extends RemoteProtocol
case class Idle(jobDone: Job) extends RemoteProtocol
case object QueueStatus extends RemoteProtocol
case class Register() extends RemoteProtocol
case class Master(actor: ActorSelection) extends RemoteProtocol
case class MasterJobSender(actor: ActorSelection, j: MasterJob) extends RemoteProtocol
case object Ping extends RemoteProtocol
case object JobAck extends RemoteProtocol
case object NoJob extends RemoteProtocol
case class JobAckSmall(job: Job) extends RemoteProtocol

trait JobState
case class JobRunning(a: ActorRef, confirm: Boolean) extends JobState
case object JobDone extends JobState
case object JobToDo extends JobState
case class JobSent(confirm: Boolean) extends JobState
case object JobConfirm extends JobState