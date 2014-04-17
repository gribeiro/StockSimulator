package com.stocksimulator.remote

import akka.actor.ActorSelection
import akka.actor.ActorRef
import com.stocksimulator.abs.Parameters
import java.io.ByteArrayOutputStream
import resource._
import java.io.ObjectOutputStream
import java.io.ByteArrayInputStream
import java.io.ObjectInputStream

abstract class RemoteProtocol

case class Job(filename: String, date: String, fs: String, parameter: Array[String], name: String, javaFs: String) extends RemoteProtocol {
  val uuid =  java.util.UUID.randomUUID.toString
  
  override def equals(o: Any) = o match {
    case j: Job => j.uuid == uuid
    case _ => o.equals(this) 
  }
  override def hashCode = uuid.hashCode()
}


object ObjectToByteArray {
  def apply[T](obj: T) = {
    val stream = new ByteArrayOutputStream
    val monad = managed(stream).map(new ObjectOutputStream(_))
    monad.acquireAndGet {
      output =>
        output.writeObject(obj)
        output.close
    }
    stream.toByteArray()
  }
}

object ByteArrayToObject {
    def apply[T](ba: Array[Byte]):T = {
    val stream = new ByteArrayInputStream(ba)
    val monad = managed(stream).map(new ObjectInputStream(_))
    var obj:Object = null
    monad.acquireAndGet {
      input =>
        obj = input.readObject()
        input.close()
    }
    obj.asInstanceOf[T]
  }
}
case class FileInit(filename: String)
case class FileChunk(filename: String, file: Array[Byte])
case class FileEnd(filename: String)


case class ResultChunk(name: String, data: Array[Byte])
case class ResultEnd(name: String, job: Job)

case class FileRequest(filename: String)
case class SaveResult(result: Array[Byte], job: Job)

case object NewJobArrived extends RemoteProtocol
case object WorkerJobRequest extends RemoteProtocol
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

trait JobState
case class JobRunning(a: ActorRef, confirm: Boolean) extends JobState
case object JobDone extends JobState
case object JobToDo extends JobState
case class JobSent(confirm: Boolean) extends JobState
case object JobConfirm extends JobState