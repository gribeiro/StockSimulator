package com.stocksimulator.remote

import java.io.ByteArrayOutputStream
import java.io.ObjectOutputStream
import java.io.ByteArrayInputStream
import java.io.ObjectInputStream
import resource._
import akka.actor.ActorRef


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

case class ChopBinary[T,U](partMessage: (Array[Byte]) => T, endMessage: () => U, actor: ActorRef) {
  def apply(ba: Array[Byte], size: Int = 4096) {
    val parts = ba.grouped(size)
    
    parts.foreach {
      part =>
        actor ! partMessage(part)
    }
    actor ! endMessage()
  }
}