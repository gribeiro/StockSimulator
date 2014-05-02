package com.stocksimulator.remote

import scala.collection.mutable.HashMap
import scala.collection.mutable.ArrayBuffer

trait BinaryHandler[T] {
  private val result = HashMap.empty[String, ArrayBuffer[Byte]]
  def receiveChunk(name: String, chunk: Array[Byte]) = {
       result.get(name) match {
        case Some(buffer) => buffer ++= chunk
        case None => result(name) = (ArrayBuffer.empty[Byte] ++ chunk)
      }
  }
  
  def finishReceive(name: String, extraData: Option[T] = None) = {
    val arr = result(name).toArray
    result -= name
    extraFinishReceive(name, arr, extraData)
  }
  def extraFinishReceive(name: String, arr: Array[Byte], extraData: Option[T])
}
