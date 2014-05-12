package com.stocksimulator.abs

import scala.collection.mutable.ArrayBuffer
import scala.math.Numeric
import com.stocksimulator.debug.Log
import com.stocksimulator.helpers.RingBuffer

class WindowCaller[T] {
	val windows:ArrayBuffer[WindowTimeControl] = ArrayBuffer.empty[WindowTimeControl]
	def <-- (w: WindowTimeControl) = register(w)
	def register(w: WindowTimeControl) = {
	  windows += w
	}
	
	
	def unregister(w: WindowTimeControl) = {
	  windows -= w
	}
	
	def call(mSeconds: Int) = {
	  windows.foreach(a => a.timePassed(mSeconds))
	}  
}
class SimpleCallBack(mSecondsToRun: Int, callback: () => Unit) extends WindowTimeControl {
  private var mSecondsRunDecrement = mSecondsToRun
  private var onState = true
  def unary_+ = on()
  def unary_- = off()
  def off() = {
    onState = false
    mSecondsRunDecrement = mSecondsToRun
  }
  def on() = {
    onState = true
  }
  def timePassed(mSeconds: Int) = {
    if(onState) {
	mSecondsRunDecrement -= mSeconds
	if(mSecondsRunDecrement <= 0) {
	  callback()
	  mSecondsRunDecrement += mSecondsToRun
	}
  }
  }
}
abstract class Windowable[T : Numeric](size: Int)(implicit tManifest: Manifest[T]) extends WindowTimeControl {
  private val buffer: RingBuffer[T] = new RingBuffer[T](size)
  private var myVal: T = implicitly[Numeric[T]].zero
  val mSecondsToRun: Int
  val mSecondsToAdd: Int
  val feeder: WindowParam[T]
  private var mSecondsRunDecrement = mSecondsToRun
  private var mSecondsAddDecrement = mSecondsToAdd
  
  def timePassed(mSeconds: Int) = {
	mSecondsRunDecrement -= mSeconds
	mSecondsAddDecrement -= mSeconds
	if(mSecondsRunDecrement <= 0) {
	  myVal = calculate()
	  mSecondsRunDecrement += mSecondsToRun
	}
	
	if(mSecondsAddDecrement <= 0) {
	
	  buffer += feeder.next()
	  
	  mSecondsAddDecrement = mSecondsToAdd
	}
  }
  protected def calculate(): T
  def lastVal() = myVal
  def getBuffer() = buffer
  
}


trait WindowParam[T] {
  def next(): T
}

trait WindowTimeControl {
   def timePassed(mSeconds: Int)
}

