package com.stocksimulator.abs

import com.stocksimulator.debug.Log

abstract class MovingWindow(windowSize: Int, elapsed: Int, update: () => Double) extends Windowable[Double](windowSize) with WindowParam[Double] {
	val mSecondsToRun = elapsed
	
	val mSecondsToAdd = elapsed/windowSize
	val feeder = this
	def calculate(): Double 
	def next() = update()
	def isAvailable(): Boolean = (getBuffer().size >= windowSize)
	override def toString() =  if(isAvailable) s"Window value: $lastVal" else "Window unavailable"
}

class MovingAvg(windowSize: Int, elapsed: Int, update: () => Double) extends MovingWindow(windowSize, elapsed, update) {
	def calculate(): Double = {
	  val values = getBuffer
	  values.sum/windowSize.toDouble
	}

	
	  
}
