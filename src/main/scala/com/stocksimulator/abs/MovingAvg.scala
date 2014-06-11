package com.stocksimulator.abs

import com.stocksimulator.debug.Log
import scala.concurrent._
import ExecutionContext.Implicits.global
import akka.util.Timeout
import scala.concurrent.duration._
import com.stocksimulator.debug.LogNames._
abstract class MovingWindow(windowSize: Int, elapsed: Int, update: () => Double) extends Windowable[Double, Double](windowSize) with WindowParam[Double] {
	val mSecondsToRun = elapsed
	val mSecondsToAdd = elapsed/windowSize
	val feeder = this
	def calculate(): Double 
	def next() = {
	  //future {
		  update()
	  //}
	}
	def isAvailable(): Boolean = (getBuffer().size >= windowSize)
	override def toString() =  if(isAvailable) s"Window value: $lastVal" else "Window unavailable"
}

class MovingAvg(windowSize: Int, elapsed: Int, update: () => Double) extends MovingWindow(windowSize, elapsed, update) {
	var myVal = 0.0
	def calculate(): Double = {
	  val values = getBuffer
	  val res = if(values.size > 0) {
	 val wait =  values.map {
	    fut =>
	      //val present = Await.result(fut, 60.seconds)
	      //println(present)
	      //present
	      fut
	  }
	  //swait.foreach(println(_))
	  wait.filter(_ != 0).sum/values.size
	  } else 0
	//  this.log(res.toString())
	  res
	}

	
	  
}
