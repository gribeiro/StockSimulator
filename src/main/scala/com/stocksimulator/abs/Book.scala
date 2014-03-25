package com.stocksimulator.abs
import com.stocksimulator.debug._


class Book(var initFeed: Int = 0) {
 var userCurPos = initFeed
 
 def feed(qtd: Int) = {
	/*if(qtd < initFeed) {
	  val diff = initFeed - qtd
	  userCurPos -= diff
	}
	initFeed = qtd*/
   userCurPos -= qtd
 }

 def incompleteFeed(qtd: Int) = {
   //feed(qtd)
   if(userCurPos > 0) {
	   if(userCurPos - qtd < 1) userCurPos = 1 else feed(qtd)
   }
 }
 def canExecute = userCurPos <= 0
 
 override def toString(): String = "Queue Position: " + userCurPos.toString() + " initFeed = " + initFeed.toString()
}
