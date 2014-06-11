package com.stocksimulator.abs

import com.stocksimulator.debug.LogNames._

object StdRate {
	import scala.io.Source
	import java.io._
	private case class SelicTable(date: String, anualRef: Double, mean: Option[Double])
	val filename = "selic.txt"
	//private[this] val rates = Map.empty[Period, Double].withDefaultValue(0.11)
	val selicDefault = 0.10 
	private[this] lazy val rates = {
	  try {
	  val fileName = "selic.txt"
	   val resourceURL = getClass.getResource(fileName)
	   
	   val preLines = if(resourceURL != null) Source.fromURL(resourceURL).getLines else Source.fromFile(fileName).getLines
	   val lines = preLines drop 2
	   val structured = lines.map{
	     line => line.split("""\;""")
	   }.filter(_.size == 9).map {
	     insideInfo => 
	       
	     def brValtoUsVal(str: String) = str.replace(',', '.').toDouble / 100
	     val date = insideInfo(0)
	     val anualRef = brValtoUsVal(insideInfo(1))
	     val mean = brValtoUsVal(insideInfo(4))
	     (date, if(mean == 0.0) SelicTable(date, anualRef, None) else SelicTable(date, anualRef, Some(mean)))
	   }
	  structured.toMap
	   }
	   catch {
	     case e: Exception => 
	       this.log(e)
	       Map.empty[String, SelicTable]
	   }
	}
	def rate(date: String):Double = {
	  
		val rateReturn = rates.get(date) match {
		  case Some(selicTable) => selicTable.mean.getOrElse(selicTable.anualRef)
		  case None => selicDefault
		}
		this.log(s"Rate for $date is $rateReturn")
		rateReturn
	}
}