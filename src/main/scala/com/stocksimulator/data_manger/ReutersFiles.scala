package com.stocksimulator.data_manger

import com.etp._
import org.joda.time._

class ReutersFiles(username: String, password: String) {
	val conn = new RTHConnector
	
	def apply(symbols: Array[String], date: String):String = {
	  conn.downloadMultiTAQFile(symbols, date, username, password)
	}

	def apply(symbols: Array[String], date: List[String]):Iterable[String] = {
	  for(d <- date) yield apply(symbols, d)
	}
	
}

object hcReuters extends ReutersFiles("andre@allianceasset.com.br", "fiveware456")
