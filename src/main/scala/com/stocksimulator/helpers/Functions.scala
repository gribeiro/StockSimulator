package com.stocksimulator.helpers

import org.joda.time.format.DateTimeFormat

object Functions {
	val dateFormat = DateTimeFormat.forPattern("dd/MM/yyyy")
	
	def changePatterns(date: String) = {
	val partA = dateFormat.parseDateTime(date)
    (List(partA.dayOfMonth().get(), partA.monthOfYear().get(), partA.year().get()).mkString("/"))
	}

}