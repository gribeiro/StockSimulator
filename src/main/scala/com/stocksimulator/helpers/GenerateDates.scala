package com.stocksimulator.helpers


import scalaz._
import Scalaz._

object GenerateDates {
  import org.joda.time._
  import org.joda.time.format.DateTimeFormat

def dateRange(from: DateTime, to: DateTime, step: Period): Iterator[DateTime] =
  Iterator.iterate(from)(_.plus(step)).takeWhile(!_.isAfter(to))
  
  
  
  val microDateFormat = DateTimeFormat.forPattern("dd/MM/yyyy")
             
   val start = DateTime.parse("02/01/2014", microDateFormat)
   val end = DateTime.parse("02/05/2014", microDateFormat)
   
  val dr = dateRange(start, end, Period.days(1))


val agg = dr.foldLeft("") {

(acc, dt) =>
val dtStr = dt.toString(microDateFormat)

acc+s"""\"$dtStr\","""

}

}