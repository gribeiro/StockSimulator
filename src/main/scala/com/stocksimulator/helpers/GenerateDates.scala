package com.stocksimulator.helpers

import scalaz._
import Scalaz._

object GenerateDates {
  import org.joda.time._
  import org.joda.time.format.DateTimeFormat
  
  def parse(str: String) = DateTime.parse(str, microDateFormat)
  def dateRange(from: DateTime, to: DateTime, step: Period): Iterator[DateTime] =
    Iterator.iterate(from)(_.plus(step)).takeWhile(!_.isAfter(to))

  val microDateFormat = DateTimeFormat.forPattern("dd/MM/yyyy")

  val start = parse("02/02/2014") 
  val end = parse("29/05/2014")

  val dr = dateRange(start, end, Period.days(1))

  val agg = dr.foldLeft("") {

    (acc, dt) =>
      val dtStr = dt.toString(microDateFormat)

      acc + s"""\"$dtStr\","""

  }

}