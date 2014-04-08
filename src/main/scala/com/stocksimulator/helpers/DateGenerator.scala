package com.stocksimulator.helpers

import scala.collection.immutable.Range.Inclusive
import org.jruby.RubyArray


object DateComposable {
  implicit class monthHandler(xs: Seq[Month]) {
    def day(d: Int) = {
      xs.map(m => m.day(d))
    }
    
    def day(ds: Int*) = {
      val all = ds.map {
        d => xs.map(m => m.day(d))
      }
      all.flatten.asInstanceOf[Seq[Day]].getStrings
    }

  
  def day(ms: Array[Int]) = {
    val all = ms.map {
        d => xs.map(m => m.day(d))
      }
      all.toSeq.flatten.asInstanceOf[Seq[Day]].getStrings
  }
  
  def day(ms: Inclusive):Seq[String] = day(ms.toArray)
  }
  
  implicit class dayHandler(xs: Seq[Day]) {
     def getStrings:Seq[String] = {
    	xs.map {
    	  d => d.dc.previousList.reverse.mkString("/")
    	}
    }
  }
}
class DateComposable(val info:Int, val previousList: List[DateComposable] = List()) {
  
  def + (that: DateComposable): DateComposable = {
	new DateComposable(that.info, that :: previousList )
  }
  
  override def toString() = {
    if(info < 10) "0"+info else info.toString
  }
  
}

case class Year(_info: Int) extends DateComposable(_info, List(new DateComposable(_info))) {
  def month(m: Int) = {
    Month(this + new DateComposable(m))
  }
  
  def month(ms: Int*):Seq[Month] = {
    ms.map(f => this.month(f))
  }
  
  def month(ms: Array[Int]): Seq[Month] = {
    ms.map(f => this.month(f))
  }
  
  def month(ms: Inclusive): Seq[Month] = month(ms.toArray)
}

case class Month(dc: DateComposable) {
  def day(d: Int) = {
	 Day(Month(dc + new DateComposable(d)))
  }
  
   def day(ms: Int*):Seq[Day] = {
    ms.map(f => this.day(f))
  }

  def day(ms: Array[Int]): Seq[Day] = {
    ms.map(f => this.day(f))
  }
  

  def day(ms: Inclusive): Seq[Day] = day(ms.toArray)
}

case class Day(month: Month) {
   def dc = month.dc
  
  override def toString():String = {
   dc.previousList.reverse.mkString("/")
  }
}
