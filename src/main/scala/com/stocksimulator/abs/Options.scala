package com.stocksimulator.abs

import scala.collection.mutable.HashMap
import org.joda.time.DateTime



object OptionInstrument {
    def optionLetterTable(letter: Char) = {
    val letterVal = letter.toInt
    val A = 'A'.toInt
    val M = 'M'.toInt
    if(letterVal >= A && letterVal <= 'L'.toInt) letterVal - (A-1)
    else if(letterVal >= M && letterVal <= 'X'.toInt) letterVal - (M-1)
    else -1
  }
  
    def reverseLetterTable(month: Int) = {
      val A = 'A'.toInt
      (month + (A-1)).toChar
    }
    def vencimento(year: Int, month: Int) = {
      val map2014 = new HashMap[Int, Int] {
        override def default(key:Int) = 17  
      }
      
      map2014(1) = 20
      map2014(4) = 22
      map2014(5) = 19
      map2014(6) = 18
      map2014(7) = 21
      map2014(8) = 18
      map2014(9) = 15
      map2014(10) = 20
      
      if(year==2014) {
        map2014(month)
      } else throw new Exception("Not implemented")
    }
}
