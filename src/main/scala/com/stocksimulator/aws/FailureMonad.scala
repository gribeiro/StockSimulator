package com.stocksimulator.aws
import scalaz._
import Scalaz._
import com.stocksimulator.debug.LogNames._
object Result {
 type Error[+A] = \/[String, A]
 type Result[+A] = OptionT[Error, A]
 
 
 
 def emptyErr[A] = none[A].point[Error]
 
 
 def err[A](str: String) = {
   OptionT(str.left : Error[Option[A]])
 }
 
 def ok[A](elem: A) = elem.point[Result]
 
 implicit class ResultGather[A](result: Result[A]) {
   def gatherErr = {
    val run =  result.run.fold(
         l = err => "Run error: " + err , 
         r = ok => "Run ok")
     this.log(run)
   }
   
 }
 
 implicit class Option2Result[T](opt: Option[T]) {
 
   def result(str: String) = {
     opt match {
       case Some(a) => ok(a)
       case None => err(str)
     }
   }
 }
 
 
 
}

