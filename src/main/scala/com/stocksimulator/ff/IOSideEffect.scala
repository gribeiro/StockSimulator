package com.stocksimulator.ff

import scala.io.Source

trait Monad
trait IOSideEffect[T] { self =>
 def run: T
 def ++[U] (that: IOSideEffect[U]) = new IOSideEffect[U] {
   def run: U = {
	self.run
   	that.run
   }
 }
}

class FiletoStringIO(filename: String) extends IOSideEffect[String] {
  def run: String = {
     Source.fromFile(filename, "utf-8").getLines mkString "\n"
  }
  

}

class PrintIO(message: String) extends IOSideEffect[Unit] {
  def run: Unit = {
    println(message)
  }
}