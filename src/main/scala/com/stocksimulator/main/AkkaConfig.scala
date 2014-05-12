package com.stocksimulator.main
import com.typesafe.config._

object AkkaConfig {
   trait AkkaConfigLike[T] {
	  def config(configurable: T): Config 
	}
   
}