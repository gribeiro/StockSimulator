package com.stocksimulator.helpers

import com.stocksimulator.abs.Parameters
import scala.collection.mutable.ArrayBuffer

object ParamGen {
  
  implicit class ParamGenerator(options: List[(Double, Double, Double, String)]) {

    def getParamArray: Array[Parameters] = {
      val params = ArrayBuffer.empty[Parameters]
      def recursiveFor(list: List[(Double, Double, Double, String)], fun: (Parameters) => Unit): Unit = {

        list match {
          case List(x) =>
            val (from, to, step, name) = x
            for (i <- from to to by step) {
              val param = new Parameters
              fun(param)
              param.set(name, i.asInstanceOf[Object])
              params += param
            }
          case x :: xs =>
            val (from, to, step, name) = x
            for (i <- from to to by step) {
              def myFun(param: Parameters): Unit = {
                fun(param)
                param.set(name, i.asInstanceOf[Object])
              }
              recursiveFor(xs, myFun)
            }

        }
      }
      recursiveFor(options, (p: Parameters) => {})
      params.toArray
    }
  }

}

