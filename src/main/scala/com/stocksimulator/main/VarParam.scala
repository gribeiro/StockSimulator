package com.stocksimulator.main
import com.stocksimulator.abs._
import com.stocksimulator.main.ConfigurationModule._
import com.stocksimulator.helpers.ParamGen._
object VarParam {
  implicit def strStr2Parameters(strStr: (String, String)): Parameters = {
    val newP = new Parameters
    newP.set(strStr._1, strStr._1)
    newP
  }
  def addParam(multi: List[Array[Parameters]]): Array[Parameters] = {
    multi.reduce(addParam(_, _))
  }
  def addParam(a: Array[Parameters], b: Array[Parameters]) = {
    for (aP <- a; bP <- b) yield {
      val newP = new Parameters
      List(aP.mem, bP.mem).foreach {
        all =>
          all.foreach {
            xs => newP.set(xs._1, xs._2)
          }
      }
      newP
    }
  }
  def apply(confPar: List[ConfigParam]): Array[Parameters] = confPar.map(p => (p.base, p.to.getOrElse(p.base), p.by.getOrElse(1.0), p.name)).getParamArray

  def getStrs(stringPar: List[StringParam]): Array[Parameters] = {

    val mapParams = Utils.perm(stringPar.map {
      case StringParam(k, list) => list.map { (k, _) }
    }.flatten)
    val paramsMapped = mapParams.map {
      subList =>
        val newParam = new Parameters
        subList.foreach {
          case (k, v) => newParam.set(k, v)
        }
        newParam
    }.toArray
    paramsMapped
  }

  def apply(confPar: List[ConfigParam], stringPar: List[StringParam]): Array[Parameters] = {
    addParam(apply(confPar), getStrs(stringPar))
  }
  def apply(conf: Configuration): Array[Parameters] = {
    val numericParams = conf.parameters.map(p => (p.base, p.to.getOrElse(p.base), p.by.getOrElse(1.0), p.name)).getParamArray
    conf.stringParam match {
      case Some(strPrm) =>
        val paramsMapped = getStrs(strPrm)
        addParam(numericParams, paramsMapped)

      case None => numericParams
    }
  }
  def apply(conf: Configuration, filter: List[String]): Array[Parameters] = {
    val varparam = apply(conf)
    val filterParams = varparam.filter {
      vp =>
        filter.contains(vp.inputStr)
    }
    //println(filterParams.length)
    filterParams
  }
}
