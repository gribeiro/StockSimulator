package com.stocksimulator.helpers


case class ParamMaker(name: String)

object PMaker {

  implicit class ParametersHelper(val sc: StringContext) extends AnyVal {
    def p(args: Any*):ParamMaker = {
      ParamMaker(sc.parts mkString "")
    }
  }

}