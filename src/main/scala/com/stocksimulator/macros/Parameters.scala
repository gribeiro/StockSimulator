package com.stocksimulator.macros


case class ParamMaker(name: String)

object PMaker {

  implicit class ParametersHelper(val sc: StringContext) extends AnyVal {
    def par(args: Any*):ParamMaker = {
      ParamMaker("")
    }
  }

}