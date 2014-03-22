package com.stocksimulator.main

import com.stocksimulator.reuters.MongoConfig
import com.stocksimulator.abs.Stock
import com.stocksimulator.abs.Parameters
import com.stocksimulator.common_strategies.RubyRatioStrategy
import com.stocksimulator.common_strategies.RubyDoubleRatioStrategy
import com.stocksimulator.java.TestStrategy
import com.stocksimulator.common_strategies.RubyStdStrategy

abstract class BSAdapter {
  def mConfig: MongoConfig
  def name: String
  def actorsQtd: Int
  def myInst: Set[Stock]
  def bookOrder: Int
  def watchSymbol: Array[String]
  def rbFilename: String
  def rbKlass: String
  def varParam: Array[Parameters]
  def from: String
  def to: String
  def strategyType: String
  def replace: Boolean
  def postRun(): Unit
  

}

trait StrategyTypes {

}