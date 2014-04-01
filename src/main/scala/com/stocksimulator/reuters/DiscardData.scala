package com.stocksimulator.reuters

import com.stocksimulator.abs._
import scala.collection.mutable.ArrayBuffer

trait TypeNode
case object SizeNode extends TypeNode
case object PriceNode extends TypeNode

case class DiscardData(stock: Stock, node: TypeNode)