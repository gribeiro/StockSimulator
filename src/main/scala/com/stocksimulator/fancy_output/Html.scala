package com.stocksimulator.fancy_output
import com.stocksimulator.abs._
import scala.collection.mutable.HashMap
import org.joda.convert.ToString
import com.stocksimulator.debug.Log

import com.stocksimulator.abs.Position
object htmlCommon  {
  val header: String = {
    """
    <html><body>
    """
  }
  
  private def tagWrap(tag:String)(s: String) =  "<" + tag + ">" + s + "</" + tag + ">"
  private val td = tagWrap("td") _
  private val tr = tagWrap("tr") _
  private val table = tagWrap("table") _
  
  def orders(param: Parameters) = {
    val unwrapped = param.unwrap
    val tdList = for((oStr, oVal) <- unwrapped) yield {  oVal match {
        case rep: OrderResult => td(oStr) + td(rep.dateTime.toLocalTime().toString()) + td(rep.quantity.toString()) + td(rep.value.toString())
        case _ => ""
    }}
    val almostTable = tdList.map(tr).mkString("\n")
    table(almostTable) + unwrapped("position")
  }
  
  
  def numericPnl(param: Parameters) = {
    if(param.size > 0) {
    val unwrapped = param.unwrap
    val position = unwrapped("position").asInstanceOf[HashMap[Stock, Position]]
    val marketLast = unwrapped("marketLast").asInstanceOf[Map[Stock, StockInfo]]
    
   val nInfo =  for((lastStock, info) <- marketLast; (posStock, pos) <- position; if(posStock == lastStock)) yield {
      if(pos.quantity <= 0) {
        val price = info match {
          case q: Quote => q.bid.price
          case t: Trade => t.priceVol.price
        }
        
       (pos.pnl - price * pos.quantity * (-1))
      }
      else {
          val price = info match {
          case q: Quote => q.ask.price
          case t: Trade => t.priceVol.price
        }
      (pos.pnl + price * pos.quantity)
      }
    }
   
    nInfo.sum
    } else 0
  }
  
  def pnl(param: Parameters) = { 
    val unwrapped = param.unwrap
    val position = unwrapped("position").asInstanceOf[HashMap[Stock, Position]]
    val marketLast = unwrapped("marketLast").asInstanceOf[Map[Stock, StockInfo]]
    
   val nInfo =  for((lastStock, info) <- marketLast; (posStock, pos) <- position; if(posStock == lastStock)) yield {
      if(pos.quantity <= 0) {
        val price = info match {
          case q: Quote => q.bid.price
          case t: Trade => t.priceVol.price
        }
       td(lastStock.name) +  td((pos.pnl - price * pos.quantity * (-1)).toString)
      }
      else {
          val price = info match {
          case q: Quote => q.ask.price
          case t: Trade => t.priceVol.price
        }
       td(lastStock.name) + td((pos.pnl + price * pos.quantity).toString)
      }
    }
    
    table(tr(nInfo.mkString("\n")))
  }
  def input(param: Parameters) = {
    val unwrapped = param.unwrap
    
    table(unwrapped.map(pair => {
      val (key, value) = pair
      td(key) + td(value.toString())
    }).map(td).mkString("\n"))
  }
  val footer: String = {
    """
    </body></html>
    """
  }
}