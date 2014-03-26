package com.stocksimulator.fancy_output

import com.stocksimulator.abs.Parameters
import com.mongodb.casbah.Imports._
import com.stocksimulator.abs.OrderResult
import com.stocksimulator.abs.BuyOrderResult
import com.stocksimulator.abs.SellOrderResult
import com.stocksimulator.abs.EmptyOrderResult
import com.stocksimulator.abs.Utils
import com.stocksimulator.debug.Log
import org.joda.time.DateTime
import com.stocksimulator.abs.Stock
import com.stocksimulator.abs.StockInfo
import com.stocksimulator.abs.Quote
import scala.collection.mutable.HashMap
import com.stocksimulator.abs.Position
import com.stocksimulator.abs.Trade
import com.stocksimulator.main._


abstract class ExtraInfoGenerator[T] {
  protected def pnl(out: Parameters) = htmlCommon.numericPnl(out)
  def apply(in: Parameters, out: Parameters):T
}



object SortinoGenerator extends ExtraInfoGenerator[Double] {
	 def apply(in: Parameters, out: Parameters):Double = {
	  0.0
	}
}

object SharpeGenerator extends ExtraInfoGenerator[Double] {
  def apply(in: Parameters, out: Parameters): Double = {
   val inContent = in.unwrap.toMap
  val outContent =  out.unwrap.toMap
     val sharpe = {
    val list = for ((oStr, oVal) <- outContent) yield {
      oVal match {
        case rep: OrderResult => rep
        case _ => EmptyOrderResult
      }

    }

    val sorted = list.toList.filter(p => p.value != 0).sortBy(f => f.dateTime.getMillis())
    val ret = sorted.scanLeft((0.0,0)) {
      (memory, nextResult) =>
        val (localPNL, quantity) = memory
        nextResult match {
          case b: BuyOrderResult => (quantity*nextResult.value - localPNL, quantity+nextResult.quantity)
          case s: SellOrderResult => (quantity*nextResult.value - localPNL, quantity-nextResult.quantity)
          case _ => (0, quantity)
        }
    }
    val localPNLs = (for(r <- ret) yield r._1) ++ List(ret.last._2 * pnl(out) - ret.last._1)
    val res = localPNLs.drop(1).toVector
    
    val sum = res.sum
    val count = res.size
    val mean = sum/count
    
    val squares = res.map(f => (f-mean)*(f-mean))
    val std = Math.sqrt(squares.sum/count)
    if(std != 0)  
    	sum/std
    else 10
  }
   sharpe
  }
}

class ExtraInfoFarm[T](in: Parameters, out: Parameters) {
  val mem = new HashMap[String, ExtraInfoGenerator[T]]
  
  def register(name: String, gen: ExtraInfoGenerator[T]) = {
    mem += name -> gen
  }
  
  def get() = {
   (for((name, generator) <- mem) yield name -> generator(in, out)).to[List]
  }
}



class MongoOutput(in: Parameters, out: Parameters, id: String, sId: String) {
  private val inContent = in.unwrap.toMap
  private val outContent = if(out == null) Map.empty[String, Object] else out.unwrap.toMap
  
  private val pnl2 = htmlCommon.numericPnl(out)
  private val inputHash = Utils.md5Hash(inContent.toString)
  private val infoFarm = new ExtraInfoFarm[Double](in, out)
  
  infoFarm.register("sortino", SortinoGenerator)
  infoFarm.register("sharpe", SharpeGenerator)
 // Log(outContent)
  var date = outContent("date").asInstanceOf[String]
  val mongoOBJs = {
    val list = for ((oStr, oVal) <- outContent) yield {
      oVal match {
        case rep: OrderResult =>
             date =  (List(rep.dateTime.dayOfMonth().get(), rep.dateTime.monthOfYear().get(), rep.dateTime.year().get()).mkString("/"))
          MongoDBObject("Order" -> rep.iType, "DateTime" -> rep.dateTime.toString(), "Quantity" -> rep.quantity, "Value" -> rep.value)
       
        case _ => MongoDBObject("Order" -> "Empty")
      }

    }
    
    val md5 = outContent.get("md5").orElse(Some("N/A")).get.asInstanceOf[String]

    val inputStr = in.inputStr
    //Log(RBSFactory.mongoOutputSymbol)
    val tempStock = outContent.get("marketLast").get.asInstanceOf[Map[Stock, StockInfo]].get(RBSFactory.mongoOutputSymbol).get
    
    val PNLStock = RBSFactory.mongoOutputSymbol
    val position = outContent.get("position").get.asInstanceOf[HashMap[Stock, Position]]
    val marketLast = (tempStock, position.get(PNLStock).get.quantity) match {
      case (a:Quote, pos) => if(pos >=0) a.bid.price else a.ask.price
      case (t:Trade, _) => t.priceVol.price
    }
    
    val qtd = position.get(PNLStock).get.quantity
    val caixa = position.get(PNLStock).get.pnl
    val acc = if(qtd<=0) qtd * marketLast else qtd*marketLast 
    val pnl = caixa + acc
    val results = List("Orders" -> list, "sID" -> sId, "simID" -> id, "Input" -> inContent, "PNL" -> pnl, "marketLast" -> marketLast, "md5" -> md5, "inputHash" -> inputHash, "inputStr" -> inputStr, "date" -> date) ++ infoFarm.get()
    MongoDBObject(results)
  }

  def output = mongoOBJs

}