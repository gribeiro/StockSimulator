package com.stocksimulator.helpers
import io.jvm.uuid._
import com.stocksimulator.abs._
import scala.collection.mutable.ListBuffer
import scala.collection.mutable.LinkedHashMap
import org.joda.time.DateTime

object ImplicitClasses {
  
implicit class extraOperators(x: Double)  {
  def over(that: Double):Double = {
    if(that != 0.0) x/that else 0.0
  }
}
  implicit class IterableUtils[T](xs: Iterable[T]) {
    def has(y: T) = {
        xs.exists {
        p => p == y
      }
    }
  }

  implicit class TicketListUtils(xs: Iterable[Ticket]) {
    def buyAndSellPartition = {
      xs.partition(t => {
        t.order match {
          case _: BuyOrder => true
          case _ => false
        }
      })
    }
    
    def buys = {
      xs.filter {
        t =>
          t.order match {
            case _: BuyOrder => true
            case _ => false
          }
      }
    }
    
    def sells = {
      xs.filter {
        t =>
          t.order match {
            case _: SellOrder => true
            case _ => false
          }
      }
    }
    
    def getStock(st: Stock) = {
      xs.filter(t => t.order.stock == st)
    }
    def ! (datetime: DateTime, vol: Int, price: Double) = executeOrders(datetime, vol, price)
    def executeOrders(datetime: DateTime, vol: Int, price: Double) = {
      xs.map(t => 
      t.order.nature match {
        case BuyNature => t -> BuyOrderResult(datetime, math.min(t.order.quantity, vol), price)
        case SellNature => t -> SellOrderResult(datetime, math.min(t.order.quantity, vol), price)
      } 
      )
    }
    
    
    
    def ~>= (value: Double) = {
      xs.filter(t => t.order.value >= value)
    }
    
    def ~<= (value: Double) = {
      xs.filter(t => t.order.value <= value)
    }
    
    def ~== (value: Double) = {
      xs.filter(t => t.order.value == value)
    }
    
    def ~> (value: Double) = {
      xs.filter(t => t.order.value > value)
    }
    
    def ~< (value: Double) = {
       xs.filter(t => t.order.value < value)
    }
  }
  
  implicit class BookUtils(xs: LinkedHashMap[Ticket, Book]) {
    def ? (price: Double) = validTickets(price)
    def validTickets(price: Double) = {
      xs.filter {
      case (lTicket, lBook) => {
        lTicket.order.value == price && lBook.canExecute
      }
    }.keys
    }
    
  }
  
  implicit def stockInfo2UUID(a: StockInfo): UUID = a.uuid

}