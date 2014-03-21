package com.stocksimulator.fancy_output

import scala.collection.mutable.HashMap
import com.stocksimulator.abs._
import scala.xml._

class XMLFormat(in: Parameters, out: Parameters, id: String, sId: String) {
  import scala.collection.JavaConversions._
  private val inContent = in.unwrap.toMap
  private val outContent = out.unwrap.toMap
  private val pnl = htmlCommon.numericPnl(out)
  private def tag(name: String, text: String) = Elem(null, name, Null, TopScope, Text(text))
  private def unescape(text:String):String = {
            def recUnescape(textList:List[Char],acc:String,escapeFlag:Boolean):String= {
                textList match {
                    case Nil => acc
                    case '&'::tail => recUnescape(tail,acc,true)
                    case ';'::tail if (escapeFlag) => recUnescape(tail,acc,false)
                    case 'a'::'m'::'p'::tail if (escapeFlag) => recUnescape(tail,acc+"\\",true)
                    case 'q'::'u'::'o'::'t'::tail if (escapeFlag) => recUnescape(tail,acc+"\"",true)
                    case 'l'::'t'::tail if (escapeFlag) => recUnescape(tail,acc+"<",true)
                    case 'g'::'t'::tail if (escapeFlag) => recUnescape(tail,acc+">",true)
                    case x::tail => recUnescape(tail,acc+x,true)
                    case _ => acc
                }
            }
            recUnescape(text.toList,"",false)

        }
  private def resultMaker[T](m: Map[String, T]) = {

    val xmlList = m.map {
      (pair2) =>
        val (str2, obj2) = pair2
        
        tag(str2, obj2.toString)
    }

    xmlList

  }

  val orders = {
    val list = for ((oStr, oVal) <- outContent) yield {
      oVal match {
        case rep: OrderResult => <order type={rep.iType}> {<datetime>{rep.dateTime.toLocalTime().toString()}</datetime><quantity>{rep.quantity.toString()}</quantity><value>{rep.value.toString()}</value> } </order>
        case _ => <order type="empty"></order>
      }

    }
    list mkString "\n"
  }

  def output = """<?xml version="1.0" encoding="UTF-8"?>""" +  unescape(<strategy sID={sId} simID={id}>{ unescape(List(<Input>{resultMaker(inContent)}</Input>, <PNL>{pnl}</PNL>, <Orders>{orders}</Orders>).mkString("\n") )} </strategy> + "")

  
  def pureXML = XML.load(output)
}