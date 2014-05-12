package com.stocksimulator.reuters

import resource._
import java.io._
import java.util.Scanner
import scala.collection.mutable.ArrayBuffer
import com.stocksimulator.helpers.RingBuffer
import scala.io.Source
import org.joda.time.DateTime
import java.util.zip.DeflaterInputStream
import java.util.zip.InflaterInputStream
import java.util.zip.DeflaterOutputStream
import scala.collection.mutable.LinkedHashMap
import com.stocksimulator.abs._
import com.stocksimulator.debug.Log
import java.nio.file.{ Paths, Files }
import com.stocksimulator.helpers.RingBuffer
import com.etp._
import scalaz._
import Scalaz._
import com.stocksimulator.debug.LogNames._

trait Credentials {
  def username: String
  def password: String
}

trait CurrentReutersCredentials extends Credentials {
  def username = "andre@allianceasset.com.br"
  def password = "fiveware456"
}

class ReutersFiles { self: Credentials =>
  val conn = new RTHConnector

  def apply(symbols: Array[String], date: String): String = {
    val filename = conn.downloadMultiTAQFile(symbols, date, username, password)
    "TAQ" + filename.split("TAQ").last
  }

  def apply(symbols: Array[String], date: List[String]): Iterable[String] = {
    for (d <- date) yield apply(symbols, d)
  }

}

case class FileFormat(val ric: String, val datetime: Long, val gmt: Int, val sType: Boolean, val price: Double, val volume: Int, val bidPrice: Double, val bidSize: Int, val askPrice: Double, val askSize: Int) extends Ordered[FileFormat] {

  def compare(that: FileFormat) = {
    datetime.compare(that.datetime)
  }
}

object FileManager {
  import scala.concurrent._
  import ExecutionContext.Implicits.global
  import akka.actor._
  import com.stocksimulator.parallel.ParCommon
  import akka.pattern.ask
  import akka.util.Timeout
  import scala.concurrent.Await
  import scala.concurrent.duration._
  implicit val timeout = Timeout(100.days)
  
  
  val config = ParCommon.config
  val system = ActorSystem("FileMS", config)
  
  val fileActor =  system.actorOf(Props(classOf[FileActor]))
  
  trait FileManaging
  case class OpenFile(filename: String) extends FileManaging
  case class SaveFile(fileName: String, fileContent: Array[FileFormat]) extends FileManaging
  case class SaveFromMemory(data: Array[Byte], fileName: String) extends FileManaging
  case class ReadSymbolTable(filename: String) extends FileManaging

  class FileActor extends Actor {

    def readSymbolTable(fileName: String) = {
      val fileMonad = managed(new FileInputStream(fileName))

      val bufferMonad = fileMonad.map(new BufferedInputStream(_)).map(new InflaterInputStream(_)).map(new BufferedInputStream(_)).map(new DataInputStream(_))
      var loadData = new ArrayBuffer[String]
      bufferMonad.acquireAndGet { input =>
        val bla = input.readInt
        val ricTableSz = input.readInt
        for (j <- 1 to ricTableSz) yield {
          loadData += input.readUTF()
        }
        input.close
      }
      loadData.toArray
    }
    def saveFromMemory(data: Array[Byte], fileName: String) = {
      val fileMonad = managed(new FileOutputStream(fileName))
      fileMonad.acquireAndGet {
        output =>
          output.write(data)
          output.close()
      }

    }

    def open(fileName: String):Array[FileFormat] = {
      val fileMonad = managed(new FileInputStream(fileName))
      val bufferMonad = fileMonad.map(new InflaterInputStream(_)).map(new DataInputStream(_))
      var loadData = new RingBuffer[FileFormat](0)
      bufferMonad.acquireAndGet { input =>
        val size = input.readInt
        val ricTableSz = input.readInt
        val ricTable = for (j <- 1 to ricTableSz) yield {
          input.readUTF()
        }
        loadData = new RingBuffer[FileFormat](size)
        val allData = for (i <- 1 to size) yield {
          val ric = ricTable(input.readInt)
          val datetime = input.readLong
          val gmt = input.readInt
          val sType = input.readBoolean
          if (!sType) {
            val price = input.readDouble
            val volume = input.readInt
            loadData += FileFormat(ric, datetime, gmt, sType, price, volume, 0, 0, 0, 0)
          } else {
            val bidPrice = input.readDouble
            val bidSize = input.readInt
            val askPrice = input.readDouble
            val askSize = input.readInt
            loadData += FileFormat(ric, datetime, gmt, sType, 0, 0, bidPrice, bidSize, askPrice, askSize)
          }
        }
        input.close
        loadData.toArray
      }
    }

    def save(fileName: String, fileContent: Array[FileFormat]) = {
      val ricArray = fileContent.par.groupBy(_.ric).keySet.toArray
      val fileMonad = managed(new FileOutputStream(fileName))
      val bufferMonad = fileMonad.map(new DeflaterOutputStream(_)).map(new DataOutputStream(_))
      val loadData = ArrayBuffer.empty[FileFormat]
      bufferMonad.acquireAndGet { output =>
        val size = fileContent.size
        output.writeInt(size)
        //outputRIC TABLE
        output.writeInt(ricArray.size)
        ricArray.foreach {
          ric =>
            output.writeUTF(ric)
        }
        //
        fileContent.foreach { formated =>
          output.writeInt(ricArray.indexOf(formated.ric))
          output.writeLong(formated.datetime)
          output.writeInt(formated.gmt)
          output.writeBoolean(formated.sType)
          if (!formated.sType) {
            output.writeDouble(formated.price)
            output.writeInt(formated.volume)
          } else {
            output.writeDouble(formated.bidPrice)
            output.writeInt(formated.bidSize)
            output.writeDouble(formated.askPrice)
            output.writeInt(formated.askSize)
          }

        }
        output.close
      }
    }
    def receive = {
      case OpenFile(filename) =>
        val opened = open(filename)
        this.log(opened.getClass())
        sender ! open(filename)
      case SaveFile(filename, fileContent) =>
        sender ! save(filename, fileContent)
      case SaveFromMemory(data, filename) =>
        sender ! saveFromMemory(data, filename)
      case ReadSymbolTable(filename) =>
        sender ! readSymbolTable(filename)
    }
  }

  val dateFormat = ReutersCommon.dateFormat
  def datExtension(filename: String): String = filename.split("""\.""").head + ".dat"

  def downloadReuters(symbols: Array[String], date: String): String = {
    val hcReuters = new ReutersFiles with CurrentReutersCredentials
    val all = symbols.map {
      str => Stock(str).checkOption(date).name
    }
    all.foreach {
      str => println(str)
    }

    hcReuters.apply(all, date)
  }

  def downloadReutersOption(symbols: Array[String], date: String): Option[String] = {

    try {
      val tryDownloadReuters = downloadReuters(symbols, date)
      val testeFileExists = fileExists(tryDownloadReuters)
      val length = (new File(tryDownloadReuters)).length() / (1024)

      if (testeFileExists && length >= 100) Some(tryDownloadReuters) else None
    } catch {
      case e: Exception =>

        this.log("Reuters download failed...")
        this.log(e)
        None

    }
  }

  def generatedFilename(symbols: Array[String], date: String): String = {
    val symb = symbols.mkString("").replace('.', '_').toUpperCase()
    val datest = date.replace('/', '_')

    (List("TAQ", symb, datest) mkString "_") + ".dat"
  }

  def fileExists(filename: String) = {
    Files.exists(Paths.get(filename))
  }

  def datFileIO(filename: String) = {
    val datFile = datExtension(filename)

    if (fileExists(datFile)) {
      new java.io.File(datFile)
    } else {
      val load = transform(filename)
      save(datFile, load)
      new java.io.File(datFile)
    }
  }
  def apply(filename: String, from: Option[String] = None, to: Option[String] = None) = {
    val datFile = datExtension(filename)

    val transformed = if (fileExists(datFile)) {
      open(datFile)
    } else {
      val load = transform(filename)
      save(datFile, load)
      load
    }

    val allTransf: Array[Map[Stock, StockInfo]] = transformed.map {
      fFormat =>
        val stock = Stock(fFormat.ric)
        val datetime = new DateTime(fFormat.datetime)
        if (fFormat.sType) {
          val bid = PriceVol(fFormat.bidPrice, fFormat.bidSize)
          val ask = PriceVol(fFormat.askPrice, fFormat.askSize)
          Map(stock -> Quote(stock, bid, ask, datetime))
        } else {
          val priceVol = PriceVol(fFormat.price, fFormat.volume)
          Map(stock -> Trade(stock, priceVol, datetime))
        }
    }

    allTransf
    //result.foreach(println(_))
    //result.toArray
  }
  def memoryGetter[@specialized(Int, Double) T](transf: (String) => T)(stock: Stock, s: String, memVector: LinkedHashMap[String, T])(implicit num: Numeric[T]): T = {
    try {
      val newVal = transf(s)
      memVector(stock) = newVal
      newVal
    } catch {
      case e: NumberFormatException =>
        memVector.getOrElse(stock, num.zero)
    }
  }

  private def intGetter(stock: Stock)(s: String, memVector: LinkedHashMap[String, Int]) = memoryGetter[Int](s => s.toInt)(stock, s, memVector)
  private def doubleGetter(stock: Stock)(s: String, memVector: LinkedHashMap[String, Double]) = memoryGetter[Double](s => s.toDouble)(stock, s, memVector)
  def transform(fileName: String) = {
    val lastBidPrice = new LinkedHashMap[String, Double]
    val lastBidVol = new LinkedHashMap[String, Int]
    val lastAskPrice = new LinkedHashMap[String, Double]
    val lastAskVol = new LinkedHashMap[String, Int]
    val lines = Source.fromFile(fileName, "utf-8").getLines
    val loadData = ArrayBuffer.empty[FileFormat]
    var i = 0
    val size = 100000
    lines.drop(1)
    val memory =
      while (lines.hasNext) {
        val buffer = new RingBuffer[String](size)
        while (lines.hasNext && buffer.size < size) {
          buffer += lines.next
          i += 1
        }
        // futures += future {
        buffer.foreach {
          elem =>
            val content = elem.split(",", -1)
            //println(content.toList)
            val ric = content(0)
            val intGet = intGetter(ric) _
            val doubleGet = doubleGetter(ric) _
            val dtime = Array(content(1), content(2)) mkString " "
            val datetime = DateTime.parse(dtime, dateFormat).getMillis()
            val gmt = content(3).toInt
            val sType = content(4) == "Quote"
            val price = if (sType) 0 else content(5).toDouble
            val volume = if (sType) 0 else content(6).toInt
            val bidPrice = doubleGet(content(7), lastBidPrice)
            val bidSize = intGet(content(8), lastBidVol)
            val askPrice = doubleGet(content(9), lastAskPrice)
            val askSize = intGet(content(10), lastAskVol)
            if (bidPrice < askPrice)
              loadData += FileFormat(ric, datetime, gmt, sType, price, volume, bidPrice, bidSize, askPrice, askSize)
        }

      }
    val sorted = loadData.sortBy(_.datetime)
    sorted.toArray
  }
  def loadOnMemory(fileName: String) = {
    val fileMonad = managed(new FileInputStream(fileName))
    val outputArr = new ByteArrayOutputStream()
    val outputMonad = managed(outputArr)

    for (input <- fileMonad; output <- outputMonad) {
      val buffer = new Array[Byte](512)
      def read(): Unit = input.read(buffer) match {
        case -1 => ()
        case n => output.write(buffer, 0, n); read()
      }
      read
    }
    outputArr.toByteArray
  }

  
  def loadInSequence[T](fm: FileManaging):T = {
     val fut = ask(fileActor, fm)
	 val await = Await.result(fut, timeout.duration).asInstanceOf[T]
	await
  }
  
  def saveFromMemory(data: Array[Byte], fileName: String) = {
	  loadInSequence[Unit](SaveFromMemory(data, fileName))
  }
  
  def readSymbolTable(fileName: String):Array[String] = {
	 loadInSequence[Array[String]](ReadSymbolTable(fileName))
  }
  def open(fileName: String):Array[FileFormat] = {
	  val loaded = loadInSequence[Array[FileFormat]](OpenFile(fileName))
	  //loaded.foreach(p => println(p))
	  loaded
  }
  def save(fileName: String, fileContent: Array[FileFormat]) = {
	  loadInSequence[Unit](SaveFile(fileName, fileContent))
  }
}