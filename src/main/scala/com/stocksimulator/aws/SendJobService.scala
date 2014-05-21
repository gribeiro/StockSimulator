package com.stocksimulator.aws
 import com.stocksimulator.main.ConfigurationModule._
import argonaut._
import Argonaut._
import scalaz._
import Scalaz._
import com.amazonaws.services.s3.model.ObjectMetadata
case class PreProcessInfo(id: String, days: List[String], symbols: List[String], param: List[ConfigParam], stringParam: Option[List[StringParam]])
object PreProcessInfo {
  implicit def paramCodec: CodecJson[PreProcessInfo] = casecodec5(PreProcessInfo.apply, PreProcessInfo.unapply)("id", "days", "symbols", "param", "stringParam")
  def load(s: String) = s.decodeOption[PreProcessInfo]
}

class SendJobService {
  self: ConfigComponent =>
 import ServicesManagement._

 import scala.concurrent._
 import ExecutionContext.Implicits.global
 import org.joda.time._
 import com.stocksimulator.reuters.FileManager
 
  val bucketName = self.queueNames.bucketName
  val queueName = self.queueNames.preprocessorInputQueue

  implicit def sqs = SL_SQS()
  implicit def s3 = SL_S3()
  val bucketOption = s3.bucket(bucketName)
  val filaOption = sqs.queue(queueName)
  
  def apply(config: Configuration) = {
   val javaFile = new java.io.File(config.javaFilename+".java")
   val name = config.name+DateTime.now().toString()
   val configFile = config.asJson.toString.toCharArray.map(_.toByte)
   val preProcessData = PreProcessInfo(name, config.dates, config.symbols, config.parameters, config.stringParam)
   val exists = javaFile.exists()
   val tryPut = for(bucket <- bucketOption; fila <- filaOption) yield {
     if(exists) {
    	 bucket.put("jobs/"+name+".java", javaFile)
    	 bucket.putObject("jobs/"+name+".json", configFile, new ObjectMetadata)
    	 fila.add(preProcessData.asJson.toString)
     } else throw new Exception("Java file does not exists...")
   }
 tryPut match {
    case Some(_) => println("Job was successful sent!")
    case None => throw new Exception("AWS Connection Error")
  }
  }
  
}