package com.stocksimulator.aws
 import com.stocksimulator.main.ConfigurationModule._
import argonaut._
import Argonaut._
import scalaz._
import Scalaz._
import com.amazonaws.services.s3.model.ObjectMetadata
case class PreProcessInfo(id: String, days: List[String], symbols: List[String], param: List[ConfigParam])
object PreProcessInfo {
  implicit def paramCodec: CodecJson[PreProcessInfo] = casecodec4(PreProcessInfo.apply, PreProcessInfo.unapply)("id", "days", "symbols", "param")
  def load(s: String) = s.decodeOption[PreProcessInfo]
}

class SendJobService(bucketName: String, queueName: String) {
 import ServicesManagement._

 import scala.concurrent._
 import ExecutionContext.Implicits.global
 import org.joda.time._
 import com.stocksimulator.reuters.FileManager
 
  implicit val sqs = SL_SQS()
  implicit val s3 = SL_S3()
  val bucketOption = s3.bucket(bucketName)
  val filaOption = sqs.queue(queueName)
  def apply(config: Configuration) = {
   
   
   val javaFile = new java.io.File(config.javaFilename+".java")
   val name = config.name+DateTime.now().toString()
   val configFile = config.asJson.toString.toCharArray.map(_.toByte)
   val preProcessData = PreProcessInfo(name, config.dates, config.symbols, config.parameters)
   val exists = javaFile.exists()
   for(bucket <- bucketOption; fila <- filaOption) {
     if(exists) {
    	 bucket.put("jobs/"+name+".java", javaFile)
    	 bucket.putObject("jobs/"+name+".json", configFile, new ObjectMetadata)
    	 fila.add(preProcessData.asJson.toString)
     } else println("Java file does not exists...")
   }
 }
  
}