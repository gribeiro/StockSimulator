package com.stocksimulator.output

object DataTransformModule {

  import org.apache.commons.codec.binary._
  import com.stocksimulator.remote.ByteArrayToObject
  import com.mongodb.BasicDBObject
  import java.io.InputStream
  
  case class ResultDBLine(sID: String) 
  
  def readBase64(base64: InputStream) = {
    val datBA = new String(org.apache.commons.io.IOUtils.toByteArray(base64).map(_.toChar))
    val body = datBA.map(_.toByte).toArray
    val decoded = Base64.decodeBase64(body)
    ByteArrayToObject[BasicDBObject](decoded)
  }
  
  
  
}