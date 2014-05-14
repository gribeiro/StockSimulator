package com.stocksimulator.aws




object CredentialManagement {
  import awscala._
  import Roles._
  import awscala.sqs._
  trait CredentialFor[-T] {
    def myCredentials(implicit role: Role): Credentials
  }

  object CredentialFor {
    val default = Credentials("AKIAJGVIWCI2TCVAGIPQ", "Zgc2rhgZlSsTl7buvTTNyBIfpUvM2CG+MX4hIoqs")
    implicit object CredForAnybody extends CredentialFor[Any] {
      def myCredentials(implicit role: Role) = {
        role match {
          case _ => default
        }
      }
    }
  }

}

object DefaultRegionForRoleManagement {
  import Roles._
  import awscala._
  import awscala.Region._
  import awscala.sqs._
  import com.amazonaws.{ regions => awsregions }
  trait DefaultRegionFor[-T] {
    def region(implicit role: Role): Region
  }
  
  object DefaultRegionFor {
    implicit object testRoleRegion extends DefaultRegionFor[Any] {
      def region(implicit role: Role) = {
        role match {
          case TestRole => awsregions.Region.getRegion(awsregions.Regions.US_WEST_2)
        }    
      }
    }
  }
}



object Roles {
  sealed trait Role
  case class UserDefinedRole(name: String) extends Role
  case object TestRole extends Role
  case object ProductionRole extends Role
  implicit val defaultRole = TestRole

}


object ServicesManagement {
import awscala._, sqs._, s3._, simpledb._, dynamodbv2._
import CredentialManagement._
import DefaultRegionForRoleManagement._
import Roles._




trait ServiceLoaderFor[T] {
  def at(region: Region)(implicit ev: CredentialFor[T]):T
  
  def apply()(implicit evRegion: DefaultRegionFor[T], ev: CredentialFor[T]) = {
    val region = evRegion.region
    at(region)
  }
}




object SL_SQS extends ServiceLoaderFor[SQS] {
 
  def at(region: Region)(implicit ev: CredentialFor[SQS]) = {
   
    val credentials = ev.myCredentials
    (new SQSClient(credentials)).at(region)
  }
}

object SL_S3 extends ServiceLoaderFor[S3] {
  def at(region: Region)(implicit ev: CredentialFor[S3]) = {
   
    val credentials = ev.myCredentials
   (new S3Client(credentials)).at(region)
  }
}

object SL_SimpleDB extends ServiceLoaderFor[SimpleDB] {
    def at(region: Region)(implicit ev: CredentialFor[SimpleDB]) = {
   
    val credentials = ev.myCredentials
    (new SimpleDBClient(credentials)).at(region)
  }
}

object SL_DynamoDB extends ServiceLoaderFor[DynamoDB] {
     def at(region: Region)(implicit ev: CredentialFor[DynamoDB]) = {
   
    val credentials = ev.myCredentials
    (new DynamoDBClient(credentials)).at(region)
  }
}


}
