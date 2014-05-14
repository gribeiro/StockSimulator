package com.stocksimulator.aws
import awscala.s3._
import awscala.sqs._

trait ConfigComponent {
  
  def queueNames: QueueNames
  
  trait QueueNames {
	  val errorQueueName: String
	  val preprocessorInputQueue: String
	  val runnerInputQueue: String
	  val outputInputQueue: String
	  val bucketName: String
	  val reutersInputQueue: String
  }
}

trait ProviderComponent {
  def providedSQS: SQS
  def providedS3: S3
}

trait DefaultProvider extends ProviderComponent {
  import ServicesManagement._
  def providedSQS = SL_SQS()
  def providedS3 = SL_S3()
}

trait DefaultConfig extends ConfigComponent {
  def queueNames = DefaultQueueNames
  object DefaultQueueNames extends QueueNames {
    val errorQueueName = "simul-erro"
    val preprocessorInputQueue = "simul-preproces"
    val runnerInputQueue = "simul-jobs"
    val outputInputQueue = "simul-result"
    val reutersInputQueue = "simul-reuters"
    val bucketName = "simulacoes-etp"
    
  }
}