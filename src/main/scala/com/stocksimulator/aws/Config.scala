package com.stocksimulator.aws

trait ConfigComponent {
  
  def queueNames: QueueNames
  
  trait QueueNames {
	  val errorQueueName: String
	  val preprocessorInputQueue: String
	  val runnerInputQueue: String
	  val outputInputQueue: String
	  val bucketName: String
  }
}



trait DefaultConfig extends ConfigComponent {
  def queueNames = DefaultQueueNames
  object DefaultQueueNames extends QueueNames {
    val errorQueueName = "simul-erro"
    val preprocessorInputQueue = "simul-preprocess"
    val runnerInputQueue = "simul-jobs"
    val outputInputQueue = "simul-result"
    val bucketName = "simulacoes-etp"
  }
}