package com.stocksimulator.aws
import awscala.s3._
import awscala.sqs._

trait ConfigComponent {

  def queueNames: QueueNames
  def bucketNames: BucketNames
  trait QueueNames {
    val errorQueueName: String
    val preprocessorInputQueue: String
    val runnerInputQueue: String
    val outputInputQueue: String
    val reutersInputQueue: String
    val mysqlInputQueue: String
  }

  trait BucketNames {
    val bucketName: String
  }

  class ProvidedQueueNames extends QueueNames {

    protected val pre = ""
    lazy val errorQueueName = pre + "simul-erro"
    lazy val preprocessorInputQueue = pre + "simul-preproces"
    lazy val runnerInputQueue = pre + "simul-jobs"
    lazy val outputInputQueue = pre + "simul-result"
    lazy val reutersInputQueue = pre + "simul-reuters"
    lazy val mysqlInputQueue = pre + "simul-mysql"
  }

  class ProvidedBucketNames extends BucketNames {
    lazy val bucketName = "simulacoes-etp"
  }
  object DefaultQueueNames extends ProvidedQueueNames
  object DefaultBucketNames extends ProvidedBucketNames

}

object ProviderComponent {
  import ProviderComponent._

  def doSQS[T](provider: ProviderComponent)(fun: (SQS) => T): T = {
    val sqs = provider.providedSQS
    fun(sqs)
  }

  def doS3[T](provider: ProviderComponent)(fun: (S3) => T): T = {
    val s3 = provider.providedS3
    fun(s3)
  }
}
trait ProviderComponent {
  def providedSQS: SQS
  def providedS3: S3

}

class DefaultProvider extends ProviderComponent {
  import ServicesManagement._
  def providedSQS = SL_SQS()
  def providedS3 = SL_S3()
}

trait DefaultConfig extends ConfigComponent {
  def queueNames: QueueNames = DefaultQueueNames
  def bucketNames: BucketNames = DefaultBucketNames
}

object TestConfig {
  protected val errorTestName = "errorTest42"
  protected val preprocessorTestName = "preprocessorTest42"
  protected val runnerTestName = "workerTest42"
  protected val outputTestName = "outputTest42"
  protected val reutersTestName = "reutersTest42"
  protected val mysqlTestName = "mysqlTest42"
  protected val listAllQueues = List(errorTestName, preprocessorTestName, runnerTestName, outputTestName, reutersTestName, mysqlTestName)

  protected def doSQS[T] = ProviderComponent.doSQS[T](new DefaultProvider) _

  def create = doSQS {
    sqs =>
      allQueues.foreach {
        _ match {
          case Left(name) => sqs.createQueue(name)
          case _ => {}
        }
      }
  }

  def allQueues = doSQS[List[Either[String, Queue]]] {
    sqs =>
      listAllQueues.map { name =>
        sqs.queue(name) match {
          case Some(q) => Right(q)
          case None => Left(name)
        }
      }
  }

  def remove = doSQS {
    sqs =>
      allQueues.foreach {
        _ match {
          case Right(queue) => sqs.delete(queue)
          case _ => {}
        }
      }
  }

}
trait TestConfig extends DefaultConfig {

  override def queueNames = TestQueueNames
  object TestQueueNames extends ProvidedQueueNames {
    //override protected val pre = "TESTE-"

    TestConfig.create
    private val errorTestName = TestConfig.errorTestName
    private val preprocessorTestName = TestConfig.preprocessorTestName
    private val runnerTestName = TestConfig.runnerTestName
    private val outputTestName = TestConfig.outputTestName
    private val reutersTestName = TestConfig.reutersTestName
    private val mysqlTestName = TestConfig.mysqlTestName
    
    override lazy val errorQueueName: String = errorTestName
    override lazy val preprocessorInputQueue: String = preprocessorTestName
    override lazy val runnerInputQueue: String = runnerTestName
    override lazy val outputInputQueue: String = outputTestName
    override lazy val reutersInputQueue: String = reutersTestName
    override lazy val mysqlInputQueue: String = mysqlTestName
  }
}