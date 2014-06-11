package com.stocksimulator.abs

import org.joda.time._
/*trait PartialContext

trait DatePC extends PartialContext {
  val datePartial: String
}

trait InstrumentsPC extends PartialContext {
  val intrumentsPartial: Set[Stock]
}

trait StdRatePC extends PartialContext{
  val stdRatePartial: Double
}
*/

object RunningContextModule {

  sealed trait RCStatus
  trait RCCreated extends RCStatus
  trait RCDateOK extends RCStatus
  trait RCInstrumentsOK extends RCStatus
  trait RCStdRateOK extends RCStatus
  trait RCMaxMinOK extends RCStatus
  type RCAllOk = RCCreated with RCDateOK with RCInstrumentsOK with RCStdRateOK with RCMaxMinOK

  def contextBuilder = new RunningContextBuilder[RCCreated]

  implicit def autoBuild(builder: RunningContextBuilder[RCAllOk]):RunningContext =  builder.build

  class RunningContextBuilder[T <: RCStatus](implicit ev1: T =:= RCCreated) {
    import com.stocksimulator.abs.StdRate._
    var date: Option[String] = None
    var instruments: Option[Set[Stock]] = None
    var stdRate: Option[Double] = None
    var max: Option[Iterable[(Stock, Double)]] = None
    var min: Option[Iterable[(Stock, Double)]] = None
    var id: Option[String] = None

    def withRunningContext(rc: RunningContext) = {
      date = Some(rc.date)
      instruments = Some(rc.instruments)
      stdRate = Some(rc.stdRate)
      max = Some(rc.max)
      min = Some(rc.min)
      id = Some(rc.id)

      this.asInstanceOf[RunningContextBuilder[RCAllOk]]      
    }

    def withId(_id: String) = {
      id = Some(_id)
      this.asInstanceOf[RunningContextBuilder[T]]
    }
    def withDate(_date: String) = {
      date = Some(_date)
      this.asInstanceOf[RunningContextBuilder[T with RCDateOK]]
    }
    
    def withDateAndProvisionedRate(_date: String) = this withDate(_date) withStdRate(rate(_date))
    
    def withInstruments(_stocks: Set[Stock]) =  {
      instruments = Some(_stocks)
      this.asInstanceOf[RunningContextBuilder[T with RCInstrumentsOK]]
    }
    def withStdRate(rate: Double) =  {
      stdRate = Some(rate)
      this.asInstanceOf[RunningContextBuilder[T with RCStdRateOK]]
    }

  	def withMaxMin(max: Iterable[(Stock, Double)], min: Iterable[(Stock, Double)]) = {
  	  this.max = Some(max)
  	  this.min = Some(min)
  	  this.asInstanceOf[RunningContextBuilder[T with RCMaxMinOK]]
  	}

    def build(implicit ev2: T =:= RCAllOk):RunningContext = {
      val outer = this
      new RunningContext {
        val date = outer.date.get
        val instruments = outer.instruments.get
        val stdRate = outer.stdRate.get
        val max = outer.max.get
        val min = outer.min.get
        override val id = outer.id.getOrElse("Normal")
      }
    }
  }

  trait RunningContext {
   val id: String = "Local"
   val date: String 
   val instruments: Set[Stock] 
   val stdRate: Double 
   val creationTime = DateTime.now()
   val max: Iterable[(Stock, Double)]
   val min: Iterable[(Stock, Double)]
 }
  

  trait PreContext {
    import java.net.InetAddress
    //val firerType: FirerType
    val tag: String = InetAddress.getLocalHost().getHostName() 
    val id: String
    val start = DateTime.now()
  }

}
