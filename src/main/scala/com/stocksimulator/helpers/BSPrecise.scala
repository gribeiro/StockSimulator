package com.stocksimulator.helpers

import scala.collection.mutable.HashMap



object BSPrecise {
  import spire.implicits._
  import spire.math._
  import scalaz._, Scalaz._
  import Memoization._
  
  implicit def double2BD(x: Double) = BigDecimal(x)
  implicit def numToBD[T: Numeric](x: T) = {
    val num = implicitly[Numeric[T]]
    num.toBigDecimal(x)
  }

  def bsVolCall[T: Numeric]( _strike: T, _r: T, _timeToExpiry: T)(_optionPrice: T, _spot: T) = {

    val spot: BigDecimal = _spot
    val strike: BigDecimal = _strike
    val r: BigDecimal = _r
    val optionPrice: BigDecimal = _optionPrice
    val timeToExpiry: BigDecimal = _timeToExpiry

    var volatility = BigDecimal(0.25)
    var n = 1
    val nmax = 100
    val error = BigDecimal(0.001)
    var dv = error + 1

    while (dv.abs > error & n <= nmax) {
      val timeSqrt = sqrt(timeToExpiry)
      for(d1 <- getD1(spot, strike, r, timeToExpiry, volatility, timeSqrt); d2 = getD2(d1, volatility, timeSqrt) ) {
      val dif = priceEuropeanBlackScholesCall(spot, strike, r, timeToExpiry, d1, d2) - optionPrice
      val vega = calcVegaCall(d1, spot, timeSqrt) / 0.01
      dv = dif / vega
      
      //val teste =  abs(volatility-dv)/volatility
      volatility = volatility - dv
      //println(volatility.doubleValue)
      if(volatility.doubleValue == Double.NegativeInfinity || volatility.doubleValue == Double.PositiveInfinity || dv.abs > 10E20) {
        volatility = 0
        n = nmax+1
      }
      }
      n = n + 1
    }

    //println(volatility)
    volatility
  }

  def bsGreeksCall[T: Numeric](_strike: T, _r: T, _timeToExpiry:T)(_optionPrice: T, _spot: T) = {
    val volatility = bsVolCall(_strike, _r, _timeToExpiry)(_optionPrice, _spot)
    val timeToExpiry:BigDecimal = _timeToExpiry
    val timeSqrt = sqrt(timeToExpiry)
    val d1:BigDecimal = getD1[BigDecimal](_spot, _strike, _r, _timeToExpiry, volatility, timeSqrt).getOrElse(0.0)
    Array(volatility, cdf(d1))
  }
  def calcVegaCall[T: Numeric](_d1: T, _spot: T, _timeSqrt:T) = {
    val d1:BigDecimal = _d1
    val spot:BigDecimal = _spot
    val timeSqrt:BigDecimal = _timeSqrt
    
    spot*timeSqrt*ndf(d1)*0.01
  }
  
  def priceEuropeanBlackScholesPut[T: Numeric](_spot: T, _strike: T, _r: T, _timeToExpiry: T, _d1: T, _d2: T) = {
    val spot: BigDecimal = _spot
    val strike: BigDecimal = _strike
    val r: BigDecimal = _r
    val timeToExpiry: BigDecimal = _timeToExpiry
    val d1: BigDecimal = _d1
    val d2: BigDecimal = _d2
    
    val callValue = priceEuropeanBlackScholesCall(spot, strike, r, timeToExpiry, d1, d2)
    strike*exp(-r*timeToExpiry) - spot + callValue
  }

  def priceEuropeanBlackScholesPut[T: Numeric](_spot: T, _strike: T, _r: T, _timeToExpiry: T, _volatility: T): Option[BigDecimal] = {

    val spot: BigDecimal = _spot
    val strike: BigDecimal = _strike
    val r: BigDecimal = _r
    val timeToExpiry: BigDecimal = _timeToExpiry
    val volatility: BigDecimal = _volatility

    val timeSqrt: BigDecimal = sqrt(timeToExpiry)
for(d1 <- getD1(spot, strike, r, timeToExpiry, volatility, timeSqrt); d2 = getD2(d1, volatility, timeSqrt)) yield {
  strike*exp(-r*timeToExpiry)*cdf(-d2) - spot*cdf(-d1)
}
  }
  
  
  def priceEuropeanBlackScholesCall[T: Numeric](_spot: T, _strike: T, _r: T, _timeToExpiry: T, _d1: T, _d2: T) = {
    val spot:BigDecimal = _spot
    val strike:BigDecimal = _strike
    val r:BigDecimal = _r
    val timeToExpiry:BigDecimal = _timeToExpiry
    val d1:BigDecimal = _d1
    val d2:BigDecimal = _d2
    
    spot*cdf(d1) - (strike*exp(-r*timeToExpiry)*cdf(d2))
  }
  
  def priceEuropeanBlackScholesCall[T: Numeric](_strike: T, _r: T, _timeToExpiry: T)(_spot: T, _volatility: T) = {
    val spot: BigDecimal = _spot
    val strike: BigDecimal = _strike
    val r: BigDecimal = _r
    val timeToExpiry: BigDecimal = _timeToExpiry
    //println(_volatility)
    val volatility: BigDecimal = _volatility
    
    val timeSqrt = sqrt(timeToExpiry)
for( d1 <- getD1(spot, strike, r, timeToExpiry, volatility, timeSqrt); d2 = getD2(d1, volatility, timeSqrt)) yield {
    spot*cdf(d1) - (strike*exp(-r*timeToExpiry)*cdf(d2))
}
    
  }
  def getD1[T: Numeric](_spot: T, _exercise: T, _r: T, _time: T, _volatility: T, _timeSqrt: T): Option[BigDecimal] = {
    val spot: BigDecimal = _spot
    val exercise: BigDecimal = _exercise
    val r: BigDecimal = _r
    val time: BigDecimal = _time
    val volatility: BigDecimal = _volatility
    val timeSqrt: BigDecimal = _timeSqrt
    
    try {
    Some(((log(spot / exercise) + (r * time)) / (volatility * timeSqrt)) + (BigDecimal(0.5) * volatility * timeSqrt))
    }
    catch {
      case e: Exception => None
    }
  }

  def getD2[T: Numeric](_d1: T, _volatility: T, _timeSqrt: T): BigDecimal = {
    val d1: BigDecimal = _d1
    val volatility: BigDecimal = _volatility
    val timeSqrt: BigDecimal = _timeSqrt

    d1 - (volatility * timeSqrt)
  }


val cdf = ringedMemo(2000) {
  x: BigDecimal => precdf(x)
}
 
  def precdf[T: Numeric](_z: T) = {
    val z: BigDecimal = _z
    
      
    val zabs = z.abs
    val p0:BigDecimal = 220.2068679123761
    val p1:BigDecimal  = 221.2135961699311
    val p2:BigDecimal  = 112.0792914978709
    val p3:BigDecimal  = 33.91286607838300
    val p4:BigDecimal  = 6.373962203531650
    val p5:BigDecimal  = .7003830644436881
    val p6:BigDecimal  = .3526249659989109E-01

    val q0:BigDecimal  = 440.4137358247522
    val q1:BigDecimal  = 793.8265125199484
    val q2:BigDecimal  = 637.3336333788311
    val q3:BigDecimal  = 296.5642487796737
    val q4:BigDecimal  = 86.78073220294608
    val q5:BigDecimal  = 16.06417757920695
    val q6:BigDecimal  = 1.755667163182642
    val q7:BigDecimal  = .8838834764831844E-1

    val cutoff:BigDecimal  = 7.071
    val root2pi:BigDecimal = 2.506628274631001
    
    val res:BigDecimal = if(z > 37.0) 1.0 else if(z < -37.0) 0.0 else {
     val expntl = exp(BigDecimal(-.5) * zabs * zabs)
     val pdf = expntl / root2pi
     
     val p = if(zabs < cutoff)  expntl * ((((((p6 * zabs + p5) * zabs + p4) * zabs + p3) * zabs + p2) * zabs + p1) * zabs + p0) / (((((((q7 * zabs + q6) * zabs + q5) * zabs + q4) * zabs + q3) * zabs + q2) * zabs + q1) * zabs + q0);
     		else  pdf / (zabs + BigDecimal(1.0) / (zabs + BigDecimal(2.0) / (zabs + BigDecimal(3.0) / (zabs + BigDecimal(4.0) / (zabs + 0.65)))));
     if(z < 0.0) p else -p + (1.0)
    }
    res
    
  }
 //implicit val teste = memoizeFun1(this.prendf[BigDecimal] _)
  
  val ndf = ringedMemo(2000) {
    x:BigDecimal => prendf(x)
  }
  //val ndf = memoize(this.prendf[BigDecimal] _)
  def prendf[T: Numeric](_input: T) = {
    val input:BigDecimal = _input
    (1/sqrt(2*BigDecimal(Math.PI))) * exp(-pow(input, 2)/2)
  }
}