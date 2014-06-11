package com.stocksimulator.helpers

object Memoization {
  import scalaz._, Scalaz._, Memo._
  
  def ringedMemo[K, V](size: Int): Memo[K, V] = {
    val mem = new RingBuffer[(K, V)](size)
    
    def get(x: K) = mem.collectFirst {case k if(k==x) => k}
    
    memo[K, V] {
      f => k => {
        get(k) match {
          case Some(v) => v._2
          case None => 
            val v = f(k)
            mem += (k, v)
            v
        }
      }
    }
  }
}
