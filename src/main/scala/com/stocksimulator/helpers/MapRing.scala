package com.stocksimulator.helpers

class MapRing[A,B](size: Int) extends Map[A, B] {
  
  
  protected val repre = new RingBuffer[(A,B)](size)
  def get(key: A) = repr.collectFirst {case (k,v) if(k==key) => v}
  def iterator = repr.iterator
  
  def + [B1 >: B](kv: (A, B1)): MapRing[A, B1] = {
    val nMap = new MapRing[A, B1](size)
    nMap.repre ++= repre
    nMap.repre += kv
    nMap
  }
  
  def - (key: A): MapRing[A, B] = {
    val nMap = new MapRing[A, B](size)
    nMap.repre ++= repre.filter {case (k,v) => k != key}
    nMap
  }
  
}