package com.stocksimulator.abs

import scala.collection.mutable.ArrayBuffer

object AutoDemotion {
  implicit def autoDemote[U](promoted: Promotion[U]) = {
    promoted.demote
  }
}

trait PromotionInfo {

}


abstract class Promotion[U](baseElement: Promotable[U]) {
  def demote = baseElement.implicitDemote
}

abstract class Promotable[U](implicit uManifest: Manifest[U]) {
	val _promotions = ArrayBuffer.empty[PromotionInfo]
	
	def addPromotionInfo(promo: PromotionInfo):Unit = { 
	  _promotions += promo
	}
	
	def promotions = _promotions.toArray
	def promoteTo[T <: Promotion[U]](implicit tManifest : Manifest[T]) = { 
	  
	  tManifest.erasure.getConstructor(uManifest.erasure).newInstance(this).asInstanceOf[T]
	}
	 def implicitDemote = this.asInstanceOf[U]
	 
	 
}



