package com.stocksimulator.abs

object AutoDemotion {
  implicit def autoDemote[U](promoted: Promotion[U]) = {
    promoted.demote
  }
}

class Promotion[U](baseElement: Promotable[U]) {
  def demote = baseElement.implicitDemote
}

class Promotable[U](implicit uManifest: Manifest[U]) {
	def promoteTo[T <: Promotion[U]](implicit tManifest : Manifest[T]) = { 
	  
	  tManifest.erasure.getConstructor(uManifest.erasure).newInstance(this).asInstanceOf[T]
	}
	 def implicitDemote = this.asInstanceOf[U]
}



