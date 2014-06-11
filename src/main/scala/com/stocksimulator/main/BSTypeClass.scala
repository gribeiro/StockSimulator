package com.stocksimulator.main

import com.stocksimulator.abs.Parameters

object BSTypeClass {
  trait BSLike[T] {
    def bootstrap(bootstrapable: T): List[(Parameters, Parameters)]
  }
  
    implicit object BSLikeRemoteJava extends BSLike[RemoteJavaBSSet] {
      def bootstrap(bootstrapable: RemoteJavaBSSet) = {
        val generator = bootstrapable.generator
        bootstrapable.bootstrap.run(generator).toList
      }
    }
    
    implicit object BSLikeJavaBSSet extends BSLike[JavaBSSet] {
      def bootstrap(bootstrapable: JavaBSSet) = {
        val generator = bootstrapable.generator
        bootstrapable.bootstrap.run(generator).toList
      }
    }
  

}