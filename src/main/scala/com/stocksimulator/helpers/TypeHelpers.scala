package com.stocksimulator.helpers

object TypeHelpers {
  type N[A] = A => Nothing
  type NN[A] = N[N[A]]
  
  trait Disj[T] {
    type or[S] = Disj[T with N[S]]
    type apply = N[T]
  }

  type disj[T] = {
    type or[S] = Disj[N[T]]#or[S]
  }
  
}