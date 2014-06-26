package com.stocksimulator.module

object ModuleMonoid {
 import scalaz._, Scalaz._
 
 trait ComposableModule {
   def run: Unit
 }
 
 implicit object SimpleModuleMonoid extends Monoid[ComposableModule] {
   def zero: ComposableModule = new ComposableModule {def run = {}}
   def append(a: ComposableModule, b: => ComposableModule) = new ComposableModule {
     def run = a.run;b.run;
   }
 }
 
 trait SimpleModuleOf[T] {
   def simpleModule: ComposableModule
 }
 
 
}