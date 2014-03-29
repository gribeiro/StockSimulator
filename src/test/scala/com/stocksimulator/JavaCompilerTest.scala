package com.stocksimulator
import com.stocksimulator.java_loader._
import com.stocksimulator.debug.Log
import org.scalatest._
import Matchers._
class CompilerTest extends FlatSpec { 
 
  "A MemoryCompiler" should "Compile a hello world" in {
    val stringToCompile = """
      public class HelloWorld {
    	public static void main(String args[]) {
    	System.out.println("It Worked");
       }
      }
      """
     
     //val (_, success) = MemoryCompiler("HelloWorld", stringToCompile)
     //success should equal (true)
  }
}