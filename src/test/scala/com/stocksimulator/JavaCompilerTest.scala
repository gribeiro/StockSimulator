package com.stocksimulator
import com.stocksimulator.java_loader._
import com.stocksimulator.debug.Log
import org.scalatest._
import Matchers._

class CompilerTest extends FlatSpec { 
 
  "A MemoryCompiler" should "Compile a hello world" in {
    val stringToCompile = """
      public class HelloWorld {
        private class B {
    		
      }
    	public int main() {
    	System.out.println("It Worked");
        B teste = new B();
      System.out.println(teste);
      return 23;
       }
      }
      """
    val teste = MemoryCompiler("HelloWorld", stringToCompile)

  }
}