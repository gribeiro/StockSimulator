import com.stocksimulator.helpers._

import com.stocksimulator.abs._
import com.stocksimulator.abs.AutoDemotion._
import com.github.nscala_time.time.Imports._
import com.stocksimulator.java._
import com.stocksimulator.java_loader._

object CSVTest {;import org.scalaide.worksheet.runtime.library.WorksheetSupport._; def main(args: Array[String])=$execute{;$skip(533); 
  val stringToCompile = """
   
      public class HelloWorld {
       private class B {
    		
      }
      private class Teste {}
    	public void main() {
    	System.out.println("It Worked");
        B teste = new B();
      System.out.println(teste);
       }
      }
      """;System.out.println("""stringToCompile  : String = """ + $show(stringToCompile ));$skip(49); val res$0 = 
   MemoryCompiler("HelloWorld", stringToCompile);System.out.println("""res0: Object = """ + $show(res$0))}
}
