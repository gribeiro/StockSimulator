
import scala.pickling._
import json._
import com.stocksimulator.abs._

object CSVTest {;import org.scalaide.worksheet.runtime.library.WorksheetSupport._; def main(args: Array[String])=$execute{;$skip(119); 
val pckl = Stock("oii").pickle;System.out.println("""pckl  : scala.pickling.json.JSONPickle = """ + $show(pckl ))}

}
