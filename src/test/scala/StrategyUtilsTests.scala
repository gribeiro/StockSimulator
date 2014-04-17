import com.stocksimulator.abs._
import com.stocksimulator.common_strategies._
import org.scalatest._
import com.stocksimulator.helpers.PMaker._
import com.stocksimulator.helpers._
class StrategyUtilsTests extends FlatSpec with Matchers {
	"(Integer rounding) A roundUp factory" should "round up 22.3 to 25 as expected when rounded by 5" in {
	 val roundUpFunction = StrategyUtils.roundUpFactory(5)
	 roundUpFunction.apply(22.3) should be (25)
	}
	it should "do nothing when rounding 25.0 by 5" in {
	  val roundUpFunction = StrategyUtils.roundUpFactory(5)
	  roundUpFunction.apply(25.0) should be (25.0)
	}
	
	"(Integer rounding) A roundDown factory" should "round down 22.3 to 20 as expected when rounded by 5" in {
	 val roundDownFunction = StrategyUtils.roundDownFactory(5)
	 roundDownFunction.apply(22.3) should be (20)
	}
	it should "do nothing when rounding 25.0 by 5" in {
	  val roundDownFunction = StrategyUtils.roundDownFactory(5)
	  roundDownFunction.apply(25.0) should be (25.0)
	}
	"(Fixedpoint rounding) A roundUp factory" should "round up 22.3 to 22.5 as expected when rounded by 2.5" in {
	 val roundUpFunction = StrategyUtils.roundUpFactory(2.5)
	 roundUpFunction.apply(22.3) should be (22.5)
	}
	it should "do nothing when rounding 25.0 by 2.5" in {
	  val roundUpFunction = StrategyUtils.roundUpFactory(2.5)
	  roundUpFunction.apply(25.0) should be (25.0)
	}
	
	"(Fixedpoint rounding) A roundDown factory" should "round down 22.3 to 20.0 as expected when rounded by 2.5" in {
	 val roundDownFunction = StrategyUtils.roundDownFactory(2.5)
	 roundDownFunction.apply(22.3) should be (20.0)
	}
	it should "do nothing when rounding 25.0 by 2.5" in {
	  val roundDownFunction = StrategyUtils.roundDownFactory(2.5)
	  roundDownFunction.apply(25.0) should be (25.0)
	}
	
	"Param maker helper interpolation of p\"abc\"" should "be equal to ParamMaker(\"abc\")" in {
	  val test = p"abc"
	  test.name should be ("abc")
	} 
	
	
}