import org.scalatest.FlatSpec
import org.scalamock.scalatest.MockFactory
import com.stocksimulator.abs._
import com.stocksimulator.abs.EventTC._
import org.joda.time.DateTime
class ProvideLiquidityTest extends FlatSpec with MockFactory  {
	  val stockTest = Stock("teste")
    val qtd = 1
    val price = 1.1
    val dt = DateTime.now()
    def ticket(e: Event) = {
	    e match {
	      case Buy => Ticket(100, BuyOrder(dt, stockTest, qtd, price))
	      case Sell => Ticket(100, SellOrder(dt, stockTest, qtd, price))
	    }
	  }
    def waiting(e: Event) = Wait(100, ticket(e))
  
	def ready(e: Event) = Ready(ticket(e))
	
  "Provide Liquidity" should "Buy properly when no tickets are present" in {
    val strategy = mock[BuySellAdapter]
    val provideLiquidty = new StrategyProvideLiquidity(strategy)
    
    inSequence {
      (strategy.getStatus _ ).expects(Killed).returning(Killed)
      (strategy.buy _).expects(Stock("teste"), 1, 1.1)
    }
    provideLiquidty.provideLiquidity(Stock("teste"), Buy, 1, 1.1)
  }
  
  it should "Sell properly when no tickets are present" in {
  val strategy = mock[BuySellAdapter]
    val provideLiquidty = new StrategyProvideLiquidity(strategy)
    
    inSequence {
      (strategy.getStatus _ ).expects(Killed).returning(Killed)
      (strategy.sell _).expects(Stock("teste"), 1, 1.1)
    }
    provideLiquidty.provideLiquidity(Stock("teste"), Sell, 1, 1.1)
    
  }
  
  it should "Do nothing if SELL was placed with same quantity and value" in {
	 val strategy = mock[BuySellAdapter]
    val provideLiquidty = new StrategyProvideLiquidity(strategy)
    
    inSequence {
      (strategy.getStatus _ ).expects(Killed).returning(Killed)
      (strategy.sell _).expects(stockTest, qtd, price).returning(waiting(Sell))
      (strategy.getStatus _).expects(waiting(Sell)).returning(waiting(Sell))
    }
    provideLiquidty.provideLiquidity(stockTest, Sell, qtd, price)
    provideLiquidty.provideLiquidity(stockTest, Sell, qtd, price)
  }
  
  
  it should "Do nothing if BUY was placed with same quantity and value" in {
	 val strategy = mock[BuySellAdapter]
    val provideLiquidty = new StrategyProvideLiquidity(strategy)
    
    inSequence {
      (strategy.getStatus _ ).expects(Killed).returning(Killed)
      (strategy.buy _).expects(stockTest, qtd, price).returning(waiting(Buy))
      (strategy.getStatus _).expects(waiting(Buy)).returning(waiting(Buy))
    }
    provideLiquidty.provideLiquidity(stockTest, Buy, qtd, price)
    provideLiquidty.provideLiquidity(stockTest, Buy, qtd, price)
  }
  
  it should "Complain when incompatible natures are detected" in {
    val strategy = mock[BuySellAdapter]
    val provideLiquidty = new StrategyProvideLiquidity(strategy)
    
    inSequence {
      (strategy.getStatus _ ).expects(Killed).returning(Killed)
      (strategy.buy _).expects(stockTest, qtd, price).returning(waiting(Sell))
      (strategy.getStatus _ ).expects(waiting(Sell)).returning(waiting(Sell))
      
    }
    provideLiquidty.provideLiquidity(stockTest, Buy, qtd, price)
    intercept[IncompatibleNatureException] {
    	provideLiquidty.provideLiquidity(stockTest, Buy, qtd+1, price)
    }
  }
  
  
  it should "Append buy when another order is waiting" in {
    val strategy = mock[BuySellAdapter]
    val provideLiquidty = new StrategyProvideLiquidity(strategy)
    inSequence {
      (strategy.getStatus _ ).expects(Killed).returning(waiting(Buy))
      (strategy.appendWaitOrder _).expects(waiting(Buy), qtd, price, Buy, stockTest)
    }
    provideLiquidty.provideLiquidity(stockTest, Buy, qtd, price)
    
  
  } 
  
  it should "Append sell when another order is waiting" in {
    val strategy = mock[BuySellAdapter]
    val provideLiquidty = new StrategyProvideLiquidity(strategy)
   
    inSequence {
      (strategy.getStatus _ ).expects(Killed).returning(waiting(Sell))
      (strategy.appendWaitOrder _).expects(waiting(Sell), qtd, price, Sell, stockTest)
    }
    provideLiquidty.provideLiquidity(stockTest, Sell, qtd, price)
    
  
  }
  
  it should "Replace properly a Sell order" in {
    val strategy = mock[BuySellAdapter]
    val provideLiquidity = new StrategyProvideLiquidity(strategy)
    val e = Sell
    inSequence {
      (strategy.getStatus _).expects(Killed).returning(ready(e))
      (strategy.replaceSell _).expects(ticket(e), qtd, price)
    }
    
    provideLiquidity.provideLiquidity(stockTest, e, qtd, price)
  }  
  it should "Replace properly a Buy order" in {
    val strategy = mock[BuySellAdapter]
    val provideLiquidity = new StrategyProvideLiquidity(strategy)
    val e = Buy
    inSequence {
      (strategy.getStatus _).expects(Killed).returning(ready(e))
      (strategy.replaceBuy _).expects(ticket(e), qtd, price)
    }
    
    provideLiquidity.provideLiquidity(stockTest, e, qtd, price)
  }  
}