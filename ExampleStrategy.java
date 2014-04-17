
import com.stocksimulator.abs.*;
import com.stocksimulator.java_loader.*;

public class ExampleStrategy extends JavaAdapter {


	public ExampleStrategy() {
		super();
		// TODO Auto-generated constructor stub
		System.out.println("Strategy loaded...");

	}

	private Stock winc1, indc1;
	private Integer spread;
	private Integer hardLimit, myLimit;
	private double lastAskPrice, lastBidPrice;
	private TunnelTicketStatus orderBuy, orderSell;
	private double lastBuy, lastSell;
	private Boolean canExec;
	public JavaStdStrategy strat;

	public void onStart() {

		strat = strategy();
		winc1 = new Stock("WINc1");
		indc1 = new Stock("INDc1");
		System.out.println(strat.getIntParam("spread"));
		spread = strat.getIntParam("spread");
		hardLimit = strat.getIntParam("hardLimit");
		lastAskPrice = 0;
		lastBidPrice = 0;
		myLimit = 0;
		lastBuy = 0;
		lastSell = 0;
		canExec = true;
	}

	private int roundUp(double n) {
		int rounded = (int) Math.ceil(n);
		if (rounded % 5 != 0)
			return rounded + (5 - rounded % 5);
		return rounded;
	}

	private int roundDown(double n) {
		int rounded = (int) Math.floor(n);
		return rounded - rounded % 5;
	}

	private double precoCompra(double bidPrice, double askPrice) {
		return (bidPrice + askPrice) / 2 - (spread / 2);
	}

	private double precoVenda(double bidPrice, double askPrice) {
		return (bidPrice + askPrice) / 2 + spread / 2;
	}
	@Override
	public void callback() {
		
	}
	@Override
	public void onSellReport(Stock s, int vol, double price) {
		
	}
	@Override
	public void onBuyReport(Stock s, int vol, double price) {
		
	}
	@Override
	public void onQuotes() {
		StockInfo indcInfo = strat.getSymbol(indc1);
		System.out.println("onQuotes!!!!;");
		if (indcInfo instanceof Quote) {
			Quote q = (Quote) indcInfo;
			Position pos = strat.getPosition(winc1);
			int qtd = pos.quantity();
			double bidPrice = q.bid().price();
			double askPrice = q.ask().price();
			if (bidPrice <= 0 || askPrice <= 0)
				return;
			int buyPrice = roundDown(precoCompra(bidPrice, askPrice));
			int sellPrice = roundUp(precoVenda(bidPrice, askPrice));

			if (qtd < hardLimit) {
				strat.provideBuyLiquidity(winc1, 1, buyPrice);
			}
			if (qtd > -hardLimit) {
				strat.provideSellLiquidity(winc1, 1, sellPrice);
			}

		}
		
	}



}
