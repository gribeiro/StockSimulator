package com.stocksimulator.java;

import com.stocksimulator.abs.*;


public class CommonStrategy extends Strategy  {
	
	protected Stock vale3, vale5;
	protected int quantity;
	protected Double pnl;
	protected Integer counter;
	protected MovingAvg avg3, avg5;

	
	
	public CommonStrategy(Market market, Parameters param) {
		super(market, param);
		vale3 = new Stock("VALE3.SA");
		vale5 = new Stock("VALE5.SA");
	}

	public Double getPnl() {
		return pnl;
	}
	
	public Integer getQuantity() {
		return new Integer(quantity);
	}
	@Override
	public void callback() {
	
	}
	@Override
	public void onStart() {
		quantity = 0;
		pnl = 0.0;
		counter = 10;
		Integer param = (Integer)getParam("myParam");
		avg3 = createMAvg(vale3, 50, param);
		avg5 = createMAvg(vale5, 50, param);
	
	}

	@Override
	public void onStop() {
		//putResult("bla", obj);
	}

	@Override
	public void onQuotes() {
		Double d3, d5, diff;
		d3 = (Double)avg3.lastVal();
		d5 = (Double)avg5.lastVal();
		diff = d5 -  0.9*d3;
	
		StockInfo infoVale = getSymbol(vale3);
		if(infoVale instanceof Quote) {
			Quote q = (Quote)infoVale;
			double bprice = q.bid().price();
			double aprice = q.ask().price();
			if(counter > 0 && diff >0){
				
				buy(vale3, 100, bprice);
				counter--;
			}
			else if(counter > -10 && diff < 0) {
				sell(vale3, 100, aprice);
			}
		}
	}

	@Override
	public void onBuyReport(Stock stock, int volume, double price) {
		//print("Buy successful! Volume: " + volume + " Price: " + price);
		if(counter < 10) sell(stock, 100, price+0.01);
		counter++;
		
	}

	@Override
	public void onSellReport(Stock stock, int volume, double price) {
		//print("Sell successful! Volume: " + volume + " Price: " + price);
		if(counter > 0) buy(stock, 100, price-0.01);
		counter--;
	}

}
