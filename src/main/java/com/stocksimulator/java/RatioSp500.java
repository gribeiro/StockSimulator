package com.stocksimulator.java;
import com.stocksimulator.abs.*;
import com.stocksimulator.common_strategies.*;

public class RatioSp500 extends RatioArbitrerStrategy {
	public RatioSp500(Market market, Parameters param) {
		super(market, param);
		// TODO Auto-generated constructor stub
	}
	// new Stock("INDc1"), new Stock("ISPc1"), 5, 20, 
	@Override
	public Stock symbolA() {
		// TODO Auto-generated method stub
		return null;
	}

	@Override
	public Stock symbolB() {
		// TODO Auto-generated method stub
		//Bla
		return null;
	}

	@Override
	public int gran() {
		// TODO Auto-generated method stub
		return 0;
	}

	@Override
	public int maxPos() {
		// TODO Auto-generated method stub
		return 0;
	}

	@Override
	public int step() {
		// TODO Auto-generated method stub
		return 0;
	}
}
