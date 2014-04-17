package com.stocksimulator.utils;

import scala.Function0;


public abstract class MovingAverageAdapter implements Function0<Object> {

	@Override
	public Object apply() {
		// TODO Auto-generated method stub
		return null;
	}

	@Override
	public byte apply$mcB$sp() {
		// TODO Auto-generated method stub
		return 0;
	}

	@Override
	public char apply$mcC$sp() {
		// TODO Auto-generated method stub
		return 0;
	}

	@Override
	public double apply$mcD$sp() {
		return insert();
	}

	@Override
	public float apply$mcF$sp() {
		// TODO Auto-generated method stub
		return 0;
	}

	@Override
	public int apply$mcI$sp() {
		// TODO Auto-generated method stub
		return 0;
	}

	@Override
	public long apply$mcJ$sp() {
		// TODO Auto-generated method stub
		return 0;
	}

	@Override
	public short apply$mcS$sp() {
		// TODO Auto-generated method stub
		return 0;
	}

	@Override
	public void apply$mcV$sp() {
		// TODO Auto-generated method stub
		
	}

	@Override
	public boolean apply$mcZ$sp() {
		// TODO Auto-generated method stub
		return false;
	}
	
	public abstract double insert() ;

}
