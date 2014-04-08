package com.stocksimulator.utils;

public class BlackScholes {
/*
	public static void main(String[] args) {
		double days = 18;
		double daysPerYear = 365;
		double timeToExpiry = days / daysPerYear;
		double spot = 35.80;
		double strike = 37.09;
		double r = 0.10;
		double optionPrice = 0.61;
		double volatility = 0.49;
		System.out.println("timeToExpiry: " + timeToExpiry);
		//PETR4\H1122.BOV	22	1.69	100	1.67	1.69	16700	24.5698	1.5000	0.9623	0.0850	0.0103	-0.0089	n/a	17:12
		
		optionPrice = BlackScholes.priceEuropeanBlackScholesPut(spot, strike, r, timeToExpiry, volatility);
		
		//System.out.println("put: " + optionPrice);		
		
		//double auxvol = BS.bsVolPut(spot, strike, r, optionPrice, timeToExpiry);
		//System.out.println(auxvol);
		//double voldelta[] = BS.bsGreeksPut(spot, strike, r, optionPrice, timeToExpiry);
		double vol = BlackScholes.bsVolCall(spot, strike, r, optionPrice, timeToExpiry);
		System.out.println("vol: " + vol);
		//System.out.println("delta: " + voldelta[1]);
		//double callPrice = priceEuropeanBlackScholesCall(spot, strike, r, timeToExpiry, vol);
		
		//System.out.println("callPrice: " + callPrice);
		
		//System.out.println(voldelta[0]);
		//System.out.println(voldelta[1]);

	}*/

	public static double bsVolCall(double spot, double strike, double r, double optionPrice, double timeToExpiry) {
		//System.out.println("bsVol: spot: " + spot);
		double volatility = 0.25;
		int n = 1;
		int nmax = 100;
		double error = 0.001;
		double dv = error + 1;
		double d1 = 0;
		double d2 = 0;
		while (Math.abs(dv) > error & n <= nmax) {
			double timeSqrt = Math.sqrt(timeToExpiry);
			d1 = getD1(spot, strike, r, timeToExpiry, volatility, timeSqrt);
			d2 = getD2(d1, volatility, timeSqrt);
			double dif = priceEuropeanBlackScholesCall(spot, strike, r, timeToExpiry, d1, d2) - optionPrice;

			double vega = calcVegaCall(d1, spot, timeSqrt) / 0.01;
			dv = dif / vega;
			volatility = volatility - dv;
			n = n + 1;
		}

		return volatility;
	}
	
	public static double bsVolPut(double spot, double strike, double r, double optionPrice, double timeToExpiry) {
		//System.out.println("bsVol: spot: " + spot);
		double volatility = 0.25;
		int n = 1;
		int nmax = 100;
		double error = 0.001;
		double dv = error + 1;
		double d1 = 0;
		double d2 = 0;
		while (Math.abs(dv) > error & n <= nmax) {
			double timeSqrt = Math.sqrt(timeToExpiry);
			d1 = getD1(spot, strike, r, timeToExpiry, volatility, timeSqrt);
			d2 = getD2(d1, volatility, timeSqrt);
			double dif = priceEuropeanBlackScholesPut(spot, strike, r, timeToExpiry, d1, d2) - optionPrice;

			double vega = calcVegaCall(d1, spot, timeSqrt) / 0.01;
			dv = dif / vega;
			volatility = volatility - dv;
			n = n + 1;
		}

		return volatility;
	}
	
	
	public static double[] bsGreeksPut(double spot, double strike, double r, double optionPrice, double timeToExpiry) {
		//System.out.println("bsGreeks: spot: " + spot);
		double volatility = bsVolPut(spot, strike, r, optionPrice, timeToExpiry);
		double timeSqrt = Math.sqrt(timeToExpiry);
		double d1 = getD1(spot, strike, r, timeToExpiry, volatility, timeSqrt);
		return new double[] { volatility, cdf(d1) };
	}
	
	
	

	public static double[] bsGreeksCall(double spot, double strike, double r, double optionPrice, double timeToExpiry) {
		//System.out.println("bsGreeks: spot: " + spot);
		double volatility = bsVolCall(spot, strike, r, optionPrice, timeToExpiry);
		double timeSqrt = Math.sqrt(timeToExpiry);
		double d1 = getD1(spot, strike, r, timeToExpiry, volatility, timeSqrt);
		return new double[] { volatility, cdf(d1) };
	}

	/**
	 * delta = dCallVal/dSpot
	 */
	public static final double calcDeltaCall(double d1) {
		return cdf(d1);
	}

	/**
	 * Partial wrt volatility - dc/dVolatility.
	 */
	public static double calcVegaCall(double d1, double spot, double timeSqrt) {
		double vega = spot * timeSqrt * ndf(d1);
		return 0.01 * vega;
	}
	
	private static double priceEuropeanBlackScholesPut(double spot, double strike, double r,double timeToExpiry, double d1, double d2) {
		double callValue = priceEuropeanBlackScholesCall(spot, strike, r,timeToExpiry, d1, d2);
		double putValue = (strike * Math.exp(-r * timeToExpiry)) - spot + callValue;
		return putValue;
	}
	
	public static double priceEuropeanBlackScholesPut(double spot, double strike, double r,double timeToExpiry, double volatility) {
		double timeSqrt = Math.sqrt(timeToExpiry);
		double d1 = getD1(spot, strike, r, timeToExpiry, volatility, timeSqrt);
		double d2 = getD2(d1, volatility, timeSqrt);
		double putValue = (strike * Math.exp(-r * timeToExpiry) * cdf(-d2)) - (spot * cdf(-d1));
		return putValue;
	}
	

	private static double priceEuropeanBlackScholesCall(double spot, double strike, double r,double timeToExpiry, double d1, double d2) {
		double callValue = (spot * cdf(d1)) - (strike * Math.exp(-r * timeToExpiry) * cdf(d2));
		return callValue;
	}
	
	public static double priceEuropeanBlackScholesCall(double spot, double strike, double r,double timeToExpiry, double volatility) {
		double timeSqrt = Math.sqrt(timeToExpiry);
		double d1 = getD1(spot, strike, r, timeToExpiry, volatility, timeSqrt);
		double d2 = getD2(d1, volatility, timeSqrt);
		double callValue = (spot * cdf(d1)) - (strike * Math.exp(-r * timeToExpiry) * cdf(d2));
		return callValue;
	}

	/**
	 * First parameter to be calculated as part of BS and greeks.
	 * 
	 * @param spot
	 *            Spot Price Underlying
	 * @param exercise
	 *            Exercise price of Option
	 * @param r
	 *            Risk Free Rate
	 * @param volatility
	 *            Stock volatility (historical or implied)
	 * @param timeSqrt
	 *            Sqrt of decimal time to expiry
	 */
	public static double getD1(double spot, double exercise, double r, double time, double volatility, double timeSqrt) {
		return ((Math.log(spot / exercise) + (r * time)) / (volatility * timeSqrt)) + (0.5 * volatility * timeSqrt);
	}

	/**
	 * @param d1
	 *            Must be calculated prior to d2
	 * @param volatility
	 *            Stock volatility (historical or implied)
	 * @param timeSqrt
	 *            Sqrt of decimal time to expiry
	 */
	public static double getD2(double d1, double volatility, double timeSqrt) {
		return (d1 - (volatility * timeSqrt));
	}

	/**
	 * Second derivative of the option price wrt underlying price. gamma =
	 * d^2Delta/dSpot^2.
	 * 
	 * @param d1
	 *            Must be calculated prior to gamma
	 * @param spot
	 *            Spot Price Underlying
	 * @param volatility
	 *            Stock volatility (historical or implied)
	 * @param timeSqrt
	 *            Sqrt of decimal time to expiry
	 */
	public static double calcGammaCall(double d1, double spot, double volatility, double timeSqrt) {
		double gamma = ndf(d1) / (spot * volatility * timeSqrt);
		return gamma;
	}

	public static double cdf(double z) {

		double zabs;
		double p;
		double expntl, pdf;

		final double p0 = 220.2068679123761;
		final double p1 = 221.2135961699311;
		final double p2 = 112.0792914978709;
		final double p3 = 33.91286607838300;
		final double p4 = 6.373962203531650;
		final double p5 = .7003830644436881;
		final double p6 = .3526249659989109E-01;

		final double q0 = 440.4137358247522;
		final double q1 = 793.8265125199484;
		final double q2 = 637.3336333788311;
		final double q3 = 296.5642487796737;
		final double q4 = 86.78073220294608;
		final double q5 = 16.06417757920695;
		final double q6 = 1.755667163182642;
		final double q7 = .8838834764831844E-1;

		final double cutoff = 7.071;
		final double root2pi = 2.506628274631001;

		zabs = Math.abs(z);

		// |z| > 37

		if (z > 37.0) {

			p = 1.0;

			return p;

		}

		if (z < -37.0) {

			p = 0.0;

			return p;

		}

		// |z| <= 37.

		expntl = Math.exp(-.5 * zabs * zabs);

		pdf = expntl / root2pi;

		// |z| < cutoff = 10/sqrt(2).

		if (zabs < cutoff) {

			p = expntl * ((((((p6 * zabs + p5) * zabs + p4) * zabs + p3) * zabs + p2) * zabs + p1) * zabs + p0) / (((((((q7 * zabs + q6) * zabs + q5) * zabs + q4) * zabs + q3) * zabs + q2) * zabs + q1) * zabs + q0);

		} else {

			p = pdf / (zabs + 1.0 / (zabs + 2.0 / (zabs + 3.0 / (zabs + 4.0 / (zabs + 0.65)))));

		}

		if (z < 0.0) {

			return p;

		} else {

			p = 1.0 - p;

			return p;

		}

	}

	/**
	 * Normal distribution method.
	 */
	public static double ndf(double input) {
		double output = 0.0;
		output = (1 / Math.sqrt(2 * Math.PI)) * Math.exp(-(Math.pow(input, 2)) / 2);
		return output;
	}

}
