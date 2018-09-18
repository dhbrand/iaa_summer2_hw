libname ts "C:\Users\maomao328\Desktop\Fall1\Time Series";
run;
proc arima data=ts.well_2 plot=all;
	identify var=avg nlag=10 /*for correlation plot*/ stationarity=(adf=2)/*three tests*/;
	*identify var=avg(1)/*take first order differences*/ nlag=10 stationarity=(adf=2); 
run;
quit;

/* Seasonal Augmented Dickey-Fuller Testing */

proc arima data=ts.well_2 plot=all;
	*identify var=avg nlag=60 stationarity=(adf=5);
	identify var=avg nlag=60 stationarity=(adf=2 dlag=12);
	*identify var=avg(12) stationarity=(adf=2);
run;
quit;
