libname ts "C:\Users\maomao328\Desktop\Fall1\Time Series";
run;
/* Seasonal Augmented Dickey-Fuller Testing */
/*check season first*/
proc arima data=ts.train plot=all;
	*identify var=avg nlag=60 stationarity=(adf=5);
	identify var=avg nlag=60 stationarity=(adf=2 dlag=12);
	*identify var=avg(12) stationarity=(adf=2);/*no need to take difference*/
run;
quit;
/*ADF*/
proc arima data=ts.train plot=all;
	identify var=avg nlag=10 /*for correlation plot*/ stationarity=(adf=2);
	*identify var=avg(1)/*take first order differences*/ nlag=10 stationarity=(adf=2); /*no need to take difference*/
run;
quit;


/*fit seasonal dummy variables*/
data season;
set ts.train;
month=month(date);
if month=1 then x1=1; else x1=0;
if month=2 then x2=1; else x2=0;
if month=3 then x3=1; else x3=0;
if month=4 then x4=1; else x4=0;
if month=5 then x5=1; else x5=0;
if month=6 then x6=1; else x6=0;
if month=7 then x7=1; else x7=0;
if month=8 then x8=1; else x8=0;
if month=9 then x9=1; else x9=0;
if month=10 then x10=1; else x10=0;
if month=11 then x11=1; else x11=0;
run;
/*model the seasonality*/
proc reg data=season;
model avg=x1 x2 x3 x4 x5 x6 x7 x8 x9 x10 x11;
output out=residual r=resid
run;
quit;

/*fit residual in the model*/
proc arima data=residual;
identify var=resid nlag=10 stationarity=(adf=2);
run;
quit;




