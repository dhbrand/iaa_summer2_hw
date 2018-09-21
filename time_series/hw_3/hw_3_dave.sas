libname ts "/folders/myshortcuts/github/orange_5_hw/time_series";
run;

proc import
  datafile = "/folders/myshortcuts/github/orange_5_hw/time_series/F-179.xlsx"
  out=ts.well
  DBMS=XLSX;
  sheet = "well";
run;

proc sql;
  create table ts.well_ts as
  select year(date) as year, month(date) as month, mean(corrected) as depth
  from ts.well
  group by year, month
  ;
quit;

data well_train;
  set ts.well_ts;
  date=input(catx('-',month,year),ANYDTDTE.);
  if date < '01jan2018'd;
  drop month year;
run;

proc arima data=well_train plot=all;
	identify var=depth nlag=10 /*for correlation plot*/ stationarity=(adf=2)/*three tests*/;
	*identify var=avg(1)/*take first order differences*/ nlag=10 stationarity=(adf=2); 
run;
quit;

/* Seasonal Augmented Dickey-Fuller Testing */

proc arima data=well_train plot=all;
	identify var=depth nlag=40 stationarity=(adf=2);
	*identify var=depth nlag=60 stationarity=(adf=2 dlag=12);
	identify var=depth(12) stationarity=(adf=2);
run;
quit;
