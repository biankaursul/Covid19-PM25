proc import out = dat
datafile = "P:\Week_1_export\Data and Programs\circarrest.xlsx"
dbms = xlsx;
run;

proc means data = dat;
run;

proc univariate data = dat noprint;
histogram pdi / normal;
run;

proc sort data = dat;
by dhca;
run;
proc boxplot data = dat;
plot minutes * dhca;
run;

proc gplot data = dat;
plot pdi*minutes;
run;
proc loess data = dat;
model pdi = minutes;
run;

proc ttest data=dat h0=110 sides=2;
var pdi;
run;

proc ttest data=dat sides=2;
class dhca;
var pdi;
run;

proc corr data=dat;
var pdi minutes;
run;

proc reg data = dat;
model pdi = minutes;
run;