proc import file='\\tsclient\Desktop\BST210\lab_test\Week 12\srt.csv' out=dat dbms=csv replace;
	getnames=yes;
run;
PROC TABULATE DATA=dat; /*tablulate and explore the data*/
CLASS sorb;
VAR sex dur tgh;
TABLE sorb,
sex*(Mean P5 P95);
TABLE sorb,
dur*(Mean P5 P95);
TABLE sorb,
tgh*(Mean P5 P95);
RUN;
PROC LIFETEST data=dat plots=survival(atrisk); /*compute K-M estimates and plot survival curve*/
strata sex sorb;
time fup*status(0);
run;
PROC LIFEREG data=dat; /*fit a exponential survival regression model and plot probability*/
      class sex sorb;
      model fup*status(0) = sex sorb tgh dur /d=exponential;
      probplot;
run;
PROC LIFEREG data=dat; /*fit a Weibull survival regression model and plot probability*/
      class sex sorb;
      model fup*status(0) = sex sorb tgh dur /d=weibull;
      probplot;
run;
PROC PHREG data = dat plots=survival; /*fit a full Cox model*/
      class sex sorb;
      model fup*status(0) = sex sorb tgh dur;
run;
