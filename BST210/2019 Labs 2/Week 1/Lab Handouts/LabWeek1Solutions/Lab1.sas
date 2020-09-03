proc import datafile="P:\Week1_export\Data_and_Programs\lab1.csv" 
out=mydata dbms=csv replace; 
getnames=yes; 
run;

proc ttest data=mydata h0=3.5 sides=u;
var x;
run;

proc reg data = mydata;
model y = x / clb;
run;
