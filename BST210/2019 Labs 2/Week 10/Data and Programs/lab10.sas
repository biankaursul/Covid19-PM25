/* original code----
FILENAME REFFILE '/folders/myfolders/Lab 10/whsmtch_v12.dta';

PROC IMPORT DATAFILE=REFFILE replace
	DBMS=DTA
	OUT=whs;
RUN;
---end original code*/

/* Correction for loading data thanks to Ta-Wei Lin */
DATA whs;
    INFILE '(Insert Path)\whsmtch.dat';
    INPUT id crpq sbp dm age smok chdlq caco high_sbp;
RUN;

* conditional logistic with indicator of high blood pressure;
proc logistic data=whs;
	strata id;
	model caco(event='1') = high_sbp;
run;

* Using categorical sbp;
proc logistic data=whs;
	strata id;
	class sbpcat (ref = '1') / param=ref;
	model caco(event='1') = sbpcat;
run;

* Ordinal sbp;
proc logistic data=whs;
	strata id;
	model caco(event='1') = sbpcat;
run;
