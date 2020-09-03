* A few Stata commands, to get us started...
use "P:\Week_1_export\Data and Programs\circarrest.dta"
summarize
histogram pdi, normal
graph box minutes, by(dhca)
scatter pdi minutes
lowess pdi minutes
ttest pdi==110
ttest pdi, by(dhca)
regress pdi minutes
predict pdifitted
twoway scatter pdi minutes || line pdifitted minutes