* Andrea Bruckner
  PREDICT 410, Sec 55
  Winter 2016
  Assignment 2
;

********************************************************************;
* Preliminary Steps and Data Survey;
********************************************************************;

* Access library where Ames housing dataset is stored;
libname mydata '/scs/crb519/PREDICT_410/SAS_Data/' access=readonly;
proc datasets library=mydata; run; quit;

* Set Ames housing dataset to short name;
data ames;
set mydata.AMES_HOUSING_DATA;
run;

* Explore Ames housing dataset contents;
proc contents data=ames; run;

* Print first 10 observations as preview of data;
Title "Preview of Dataset";
options obs=10;
proc print data=ames; run;
options obs=max;

* Print scatterplot of GrLivArea v. SalePrice to find obvious outliers or unusual data;
ods graphics on;
proc sgplot data=ames;
	scatter X=GrLivArea Y=SalePrice;
	title "GrLivArea v. SalePrice Scatter Plot – No Smoothers";
run;
ods graphics off;

* Making the previous scatterplot more readable;
proc sgplot data=ames;
reg x=GrLivArea y=SalePrice / clm;
loess x=GrLivArea y=SalePrice / nomarkers;
title "GrLivArea v. SalePrice Scatter Plot – Smoothers";
run; quit;

********************************************************************;
* Define the Sample Population;
********************************************************************;

* Create drop conditions;
data temp;
	set ames;
	format drop_condition $40.;
	if (BldgType ne '1Fam') then drop_condition='01: Not a Single Family Home';
	else if (Zoning = 'A' or zoning = 'C' or zoning = 'FV' or zoning = 'I' or zoning = 'RP')
		then drop_condition='02: Non-Residential Zone or RV Park';
	else if (SaleCondition ne 'Normal') then drop_condition='03: Not Normal Sale Condition';
	else if (GrLivArea >= 4000) then drop_condition='04: Atypically Large House';
	else if (TotalBsmtSF < 1) then drop_condition='05: No Basement';
	else if (GarageArea < 1) then drop_condition='06: No Garage';
	else if (SalePrice <= 0) then drop_condition='07: Pricing Error';
	else if (Utilities ne 'AllPub') then drop_condition='08: Not All Public Utilities Available';
else drop_condition='09: Sample Population';
run;	

* View frequency of drop conditions;
proc freq data=temp;
tables drop_condition;
title 'Sample Population Waterfall';
run; quit; * (Reveals that all Sale Prices are positive);

* Subset data for just Sample Population and define logSalePrice variable;
data sample;
set temp;
if (drop_condition='09: Sample Population');
logSalePrice = log(SalePrice);
run;

* View scatterplot of sample data;
ods graphics on;
proc sgplot data=sample;
	scatter X=GrLivArea Y=SalePrice;
	title "Sample Population GrLivArea v. SalePrice Scatter Plot – No Smoothers";
run;
ods graphics off;

********************************************************************;
* Data Quality Checks;
********************************************************************;

* Create summary statistics for continuous data;
proc univariate data=sample;
var GrLivArea GarageArea TotalBsmtSF;
title;
run; quit;

proc means data=sample min q1 mean median q3 max std;
var GrLivArea GarageArea TotalBsmtSF;
run; quit;

proc means data=sample min q1 mean median q3 max std;
class SubClass;
var GrLivArea GarageArea TotalBsmtSF;
run; quit;

********************************************************************;
* Initial EDA;
********************************************************************;

proc means data=sample min mean median max;
class Neighborhood;
var SalePrice;
var GrLivArea;
var GarageArea;
var TotalBsmtSF;
run; quit;

proc sgplot data=sample;
histogram SalePrice / transparency=0.5;
density SalePrice / type=normal;
density SalePrice / type=kernel;
run; quit;

proc sgplot data=sample;
histogram GrLivArea / transparency=0.5;
density GrLivArea / type=normal;
density GrLivArea / type=kernel;
run; quit;

proc sgplot data=sample;
histogram GarageArea / transparency=0.5;
density GarageArea / type=normal;
density GarageArea / type=kernel;
run; quit;

proc sgplot data=sample;
histogram TotalBsmtSF / transparency=0.5;
density TotalBsmtSF / type=normal;
density TotalBsmtSF / type=kernel;
run; quit;

ods graphics on;
proc corr data=sample plots=matrix;
var SalePrice GrLivArea TotalBsmtSF;
run;
ods graphics off;

********************************************************************;
* EDA for Modeling;
********************************************************************;

proc sgplot data=sample;
reg x=GrLivArea y=SalePrice / clm;
loess x=GrLivArea y=SalePrice / nomarkers;
run; quit;

proc sgplot data=sample;
reg x=TotalBsmtSF y=SalePrice / clm;
loess x=TotalBsmtSF y=SalePrice / nomarkers;
run; quit;

proc sgplot data=sample;
histogram logSalePrice / transparency=0.5;
density logSalePrice / type=normal;
density logSalePrice / type=kernel;
run; quit;

proc sgplot data=sample;
reg x=GrLivArea y=logSalePrice / clm;
loess x=GrLivArea y=logSalePrice / nomarkers;
run; quit;

proc sgplot data=sample;
reg x=TotalBsmtSF y=logSalePrice / clm;
loess x=TotalBsmtSF y=logSalePrice / nomarkers;
run; quit;

* Create simple linear regression models;

proc reg data=sample alpha=0.05;
model SalePrice=GrLivArea / clb;
run;

proc reg data=sample alpha=0.05;
model SalePrice=TotalBsmtSF / clb;
run;

proc reg data=sample;
model SalePrice=TotalBsmtSF;
run;

* Create multiple linear regression model;

proc reg data=sample alpha=0.05;
model SalePrice=GrLivArea TotalBsmtSF / clb;
run;

* Create log simple linear regression models;

proc reg data=sample alpha=0.05;
model logSalePrice=GrLivArea / clb;
run;

proc reg data=sample alpha=0.05;
model logSalePrice=TotalBsmtSF / clb;
run;

* Create log multiple linear regression model;

proc reg data=sample alpha=0.05;
model logSalePrice=GrLivArea TotalBsmtSF / clb;
run;

* Create all regular (not log) models unpacked;

ods graphics on;
proc reg data=sample alpha=0.05 plots(unpack);
model SalePrice=GrLivArea / clb;
model SalePrice=TotalBsmtSF / clb;
model SalePrice=GrLivArea TotalBsmtSF / clb;
run;
ods graphics off;

* Create all log models unpacked;

ods graphics on;
proc reg data=sample alpha=0.05 plots(unpack);
model logSalePrice=GrLivArea / clb;
model logSalePrice=TotalBsmtSF / clb;
model logSalePrice=GrLivArea TotalBsmtSF / clb;
run;
ods graphics off;

********************************************************************;
* END;
********************************************************************;