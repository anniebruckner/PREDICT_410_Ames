* Andrea Bruckner
  PREDICT 410, Sec 55
  Winter 2016
  Assignment 3
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

* Make the previous scatterplot more readable;
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

* Subset data for just Sample Population and define transformation variables;
data sample;
set temp;
if (drop_condition='09: Sample Population');
logSalePrice = log(SalePrice);
logGrLivArea = log(GrLivArea);
logTotalBsmtSF = log(TotalBsmtSF);
GrLivAreaSqRt = sqrt(GrLivArea);
TotalBsmtSFSqRt = sqrt(TotalBsmtSF);
GrLivAreaInv = 1/(GrLivArea);
TotalBsmtSFInv = 1/(TotalBsmtSF);
* Would be interesting to use for outliers,
but outliers cannot be defined by the response variable;
* PricePerSqFtGrLiv=(SalePrice/GrLivArea);
* PricePerSqFtBsmt=(SalePrice/TotalBsmtSF);
run;

* View scatterplot of sample data;
ods graphics on;
proc sgplot data=sample;
	scatter X=GrLivArea Y=SalePrice;
	title "Sample Population GrLivArea v. SalePrice Scatter Plot – No Smoothers";
run;
ods graphics off;

********************************************************************;
* Initial EDA;
********************************************************************;

* See which variables have strongest correlation with SalePrice;
ods graphics on;
proc corr data=sample plots=matrix;
var SalePrice GrLivArea TotalBsmtSF LotFrontage LotArea MasVnrArea BsmtFinSF1 BsmtFinSF2 BsmtUnfSF FirstFlrSF SecondFlrSF LowQualFinSF GarageArea WoodDeckSF OpenPorchSF EnclosedPorch ThreeSsnPorch ScreenPorch PoolArea MiscVal;
title;
run;
ods graphics off;

* Create plots of top 3 predictors + response variable;
proc sgplot data=sample;
histogram SalePrice;
density SalePrice / type=normal;
density SalePrice / type=kernel;
run; quit;

proc sgplot data=sample;
histogram GrLivArea;
density GrLivArea / type=normal;
density GrLivArea / type=kernel;
run; quit;

proc sgplot data=sample;
histogram GarageArea;
density GarageArea / type=normal;
density GarageArea / type=kernel;
run; quit;

proc sgplot data=sample;
histogram TotalBsmtSF;
density TotalBsmtSF / type=normal;
density TotalBsmtSF / type=kernel;
run; quit;

* Create matrix of 2 most promising predictors;
ods graphics on;
proc corr data=sample plots(maxpoints=none)=matrix;
var SalePrice GrLivArea TotalBsmtSF;
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
histogram logSalePrice;
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

********************************************************************;
* Create Models;
********************************************************************;

* Create simple linear regression models;
proc reg data=sample alpha=0.05;
model SalePrice=GrLivArea / clb;
run;

proc reg data=sample alpha=0.05;
model SalePrice=TotalBsmtSF / clb;
run;

* Create multiple linear regression model;
proc reg data=sample alpha=0.05;
model SalePrice=GrLivArea TotalBsmtSF / clb;
output out=sample_out cookd=cookd;
run;

********************************************************************;
* Remove Outliers;
********************************************************************;

* EDA for predictor variables;
proc means data=sample min q1 mean median q3 max qrange;
var GrLivArea TotalBsmtSF;
run; quit;

proc options option=macro;
run;

* Create IQR Macro;
%macro PredictorsIQR(indata,x,outdata);
proc means data=&indata q1 q3 qrange;
var &x;
output out=&outdata q1=q1 q3=q3 qrange=qrange;
run;
quit;
%mend PredictorsIQR;

* Calculate IQR for GrLivArea;
%PredictorsIQR(indata=%str(sample),
x=%str(GrLivArea),
outdata=%str(GrLivArea_IQR));

* Calculate upper and lower fence for GrLivArea;
data GrLivArea_IQR;
set GrLivArea_IQR;
GrLiv_LL = q1 - 1.5*qrange;
GrLiv_UL = q3 + 1.5*qrange;
run; quit;

proc print data=GrLivArea_IQR;
run; quit;

* Lower Limit: 167;
* Upper Limit: 2735;

* Calculate IQR for TotalBsmtSF;
%PredictorsIQR(indata=%str(sample),
x=%str(TotalBsmtSF),
outdata=%str(TotalBsmtSF_IQR));

* Calculate upper and lower fence for TotalBsmtSF;
data TotalBsmtSF_IQR;
set TotalBsmtSF_IQR;
Bsmt_LL = q1 - 1.5*qrange;
Bsmt_UL = q3 + 1.5*qrange;
run; quit;

proc print data=TotalBsmtSF_IQR;
run; quit;

* Lower Limit: 183;
* Upper Limit: 1887;

* Create outliers waterfall;

data temp2;
	set sample_out;
	format outlier_def $40.;
	if (GrLivArea < 167 or GrLivArea > 2735) then do;
		outlier_def = '1. GrLivArea Outliers';
		outlier_code = 1;
	end;
	else if (TotalBsmtSF < 183 or TotalBsmtSF > 1887) then do;
		outlier_def = '2. TotalBsmtSF Outliers';
		outlier_code = 2;
	end;
	else if (cookd > 4/1831) then do;
		outlier_def = '3. Cook D Outliers';
		outlier_code = 3;
	end;
	else do;
		outlier_def = '4. Not An Outlier';
		outlier_code = 0;
	end;
run;

* View frequency of outliers;
proc freq data=temp2;
tables outlier_def;
title 'Outliers';
run; quit;

* Subset data for just sample without outliers;
data outliers;
set temp2;
if (outlier_code > 0) then delete;
run;

* Create outliers multiple linear regression model;
proc reg data=outliers alpha=0.05;
model SalePrice=GrLivArea TotalBsmtSF / clb;
run;

* Create unpacked outliers model;
ods graphics on;
proc reg data=outliers alpha=0.05 plots(unpack);
model SalePrice=GrLivArea TotalBsmtSF / clb;
run;
ods graphics off;

********************************************************************;
* Create Transformed Models;
********************************************************************;

* Create log response simple linear regression models;
proc reg data=sample alpha=0.05;
model logSalePrice=GrLivArea / clb;
run;

proc reg data=sample alpha=0.05;
model logSalePrice=TotalBsmtSF / clb;
run;

* Create log response multiple linear regression model;
proc reg data=sample alpha=0.05;
model logSalePrice=GrLivArea TotalBsmtSF / clb;
run;

* Create log predictors and log response simple linear regression models;
proc reg data=sample alpha=0.05;
model logSalePrice=logGrLivArea / clb;
run;

proc reg data=sample alpha=0.05;
model logSalePrice=logTotalBsmtSF / clb;
run;

* Create log predictors and log response multiple linear regression models;
proc reg data=sample alpha=0.05;
model logSalePrice=logGrLivArea TotalBsmtSF / clb;
run;

proc reg data=sample alpha=0.05;
model logSalePrice=GrLivArea logTotalBsmtSF / clb;
run;

proc reg data=sample alpha=0.05;
model logSalePrice=logGrLivArea logTotalBsmtSF / clb;
run;

* Create square root predictors and log response multiple linear regression models;
proc reg data=sample alpha=0.05;
model logSalePrice=GrLivAreaSqRt TotalBsmtSF / clb;
run;

proc reg data=sample alpha=0.05;
model logSalePrice=GrLivArea TotalBsmtSFSqRt / clb;
run;

proc reg data=sample alpha=0.05;
model logSalePrice=GrLivAreaSqRt TotalBsmtSFSqRt / clb;
run;

* Create inverse predictors and log response multiple linear regression models;
proc reg data=sample alpha=0.05;
model logSalePrice=GrLivAreaInv TotalBsmtSF / clb;
run;

proc reg data=sample alpha=0.05;
model logSalePrice=GrLivArea TotalBsmtSFInv / clb;
run;

proc reg data=sample alpha=0.05;
model logSalePrice=GrLivAreaInv TotalBsmtSFInv / clb;
run;

********************************************************************;
* Create Unpacked Models;
********************************************************************;

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