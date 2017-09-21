* Andrea Bruckner
  PREDICT 410, Sec 55
  Winter 2016
  Assignment 1
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
options obs=max; * reset options to analyze and report on all data;

* Print scatterplot of SalePrice v. GrLivArea to find obvious outliers or unusual data;
ods graphics on;
PROC SGPLOT data=ames;
	scatter X=SalePrice Y=GrLivArea;
	title "Sale Price v. Above Ground Living Area Scatter Plot – No Smoothers";
run;
ods graphics off;

********************************************************************;
* Define the Sample Population;
********************************************************************;

* Create drop conditions;
data temp;
	set ames;
	format drop_condition $35.;
	if (BldgType ne '1Fam') then drop_condition='01: Not a Single Family Home';
	else if (Zoning = 'A' or zoning = 'C' or zoning = 'FV' or zoning = 'I' or zoning = 'RP')
		then drop_condition='02: Non-Residential Zone or RV Park';
	else if (SaleCondition ne 'Normal') then drop_condition='03: Not Normal Sale Condition';
	else if (GrLivArea >= 4000) then drop_condition='04: Atypically Large House';
	else if (SalePrice <= 0) then drop_condition='05: Pricing Error';
else drop_condition='06: Sample Population';
run;	

* View frequency of drop conditions;
proc freq data=temp;
tables drop_condition;
title 'Sample Population Waterfall';
run; quit; * (Reveals that all Sale Prices are positive);

* Subset data for just Sample Population;
data sample;
set temp;
if (drop_condition='06: Sample Population');
run;

* View scatterplot of sample data;
ods graphics on;
PROC SGPLOT data=sample;
	scatter X=SalePrice Y=GrLivArea;
	title "Sample Population Sale Price v. Above Ground Living Area Scatter Plot – No Smoothers";
run;
ods graphics off;

********************************************************************;
* Data Quality Checks;
********************************************************************;

* Create frequency tables for categorical data;
proc freq data=sample;
tables Utilities Neighborhood Condition1 HouseStyle OverallQual OverallCond BsmtFinType1 Heating CentralAir KitchenQual GarageType GarageCars SaleCondition;
title;
run; quit;

* Create summary statistics for numeric data;
proc univariate data=sample;
var LotArea GrLivArea LowQualFinSF FullBath HalfBath BedroomAbvGr MoSold;
run; quit;

proc means data=sample min q1 mean median q3 max std;
var LotArea GrLivArea LowQualFinSF FullBath HalfBath BedroomAbvGr MoSold;
run; quit;

********************************************************************;
* Initial EDA;
********************************************************************;

* Create frequency tables for Initial EDA categorical data;
proc freq data=sample;
tables Neighborhood HouseStyle OverallQual BsmtFinType1 GarageType;
run; quit;

* Create summary statistics for Initial EDA numeric data;
proc univariate data=sample;
var LotArea GrLivArea FullBath BedroomAbvGr MoSold;
run; quit;

proc means data=sample min q1 mean median q3 max std;
var LotArea GrLivArea FullBath BedroomAbvGr MoSold;
run; quit;

**OverallQual is more readable when using proc univariate and proc means**;
proc univariate data=sample;
var OverallQual;
run; quit;

proc means data=sample min q1 mean median q3 max std;
var OverallQual;
run; quit;

**FullBath BedroomAbvGr MoSold are more readable when using proc freq**;
proc freq data=sample;
tables FullBath BedroomAbvGr MoSold ;
run; quit;

* Observations in a typical housing transaction;
data temp2;
	set sample;
	format drop_condition $35.;
	if (OverallQual = '1' or OverallQual = '2' or OverallQual = '10') then drop_condition='01: Not Typical Overall Quality';
	else if (HouseStyle = '1.5Unf' or HouseStyle = '2.5Fin' or HouseStyle = '2.5Unf') then drop_condition='02: Not Typical House Style';
	else if (GarageType = '2Types' or GarageType = 'Basement' or GarageType = 'Carport') then drop_condition='03: Not Typical Garage Type';
	else if (BsmtFinType1 = 'LWQ' or BsmtFinType1 = 'NA') then drop_condition='04: Not Typical Basement Type';
	else if (Neighborhood = 'Blmngtn') then drop_condition='05: Not Typical Neighboorhood'; * I decided to drop neighborhoods with a 3% or less frequency;
	else if (6000 <= LotArea and LotArea >= 17400) then drop_condition='06: Not Typical Lot Area';
	else if (858 <= GrLivArea and GrLivArea >= 2448) then drop_condition='07: Not Typical GrLivArea';
	else if (FullBath ne 1 and FullBath ne 2) then drop_condition='08: Not Typical Number of Full Baths';
	else if (BedroomAbvGr ne 2 and BedroomAbvGr ne 3 and BedroomAbvGr ne 4)then drop_condition='09: Not Typical Number of Bedrooms';
	else if (MoSold = 1 or MoSold = 12) then drop_condition='10: Not Typical Month Sold';
else drop_condition='11: Typical Housing Transaction';
run;

* View frequency of drop conditions;
proc freq data=temp2;
tables drop_condition;
title 'Typical Housing Transaction Waterfall';
run; quit;

* Subset data for just Typical Housing Transaction;
data typical;
set temp2;
if (drop_condition='11: Typical Housing Transaction');
run;

********************************************************************;
* Initial EDA for Modeling;
********************************************************************;

* Box plots for Categorical Data;
**(log(SalePrice) did not work)**;
***I'd like to eventually write a macro and perform a t-test to compare group means***;
proc sort data=typical;
by Neighborhood;
run;
Title "Variable EDA - Boxplot of Neighborhoods";
proc boxplot data=typical;
plot SalePrice * Neighborhood;
run;

proc sort data=typical;
by HouseStyle;
run;
Title "Variable EDA - Boxplot of House Styles";
proc boxplot data=typical;
plot SalePrice * HouseStyle;
run;

proc sort data=typical;
by OverallQual;
run;
Title "Variable EDA - Boxplot of Overall Quality";
proc boxplot data=typical;
plot SalePrice * OverallQual;
run;

proc sort data=typical;
by BsmtFinType1;
run;
Title "Variable EDA - Boxplot of Basement Finished Area";
proc boxplot data=typical;
plot SalePrice * BsmtFinType1;
run;

proc sort data=typical;
by GarageType;
run;
Title "Variable EDA - Boxplot of Garage Type";
proc boxplot data=typical;
plot SalePrice * GarageType;
run;

* Scatter plots for Numeric Data;
Neighborhood HouseStyle OverallQual BsmtFinType1 GarageType
LotArea GrLivArea FullBath BedroomAbvGr MoSold

ods graphics on;
proc sgscatter data=typical;
compare x=(LotArea)
y=SalePrice / loess reg;
title;
run; quit;
ods graphics off;

ods graphics on;
proc sgscatter data=typical;
compare x=(GrLivArea)
y=SalePrice / loess reg;
run; quit;
ods graphics off;

ods graphics on;
proc sgscatter data=typical;
compare x=(FullBath)
y=SalePrice / loess reg;
run; quit;
ods graphics off;

ods graphics on;
proc sgscatter data=typical;
compare x=(BedroomAbvGr)
y=SalePrice / loess reg;
run; quit;
ods graphics off;

ods graphics on;
proc sgscatter data=typical;
compare x=(MoSold)
y=SalePrice / loess reg;
run; quit;
ods graphics off;

**FullBath BedroomAbvGr MoSold look better as box plots**;
proc sort data=typical;
by FullBath;
run;
Title "Variable EDA - Boxplot of Number of Full Bathrooms";
proc boxplot data=typical;
plot SalePrice * FullBath;
run;

proc sort data=typical;
by BedroomAbvGr;
run;
Title "Variable EDA - Boxplot of Number of Bedrooms Above Ground";
proc boxplot data=typical;
plot SalePrice * BedroomAbvGr;
run;

proc sort data=typical;
by MoSold;
run;
Title "Variable EDA - Boxplot of Month Sold";
proc boxplot data=typical;
plot SalePrice * MoSold;
run;

********************************************************************;
* END;
********************************************************************;