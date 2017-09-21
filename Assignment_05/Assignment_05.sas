* Andrea Bruckner
  PREDICT 410, Sec 55
  Winter 2016
  Assignment 5
;

********************************************************************;
* Preliminary Steps;
********************************************************************;

* Access library where Ames housing dataset is stored;
libname mydata '/scs/crb519/PREDICT_410/SAS_Data/' access=readonly;
proc datasets library=mydata; run; quit;

* Set Ames housing dataset to short name;
data ames;
set mydata.AMES_HOUSING_DATA;
run;

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

* Subset data for just Sample Population and Define TotalBath_Calc;
data sample;
set temp;
if (drop_condition='09: Sample Population');
TotalBath = max(FullBath,0) + max(BsmtFullBath,0);
TotalHalfBath = max(HalfBath,0) + max(BsmtHalfBath,0);
TotalBath_Calc = TotalBath + TotalHalfBath;
title;
run;

********************************************************************;
* Create a Train/Test Split of Data for Cross-Validation;
********************************************************************;

data test_train;
set sample;
* generate a uniform(0,1) random variable with seed set to 123;
u = uniform(123);
if (u < 0.70) then train = 1;
else train = 0;
if (train=1) then train_response=SalePrice;
else train_response=.;
run;

proc contents data=test_train; run;

* Print first 10 observations as preview of data;
Title "Preview of Dataset";
options obs=10;
proc print data=test_train; run;
options obs=max;

* View frequency test/train data;
proc freq data=test_train;
tables train;
title 'Observation Counts for Test/Train Data Partition';
run; quit;

/*
If we wanted to create two separate data sets from the split,
this is how we would code it.

* Define Train Data Set;
data ames_train;
set test_train;
if (train = 1);
run;

* Define Test Data Set;
data ames_test;
set test_train;
if (train = 0);
run;

title "Preview of Training Dataset";
options obs=10;
proc print data=ames_train; run;
options obs=max;

title "Preview of Testing Dataset";
options obs=10;
proc print data=ames_test; run;
options obs=max;
*/

********************************************************************;
* Model Identification;
********************************************************************;

* (Orignally included BsmtUnfSF as a predictor but removed it since it was
showing bias/multicollinearity with BsmtFinSF1 BsmtFinSF2);

* Adjusted R-Squared Selection;
ods graphics on;
title "Automated Variable Selection Using Adjusted R-Squared Selection";
proc reg data=test_train outest=Model_AdjR2_out;
Model_AdjR2: model train_response = GrLivArea TotalBsmtSF LotFrontage
LotArea BsmtFinSF1 BsmtFinSF2 FirstFlrSF
GarageArea OpenPorchSF OverallQual OverallCond
YearBuilt BedroomAbvGr TotRmsAbvGrd TotalBath_Calc / adjrsq aic bic mse cp vif selection = adjrsq best=10;
run;
ods graphics off;

proc print data=Model_AdjR2_out; run; * This contains parameter estimates for all models using the Adjusted R-Squared Selection;

* NB: Running proc reg with data=test_train and train_response, or data=ames_train and SalePrice
all use the same number of observations, 1015, as the code above even if they read different amounts of variables,
1831 v. 1252. They all result in the same variable selection.;

* Adjusted R-Squared Selection with Variables Removed/Multicollinearity Assessment Using VIFs;
ods graphics on;
title "Adjusted R-Squared Selection with Variables Removed";
proc reg data=test_train outest=Model_AdjR2_out2;
Model_AdjR2: model train_response = GrLivArea TotalBsmtSF LotFrontage
LotArea BsmtFinSF1 /*BsmtFinSF2*/ FirstFlrSF
GarageArea /*OpenPorchSF*/ OverallQual OverallCond
YearBuilt BedroomAbvGr TotRmsAbvGrd /*TotalBath_Calc*/ / selection=rsquare start=12 stop=12 adjrsq aic bic mse cp vif;
output out = AdjR2_fit pred=yhat;
run;
ods graphics off;
 
proc print data=Model_AdjR2_out2; run; * This contains parameter estimates for just the best model from the Adjusted R-Squared Selection;

* Defining variables for MSE, MAE, and predictive accuracy computations;
data AdjR2_fit;
set AdjR2_fit;
title;
residual = SalePrice - yhat;
abs_res = abs(residual);
sq_res = residual*residual;
mse = mean(sq_res); * Not really necessary since, for a single observation, mse=sq_res and mae=abs_res;
mae = mean(abs_res);
accuracy = abs_res / SalePrice;
format Prediction_Grade $7.;
if accuracy <= 0.1 then Prediction_Grade = 'Grade 1';
else if accuracy <= 0.15 then Prediction_Grade = 'Grade 2';
else Prediction_Grade = 'Grade 3';
run;

proc print data=AdjR2_fit (obs=10);
run;

title "Model_AdjR2's MSE and MAE";
proc means data=AdjR2_fit n mean;
class train;
var sq_res abs_res;
output out = msemae_AdjR2
mean(sq_res abs_res) = MSE MAE;
proc print data = msemae_AdjR2;
run; quit;

title "Model_AdjR2's Operational Accuracy";
proc freq data = AdjR2_fit;
tables Prediction_Grade;
run;

********************************************************************;

* Maximum R-Squared Selection;
ods graphics on;
title "Automated Variable Selection Using Maximum R-Squared Selection";
proc reg data=test_train outest=Model_MaxR_out;
Model_MaxR: model train_response = GrLivArea TotalBsmtSF LotFrontage
LotArea BsmtFinSF1 BsmtFinSF2 FirstFlrSF
GarageArea OpenPorchSF OverallQual OverallCond
YearBuilt BedroomAbvGr TotRmsAbvGrd TotalBath_Calc / adjrsq aic bic mse cp vif selection = maxr;
run;
ods graphics off;

proc print data=Model_MaxR_out; run;

* Maximum R-Squared Selection with Variables Removed/Multicollinearity Assessment Using VIFs;
ods graphics on;
title "Maximum R-Squared Selection with Variables Removed";
proc reg data=test_train outest=Model_MaxR_out2;
Model_MaxR: model train_response = GrLivArea TotalBsmtSF LotFrontage
LotArea BsmtFinSF1 /*BsmtFinSF2*/ FirstFlrSF
GarageArea /*OpenPorchSF*/ OverallQual OverallCond
YearBuilt BedroomAbvGr TotRmsAbvGrd /*TotalBath_Calc*/ / selection=rsquare start=12 stop=12 adjrsq aic bic mse cp vif;
output out = MaxR_fit pred=yhat;
run;
ods graphics off;
 
proc print data=Model_MaxR_out2; run;


* Defining variables for MSE, MAE, and predictive accuracy computations;
data MaxR_fit;
set MaxR_fit;
title;
residual = SalePrice - yhat;
abs_res = abs(residual);
sq_res = residual*residual;
accuracy = abs_res / SalePrice;
format Prediction_Grade $7.;
if accuracy <= 0.1 then Prediction_Grade = 'Grade 1';
else if accuracy <= 0.15 then Prediction_Grade = 'Grade 2';
else Prediction_Grade = 'Grade 3';
run;

proc print data=MaxR_fit (obs=10);
run;

title "Model_MaxR's MSE and MAE";
proc means data=MaxR_fit n mean;
class train;
var sq_res abs_res;
output out = msemae_MaxR
mean(sq_res abs_res) = MSE MAE;
proc print data = msemae_MaxR;
run; quit;

title "Model_MaxR's Operational Accuracy";
proc freq data = MaxR_fit;
tables Prediction_Grade;
run;

********************************************************************;

* Mallow's Cp Selection;
ods graphics on;
title "Automated Variable Selection Using Mallow's Cp Selection";
proc reg data=test_train outest=Model_MCp_out;
Model_MCp: model train_response = GrLivArea TotalBsmtSF LotFrontage
LotArea BsmtFinSF1 BsmtFinSF2 FirstFlrSF
GarageArea OpenPorchSF OverallQual OverallCond
YearBuilt BedroomAbvGr TotRmsAbvGrd TotalBath_Calc / adjrsq aic bic mse cp vif selection = cp best=10;
run;
ods graphics off;

proc print data=Model_MCp_out; run;

* Mallow's Cp Selection with Variables Removed/Multicollinearity Assessment Using VIFs;
* (This selection resulted in the same model as Model_AdjR2.);
ods graphics on;
title "Mallow's Cp Selection with Variables Removed";
proc reg data=test_train outest=Model_MCp_out2;
Model_MCp: model train_response = GrLivArea TotalBsmtSF LotFrontage
LotArea BsmtFinSF1 /*BsmtFinSF2*/ FirstFlrSF
GarageArea /*OpenPorchSF*/ OverallQual OverallCond
YearBuilt BedroomAbvGr TotRmsAbvGrd /*TotalBath_Calc*/ / selection=rsquare start=12 stop=12 adjrsq aic bic mse cp vif;
output out = MCp_fit pred=yhat;
run;
ods graphics off;
 
proc print data=Model_MCp_out2; run;

* Defining variables for MSE, MAE, and predictive accuracy computations;
data MCp_fit;
set MCp_fit;
title;
residual = SalePrice - yhat;
abs_res = abs(residual);
sq_res = residual*residual;
accuracy = abs_res / SalePrice;
format Prediction_Grade $7.;
if accuracy <= 0.1 then Prediction_Grade = 'Grade 1';
else if accuracy <= 0.15 then Prediction_Grade = 'Grade 2';
else Prediction_Grade = 'Grade 3';
run;

proc print data=MCp_fit (obs=10);
run;

title "Model_MCp's MSE and MAE";
proc means data=MCp_fit n mean;
class train;
var sq_res abs_res;
output out = msemae_MCp
mean(sq_res abs_res) = MSE MAE;
proc print data = msemae_MCp;
run; quit;

title "Model_MCp's Operational Accuracy";
proc freq data = MCp_fit;
tables Prediction_Grade;
run;


********************************************************************;

* Forward Selection;
ods graphics on;
title "Automated Variable Selection Using Forward Selection";
proc reg data=test_train outest=Model_F_out;
Model_F: model train_response = GrLivArea TotalBsmtSF LotFrontage
LotArea BsmtFinSF1 BsmtFinSF2 FirstFlrSF
GarageArea OpenPorchSF OverallQual OverallCond
YearBuilt BedroomAbvGr TotRmsAbvGrd TotalBath_Calc / adjrsq aic bic mse cp vif selection = forward slentry=0.10;
run;
ods graphics off;

proc print data=Model_F_out (obs=10); run;

* Forward Selection with Variables Removed/Multicollinearity Assessment Using VIFs;
* (This selection resulted in the same model as Model_AdjR2 and Model_MCp.);
ods graphics on;
title "Forward Selection with Variables Removed";
proc reg data=test_train outest=Model_F_out2;
Model_F: model train_response = GrLivArea TotalBsmtSF LotFrontage
LotArea BsmtFinSF1 /*BsmtFinSF2*/ FirstFlrSF
GarageArea /*OpenPorchSF*/ OverallQual OverallCond
YearBuilt BedroomAbvGr TotRmsAbvGrd /*TotalBath_Calc*/ / selection=rsquare start=12 stop=12 adjrsq aic bic mse cp vif;
output out = F_fit pred=yhat;
run;
ods graphics off;
 
proc print data=Model_F_out2; run;

* Defining variables for MSE, MAE, and predictive accuracy computations;
data F_fit;
set F_fit;
title;
residual = SalePrice - yhat;
abs_res = abs(residual);
sq_res = residual*residual;
accuracy = abs_res / SalePrice;
format Prediction_Grade $7.;
if accuracy <= 0.1 then Prediction_Grade = 'Grade 1';
else if accuracy <= 0.15 then Prediction_Grade = 'Grade 2';
else Prediction_Grade = 'Grade 3';
run;

proc print data=F_fit (obs=10);
run;

title "Model_F's MSE and MAE";
proc means data=F_fit n mean;
class train;
var sq_res abs_res;
output out = msemae_F
mean(sq_res abs_res) = MSE MAE;
proc print data = msemae_F;
run; quit;

title "Model_F's Operational Accuracy";
proc freq data = F_fit;
tables Prediction_Grade;
run;

********************************************************************;

* Backward Selection;
ods graphics on;
title "Automated Variable Selection Using Backward Selection";
proc reg data=test_train outest=Model_B_out;
Model_B: model train_response = GrLivArea TotalBsmtSF LotFrontage
LotArea BsmtFinSF1 BsmtFinSF2 FirstFlrSF
GarageArea OpenPorchSF OverallQual OverallCond
YearBuilt BedroomAbvGr TotRmsAbvGrd TotalBath_Calc / adjrsq aic bic mse cp vif selection = backward slstay=0.10;
run;
ods graphics off;

proc print data=Model_B_out (obs=10); run;

* Backward Selection with Variables Removed/Multicollinearity Assessment Using VIFs;
* (This selection resulted in the same model as Model_AdjR2, Model_MCp, and Model_F.);
ods graphics on;
title "Backward Selection with Variables Removed";
proc reg data=test_train outest=Model_B_out2;
Model_B: model train_response = GrLivArea TotalBsmtSF LotFrontage
LotArea BsmtFinSF1 /*BsmtFinSF2*/ FirstFlrSF
GarageArea /*OpenPorchSF*/ OverallQual OverallCond
YearBuilt BedroomAbvGr TotRmsAbvGrd /*TotalBath_Calc*/ / selection=rsquare start=12 stop=12 adjrsq aic bic mse cp vif;
output out = B_fit pred=yhat;
run;
ods graphics off;
 
proc print data=Model_B_out2; run;

* Defining variables for MSE, MAE, and predictive accuracy computations;
data B_fit;
set B_fit;
title;
residual = SalePrice - yhat;
abs_res = abs(residual);
sq_res = residual*residual;
accuracy = abs_res / SalePrice;
format Prediction_Grade $7.;
if accuracy <= 0.1 then Prediction_Grade = 'Grade 1';
else if accuracy <= 0.15 then Prediction_Grade = 'Grade 2';
else Prediction_Grade = 'Grade 3';
run;

proc print data=B_fit (obs=10);
run;

title "Model_B's MSE and MAE";
proc means data=B_fit n mean;
class train;
var sq_res abs_res;
output out = msemae_B
mean(sq_res abs_res) = MSE MAE;
proc print data = msemae_B;
run; quit;

title "Model_B's Operational Accuracy";
proc freq data = B_fit;
tables Prediction_Grade;
run;

********************************************************************;

* Stepwise Selection;
ods graphics on;
title "Automated Variable Selection Using Stepwise Selection";
proc reg data=test_train outest=Model_S_out;
Model_S: model train_response = GrLivArea TotalBsmtSF LotFrontage
LotArea BsmtFinSF1 BsmtFinSF2 FirstFlrSF
GarageArea OpenPorchSF OverallQual OverallCond
YearBuilt BedroomAbvGr TotRmsAbvGrd TotalBath_Calc / adjrsq aic bic mse cp vif selection = stepwise slentry =0.10 slstay=0.10;
run;
ods graphics off;

proc print data=Model_S_out (obs=10); run;

* Stepwise Selection with Variables Removed/Multicollinearity Assessment Using VIFs;
* (This selection resulted in the same model as Model_AdjR2, Model_MCp, Model_F, and Model_B.);
ods graphics on;
title "Stepwise Selection with Variables Removed";
proc reg data=test_train outest=Model_S_out2;
Model_S: model train_response = GrLivArea TotalBsmtSF LotFrontage
LotArea BsmtFinSF1 /*BsmtFinSF2*/ FirstFlrSF
GarageArea /*OpenPorchSF*/ OverallQual OverallCond
YearBuilt BedroomAbvGr TotRmsAbvGrd /*TotalBath_Calc*/ / selection=rsquare start=12 stop=12 adjrsq aic bic mse cp vif;
output out = S_fit pred=yhat;
run;
ods graphics off;
 
proc print data=Model_S_out2; run;

* Defining variables for MSE, MAE, and predictive accuracy computations;
data S_fit;
set S_fit;
title;
residual = SalePrice - yhat;
abs_res = abs(residual);
sq_res = residual*residual;
accuracy = abs_res / SalePrice;
format Prediction_Grade $7.;
if accuracy <= 0.1 then Prediction_Grade = 'Grade 1';
else if accuracy <= 0.15 then Prediction_Grade = 'Grade 2';
else Prediction_Grade = 'Grade 3';
run;

proc print data=S_fit (obs=10);
run;

title "Model_S's MSE and MAE";
proc means data=S_fit n mean;
class train;
var sq_res abs_res;
output out = msemae_S
mean(sq_res abs_res) = MSE MAE;
proc print data = msemae_S;
run; quit;

title "Model_S's Operational Accuracy";
proc freq data = S_fit;
tables Prediction_Grade;
run;

********************************************************************;
* END;
********************************************************************;