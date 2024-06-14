## predictbodyfat R Package

This package provides a function `predictBodyFat` for prediction or imputation of body fat percentage in data sets with one of two sets of covariates: 
(1) age, gender, height, and weight, and (2) the previous four plus pregnancy status, education, race/ethnicity, waist circumference, and arm circumference.

### Prediction function

The function leverages prediction models fit using NHANES data with body fat percentage measured by dual-energy X-ray absorptiometry (DXA). 
The files used to load this data and fit these models are provided so that others can augment the code for more complex models or to leverage different sets of covariates. 
These can be run in the order 'load_data.R', 'clean_data.R', and 'model_data.R'. For simplicity, 'model_data.R' sources the other two scripts if saved in a parent R directory
and can be run alone.

These scripts leverage the `nhanesA` R package to load publicly available NHANES data from the CDC. If that package becomes deprecated or otherwise unavailable,
all datasets used for training models are available at the CDC's website: https://www.cdc.gov/nchs/nhanes/index.htm

### Data formatting

Note that your data must be formatted exactly as the data used to train the underlying prediction models in order to use the prediction function. 
So every covariate in either (1) or (2) must be present in the data. If a covariate is missing for only some data rows, the function will return NA for those rows.
Thus, we recommend you do imputation or multiple imputation of missing covariate data prior to using the prediction function if a full set of predictions are desired.
Likewise the class of each variable and unit should match that outlines in the function documentation (i.e., race/ethnicity must be a factor variable that matches
the limited version collected by early years of NHANES and height must be provided in centimeters).

### Contact

This package was developed by Eliab Woldebegriel and maintained by Eliab Woldegebriel and Bryan Blette. Questions on the package can be sent to bryan.blette@vumc.org.
