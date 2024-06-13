# load and clean data if not run already
source("/Users/eliabwoldegebriel/Documents/predictbodyfat/R/clean_data.R")

# load libraries
library(splines)
library(survey)
library(dplyr)
library(mice)
library(randomForest)

# set seed for reproducibility
set.seed(123)

##############################################################################
# prepare data for modeling
##############################################################################

# split data into training and testing data (80/20 split)
# randomly select 80% of the ids and avoid identical ids
train_IDs <- sample(combined_data$id, 0.8*length(combined_data$id), replace = F)

# select rows from combined data that filter the ids from combine data so that it matches with train ids
train_data <- combined_data[combined_data$id %in% train_IDs, ]

# the test data should include the remaining 20%.
test_data <- combined_data %>% anti_join(train_data, by = 'id')

# for simplicity, only use complete data in the test data
test_data <- test_data[complete.cases(test_data), ]

# multiple imputation on train data
combine_data_MI <- mice(train_data, m = 5)

# bring NHANES-provided imputations of BFP back in
combine_data_MI_long <- complete(combine_data_MI, action = 'long', include = TRUE)
for (i in 1:5) {
  combine_data_MI_long$Body_Fat_Percentage[combine_data_MI_long$.imp == i] <-
    BFP_imputations$DXDTOPF[BFP_imputations$`X_MULT_` == i &
                              BFP_imputations$id %in% train_IDs]
}
combine_data_MI <- as.mids(combine_data_MI_long)


##############################################################################
# prediction models using covariate set 1 (Age, Gender, Weight, Height)
##############################################################################

# first fit models ignoring the survey weights

# fit linear regression models
impute_model1 <- with(combine_data_MI, lm(Body_Fat_Percentage ~ Weight + Height + Age + Gender))
impute_model2 <- with(combine_data_MI, lm(Body_Fat_Percentage ~ Weight * Height * Gender + Age * Weight))
impute_model3 <- with(combine_data_MI, lm(Body_Fat_Percentage ~ bs(Weight, df = 6)*Height*Gender + bs(Age, df = 6)*Weight))

# pool the results from imputed data
pooled1 <- pool(impute_model1)
pooled2 <- pool(impute_model2)
pooled3 <- pool(impute_model3)

# copy one of the fitted lm models
pooled_lm1 <- impute_model1$analyses[[1]]
pooled_lm2 <- impute_model2$analyses[[1]]
pooled_lm3 <- impute_model3$analyses[[1]]

# replace the coefficients with pooled estimate for prediction
pooled_lm1$coefficients <- summary(pooled1)$estimate
pooled_lm2$coefficients <- summary(pooled2)$estimate
pooled_lm3$coefficients <- summary(pooled3)$estimate

# get the predictions
predict1 <- predict(pooled_lm1, newdata = test_data)
predict2 <- predict(pooled_lm2, newdata = test_data)
predict3 <- predict(pooled_lm3, newdata = test_data)

# Calculate the mean squared error (MSE)
cov1_MSE1 <- mean((test_data$Body_Fat_Percentage - predict1)^2)
cov1_MSE2 <- mean((test_data$Body_Fat_Percentage - predict2)^2)
cov1_MSE3 <- mean((test_data$Body_Fat_Percentage - predict3)^2)

# fit random forest model using na.roughfix for handling missingness
rf_model <- randomForest(Body_Fat_Percentage ~ Weight + Height + Age + Gender, data = train_data, ntree = 200, na.action = na.roughfix)

# calculate residuals and save as an object called rf_residuals
trainData_rf_predict <- predict(rf_model, newdata = train_data)
rf_residuals <- train_data$Body_Fat_Percentage - trainData_rf_predict
saveRDS(rf_residuals, file = "/Users/eliabwoldegebriel/Documents/predictbodyfat/R/rf_residuals1.rds")

# Get MSE for random forest
testData_rf_predict <- predict(rf_model, newdata = test_data)
cov1_MSE4 <-  mean((test_data$Body_Fat_Percentage - testData_rf_predict)^2)

# find model with lowest MSE
lowest_MSE <- min(cov1_MSE1, cov1_MSE2, cov1_MSE3, cov1_MSE4)

# get the model with smallest value to be saved as rds
if(lowest_MSE == cov1_MSE1){
  saved_model1 <- pooled_lm1
} else if(lowest_MSE == cov1_MSE2){
  saved_model1 <- pooled_lm2
} else if (lowest_MSE == cov1_MSE3) {
  saved_model1 <- pooled_lm3
} else{
  saved_model1 <- rf_model
}
# save the best model
saveRDS(saved_model1, file = "/Users/eliabwoldegebriel/Documents/predictbodyfat/R/saved_model1.rds")


# then fit models using the survey weights

# use svydesign function from survey package to incorporate the sampling weights into the linear regression models above
survey_weight <- svydesign(id = ~1, weights = ~combined_weight, data = combined_data)

# fit linear regression models (switch splines to logs to work better with survey package)
weight_model1 <- with(combine_data_MI, svyglm(Body_Fat_Percentage ~ Weight + Height + Age + Gender, design = survey_weight))
weight_model2 <- with(combine_data_MI, svyglm(Body_Fat_Percentage ~ Weight * Height * Gender + Age * Weight, design = survey_weight))
weight_model3 <- with(combine_data_MI, svyglm(Body_Fat_Percentage ~ Weight * Height * Gender + Age * Weight + log(Age) + log(Weight), design = survey_weight))

# pool model
weight_pooled1 <- pool(weight_model1)
weight_pooled2 <- pool(weight_model2)
weight_pooled3 <- pool(weight_model3)

# copy one of the fitted svyglm models
weight_pooled_svyglm1 <-weight_model1$analyses[[1]]
weight_pooled_svyglm2 <-weight_model2$analyses[[1]]
weight_pooled_svyglm3 <-weight_model3$analyses[[1]]

# replace the coefficient with pooled estimate
weight_pooled_svyglm1$coefficients <- summary(weight_pooled1)$estimate
weight_pooled_svyglm2$coefficients <- summary(weight_pooled2)$estimate
weight_pooled_svyglm3$coefficients <- summary(weight_pooled3)$estimate

# get the weighted prediction
weight_predict1 <- predict(weight_pooled_svyglm1, newdata = test_data)
weight_predict2 <- predict(weight_pooled_svyglm2, newdata = test_data)
weight_predict3 <- predict(weight_pooled_svyglm3, newdata = test_data)

# calculate mean squared error in our test data
cov1_weight_MSE1 <- mean((test_data$Body_Fat_Percentage - weight_predict1)^2)
cov1_weight_MSE2 <- mean((test_data$Body_Fat_Percentage - weight_predict2)^2)
cov1_weight_MSE3 <- mean((test_data$Body_Fat_Percentage - weight_predict3)^2)

# fit random forest model with survey weights
weight_rf_model <- randomForest(Body_Fat_Percentage ~ Weight + Height + Age + Gender, weights = train_data$combined_weight, data = train_data, ntree = 200, na.action = na.roughfix)

# calculate and save residuals from weighted random forest
trainData_weight_rf_predict <- predict(weight_rf_model, newdata = train_data)
weighted_rf_residuals <- train_data$Body_Fat_Percentage - trainData_weight_rf_predict
saveRDS(weighted_rf_residuals, file = "/Users/eliabwoldegebriel/Documents/predictbodyfat/R/weighted_rf_residuals1.rds")

# Get MSE for random forest
testData_weight_rf_predict <- predict(weight_rf_model, newdata = test_data)
cov1_weighted_MSE4 <-  mean((test_data$Body_Fat_Percentage - testData_weight_rf_predict)^2)

# calculate lowest weighted MSE
weighted_lowest_MSE <- min(cov1_weight_MSE1, cov1_weight_MSE2, cov1_weight_MSE3, cov1_weighted_MSE4)

# get the model with smallest weighted value to be saved as rds
if (weighted_lowest_MSE == cov1_weight_MSE1) {
  saved_weighted_model1 <- weight_pooled_svyglm1
} else if (weighted_lowest_MSE == cov1_weight_MSE2) {
  saved_weighted_model1 <- weight_pooled_svyglm2
} else if (weighted_lowest_MSE == cov1_weight_MSE3) {
  saved_weighted_model1 <- weight_pooled_svyglm3
} else {
  saved_weighted_model1 <- weight_rf_model
}

# save the best weighted model
saveRDS(saved_weighted_model1, file = "/Users/eliabwoldegebriel/Documents/predictbodyfat/R/saved_weighted_model1.rds")


##############################################################################
# prediction models using covariate set 2 (Age, Gender, Weight, Height
# Pregnancy, Education, Race/Ethnicity, Waist Circumference, Arm Circumference)
##############################################################################

# first fit models ignoring the survey weights
# fit linear regression models
impute_model1_cov2 <- with(combine_data_MI, lm(Body_Fat_Percentage ~ Pregnant + Education + factor(Race_Ethnicity) + Arm_Circumference + Waist_Circumference + Weight + Height + Age + Gender))
impute_model2_cov2 <- with(combine_data_MI, lm(Body_Fat_Percentage ~ Pregnant * Weight + Education + factor(Race_Ethnicity) * Weight + Arm_Circumference + Waist_Circumference * Weight * Height * Gender + Age * Weight))
impute_model3_cov2 <- with(combine_data_MI, lm(Body_Fat_Percentage ~ bs(Age, df = 6) + bs(Weight, df = 6) + Pregnant * Weight + Education + factor(Race_Ethnicity) * Weight + Arm_Circumference + Waist_Circumference * Weight * Height * Gender + Age * Weight))

# pool the results from imputed data
pooled1_cov2 <- pool(impute_model1_cov2)
pooled2_cov2 <- pool(impute_model2_cov2)
pooled3_cov2 <- pool(impute_model3_cov2)

# copy one of the fitted lm models
pooled_lm1_cov2 <- impute_model1_cov2$analyses[[1]]
pooled_lm2_cov2 <- impute_model2_cov2$analyses[[1]]
pooled_lm3_cov2 <- impute_model3_cov2$analyses[[1]]

# replace the coefficient with pooled estimate
pooled_lm1_cov2$coefficients <- summary(pooled1_cov2)$estimate
pooled_lm2_cov2$coefficients <- summary(pooled2_cov2)$estimate
pooled_lm3_cov2$coefficients <- summary(pooled3_cov2)$estimate

# get the prediction
predict1_cov2 <- predict(pooled_lm1_cov2, newdata = test_data)
predict2_cov2 <- predict(pooled_lm2_cov2, newdata = test_data)
predict3_cov2 <- predict(pooled_lm3_cov2, newdata = test_data)

# Calculate the mean squared error (MSE)
cov2_MSE1 <- mean((test_data$Body_Fat_Percentage - predict1_cov2)^2)
cov2_MSE2 <- mean((test_data$Body_Fat_Percentage - predict2_cov2)^2)
cov2_MSE3 <- mean((test_data$Body_Fat_Percentage - predict3_cov2)^2)

# fit random forest model using na.roughfix for handling missingness
rf_model2_cov2 <- randomForest(Body_Fat_Percentage ~ Pregnant + Education + Race_Ethnicity + Arm_Circumference + Waist_Circumference + Weight + Height + Age + Gender, data = train_data, ntree = 200, na.action = na.roughfix)

# calculate and save residuals from random forest
trainData_rf_predict2 <- predict(rf_model2_cov2, newdata = train_data)
rf_residuals2 <- train_data$Body_Fat_Percentage - trainData_rf_predict2
saveRDS(rf_residuals2, file = "/Users/eliabwoldegebriel/Documents/predictbodyfat/R/rf_residuals2.rds")

# Get MSE for random forest
testData_rf_predict2 <- predict(rf_model2_cov2, newdata = test_data)
cov2_MSE4 <-  mean((test_data$Body_Fat_Percentage - testData_rf_predict2)^2)

# values will be stored in lowest MSE
lowest_MSE_cov2 <- min(cov2_MSE1, cov2_MSE2, cov2_MSE3, cov2_MSE4)

# get the model with smallest value to be saved as rds
if (lowest_MSE_cov2 == cov2_MSE1) {
  saved_model2 <- pooled_lm1_cov2
} else if(lowest_MSE_cov2 == cov2_MSE2) {
  saved_model2 <- pooled_lm2_cov2
} else if(lowest_MSE_cov2 == cov2_MSE3) {
  saved_model2 <- pooled_lm3_cov2
} else {
  saved_model2 <- rf_model2_cov2
}

# save the best model
saveRDS(saved_model2, file = "/Users/eliabwoldegebriel/Documents/predictbodyfat/R/saved_model2.rds")


# then fit models using the survey weights

# fit models (pregant status removed due to poor integration of mice and survey packages)
weight_model1_cov2 <- with(combine_data_MI, svyglm(Body_Fat_Percentage ~ Education + Race_Ethnicity + Arm_Circumference + Waist_Circumference + Weight + Height + Age + Gender, design = survey_weight))
weight_model2_cov2 <- with(combine_data_MI, svyglm(Body_Fat_Percentage ~ Education + Race_Ethnicity * Weight + Arm_Circumference + Waist_Circumference * Weight * Height * Gender + Age * Weight, design = survey_weight))
weight_model3_cov2 <- with(combine_data_MI, svyglm(Body_Fat_Percentage ~ log(Age) + log(Weight) + Education + Race_Ethnicity * Weight + Arm_Circumference + Waist_Circumference * Weight * Height * Gender + Age * Weight, design = survey_weight))

# pool model
weight_pooled1_cov2 <- pool(weight_model1_cov2)
weight_pooled2_cov2 <- pool(weight_model2_cov2)
weight_pooled3_cov2 <- pool(weight_model3_cov2)

# copy one of the fitted svyglm models
weight_pooled_svyglm1_cov2 <- weight_model1_cov2$analyses[[1]]
weight_pooled_svyglm2_cov2 <- weight_model2_cov2$analyses[[1]]
weight_pooled_svyglm3_cov2 <- weight_model3_cov2$analyses[[1]]

# replace the coefficient with pooled estimate
weight_pooled_svyglm1_cov2$coefficients <- summary(weight_pooled1_cov2)$estimate
weight_pooled_svyglm2_cov2$coefficients <- summary(weight_pooled2_cov2)$estimate
weight_pooled_svyglm3_cov2$coefficients <- summary(weight_pooled3_cov2)$estimate

# get the weighted prediction
weight_predict1_cov2 <- predict(weight_pooled_svyglm1_cov2, newdata = test_data)
weight_predict2_cov2 <- predict(weight_pooled_svyglm2_cov2, newdata = test_data)
weight_predict3_cov2 <- predict(weight_pooled_svyglm3_cov2, newdata = test_data)

# calculating weighted mean squared error as new metrics in our test data
cov2_weight_MSE1 <- mean((test_data$Body_Fat_Percentage - weight_predict1_cov2)^2)
cov2_weight_MSE2 <- mean((test_data$Body_Fat_Percentage - weight_predict2_cov2)^2)
cov2_weight_MSE3 <- mean((test_data$Body_Fat_Percentage - weight_predict3_cov2)^2)

# fit weighted random forest model
weight_rf_model2 <- randomForest(Body_Fat_Percentage ~ Pregnant + Education + Race_Ethnicity + Arm_Circumference + Waist_Circumference + Weight + Height + Age + Gender, weights = train_data$combined_weight, data = train_data, ntree = 200, na.action = na.roughfix)

# calculate and save residuals from weighted random forest
trainData_weight_rf_predict2 <- predict(weight_rf_model2, newdata = train_data)
weighted_rf_residuals2 <- train_data$Body_Fat_Percentage - trainData_weight_rf_predict2
saveRDS(weighted_rf_residuals2, file = "/Users/eliabwoldegebriel/Documents/predictbodyfat/R/weighted_rf_residuals2.rds")

# Get MSE for random forest
testData_weight_rf_predict2 <- predict(weight_rf_model2, newdata = test_data)
cov2_weighted_MSE4 <-  mean((test_data$Body_Fat_Percentage - testData_weight_rf_predict2)^2)

# values will be stored in lowest weighted MSE
weighted_lowest_MSE_cov2 <- min(cov2_weight_MSE1, cov2_weight_MSE2, cov2_weight_MSE3, cov2_weighted_MSE4)

# get the model with smallest weighted value to be saved as rds
if (weighted_lowest_MSE_cov2 == cov2_weight_MSE1) {
  saved_weighted_model2 <- weight_pooled_svyglm1_cov2
} else if (weighted_lowest_MSE_cov2 == cov2_weight_MSE2) {
  saved_weighted_model2 <- weight_pooled_svyglm2_cov2
} else if (weighted_lowest_MSE_cov2 == cov2_weight_MSE3) {
  saved_weighted_model2 <- weight_pooled_svyglm3_cov2
} else {
  saved_weighted_model2 <- weight_rf_model2
}

# save the best weighted model
saveRDS(saved_weighted_model2, file = "/Users/eliabwoldegebriel/Documents/predictbodyfat/R/saved_weighted_model2.rds")
