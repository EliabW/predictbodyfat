# load the models
# saved_model1 <- readRDS("/Users/eliabwoldegebriel/Documents/predictbodyfat/R/saved_model1.rds")
# saved_weighted_model1 <- readRDS("/Users/eliabwoldegebriel/Documents/predictbodyfat/R/saved_weighted_model1.rds")
# saved_model2 <- readRDS("/Users/eliabwoldegebriel/Documents/predictbodyfat/R/saved_model2.rds")
# saved_weighted_model2 <- readRDS("/Users/eliabwoldegebriel/Documents/predictbodyfat/R/saved_weighted_model2.rds")
# rf_residuals <- readRDS("/Users/eliabwoldegebriel/Documents/predictbodyfat/R/rf_residuals1.rds")
# weighted_rf_residuals <- readRDS("/Users/eliabwoldegebriel/Documents/predictbodyfat/R/weighted_rf_residuals1.rds")
# rf_residuals2 <- readRDS("/Users/eliabwoldegebriel/Documents/predictbodyfat/R/rf_residuals2.rds")
# weighted_rf_residuals2 <- readRDS("/Users/eliabwoldegebriel/Documents/predictbodyfat/R/weighted_rf_residuals2.rds")

#' Predicts or imputes body fat percentage
#'
#' The function predicts or imputes body fat percentage based on user input data
#' The data set provided must have the data must have the exact variable names/forms described below
#' Only complete cases to receive predictions, otherwise NA will be returned for any incomplete rows
#' Imputation or multiple imputation for predictors should be done before using this function
#'
#' @param data takes the user input data set with the required variables for prediction
#' @param weighted a logical value that determines whether predictions come from models fit with survey weights (TRUE) or not (FALSE)
#' @param type a character string that allows users to "predict" for standard prediction or "impute" for multiple imputation
#' @param num_imp an integer that indicates number of imputations user want to generate if type = "impute"
#' @param covariate_set an integer that specifies which set the user wants to use for prediction (set 1 = 1 and set 2 = 2)
#'   set 1 includes
#'       Age (in years)
#'       Gender (binary: 0 = male, 1 = female)
#'       Height (in cm)
#'       Weight (in kg)
#'.  set 2 includes the above four variables, plus the following
#'        Pregnant (0 = no, 1 = yes)
#'        Education (0 = child < 20 years old, 1 = less than 9th grade, 2 = 9-11th grade (or 12th with no diploma), 3 = high school or GED, 4 = some college or AA, 5 = college graduate or above)
#'        Race_Ethnicity (1 = Hispanic, 2 = Non-Hispanic White, 3 = Non-Hispanic Black, 4 = Other Race, including Multi-Racial)
#'        Waist_Circumference (in cm)
#'        Arm_Circumference (in cm)
#'
#' @return A vector of body fat percentage prediction without imputation (type = "predict") or a matrix of imputed values (type = "impute")
#' @export
#'
#' @examples
#' Example of standard prediction
#' df <- data.frame(
#'     Age = c(35,38),
#'     Gender = c("Female","Male"),
#'     Height = c(150,180),
#'     Weight = c(70,60)
#'     )
#' predictBodyFat(data = df, weighted = FALSE, type = "predict", covariate_set = 1)
#'
#' Example of multiple imputation
#' df2 <- data.frame(
#'    Age = c(35,38,NA,NA),
#'    Gender = as.factor(c("Female","Male",NA,NA)),
#'    Height = c(150,180,NA,NA),
#'    Weight = c(70,60,NA,NA),
#'    Pregnant = c(1,0,NA,NA),
#'    Education = c(4,5,NA,NA),
#'    Race_Ethnicity = as.factor(c("Hispanic", "Non-Hispanic White","Non-Hispanic Black","Other Race - Including Multi-Racial")),
#'    Arm_Circumference = c(20,25,NA,NA),
#'    Waist_Circumference = c(50,60,NA,NA)
#'    )
#' predictBodyFat(data = df2, weighted = FALSE, type = "impute", num_imp = 2, covariate_set = 2)
#' @import survey
#' @import randomForest
#' @import splines
predictBodyFat <- function(data, weighted = FALSE, type = "predict", num_imp = 1, covariate_set = 1) {
  # select model to use based on covariate set and weighted argument
  if(covariate_set == 1) {
    if(weighted == TRUE) {
      saved_model <- saved_weighted_model1
      rf_residual <- weighted_rf_residuals
    } else {
      saved_model <- saved_model1
      rf_residual <- rf_residuals
    }
  }
  # Checks that covariate set == 2
  else if (covariate_set == 2) {
    if(weighted == TRUE){
      saved_model <- saved_weighted_model2
      rf_residual <- weighted_rf_residuals2
    } else {
      saved_model <- saved_model2
      rf_residual <- rf_residuals2
    }
  }

  # adjust factor levels to address predict.randomForest() bug
  levels(data$Gender) <- c("Female", "Male")
  if (covariate_set == 2) {
    levels(data$Race_Ethnicity) <- c("Hispanic", "Non-Hispanic White",
                                     "Non-Hispanic Black",
                                     "Other Race - Including Multi-Racial")
  }


  # initialize the predict_fat vector
  predict_fat <- rep(NA, nrow(data))

  # Handle prediction of chosen model output differently for weighted or unweighted
  if(weighted == TRUE) {
    # get rows with no missing values
    rows_no_NA <- complete.cases(data)

    # predict chosen model with weighted survey
    predict_fat[rows_no_NA] <- predict(saved_model, newdata = data[rows_no_NA, ])
  } else {
    # predict chosen model without weighted survey
    predict_fat <- c(predict(saved_model, newdata = data))
  }

  # checks if type is impute
  if(type == "impute") {
    # create an empty matrix
    matrix_imp <- matrix(0, nrow = nrow(data), ncol = num_imp)

    # fill the empty matrix
    for(i in 1:num_imp) {
      # checks if the chosen model's class is lm or random forest
      if("lm" %in% class(saved_model)) {
        sd_residuals <- sd(saved_model$residuals)
      } else {
        sd_residuals <- sd(rf_residual, na.rm = T)
      }

      # create randomness
      random_noise <- rnorm(dim(data)[1], 0, sd_residuals)

      # add random noise to the prediction
      matrix_imp[,i] <- predict_fat #'random_noise
    }
    # return a matrix of imputed values
    return(matrix_imp)
  } else {
    # return a vector of body fat percentage prediction without imputation
    return(predict_fat)
  }
}
