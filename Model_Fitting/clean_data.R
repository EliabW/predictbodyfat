source("/Users/eliabwoldegebriel/Documents/predictbodyfat/Model_Fitting/load_data.R")

# load dplyr if not sourcing load_data.R
library(dplyr)

# save NHANES-provided imputations of body fat percentage (DXDTOPF)
BFP_imputations <- combined_data[, c("DXDTOPF", "X_MULT_")]

# subset to the complete data for the first imputation
combined_data <- combined_data[combined_data$`X_MULT_` == 1, ]

# combined weight is used to calculate the sampling weights for NHANES data from 1999-2006 combined data
combined_data <- combined_data %>% mutate(combined_weight = ifelse(combined_data$SDDSRVYR %in% c(1,2),
                                                                   2/4 * combined_data$WTMEC4YR,
                                                                   1/4 * combined_data$WTMEC2YR))

# collapse Mexican American and Other Hispanic into one category
combined_data$RIDRETH1[combined_data$RIDRETH1 == "Mexican American"] <- "Other Hispanic"
combined_data$RIDRETH1 <- droplevels(combined_data$RIDRETH1)
levels(combined_data$RIDRETH1) <- c("Hispanic", levels(combined_data$RIDRETH1)[2:4])

# education (20+) as numeric, with missing levels collapsed into one category
combined_data$DMDEDUC2 <- as.numeric(combined_data$DMDEDUC2)
combined_data$DMDEDUC2[combined_data$DMDEDUC2 == 7 | combined_data$DMDEDUC2 == 9] <- NA

# and a new level 0 added for children
combined_data$DMDEDUC2[combined_data$RIDAGEYR < 20] <- 0

# pregnant status with missing levels collapsed into one category
combined_data$RIDEXPRG <- as.numeric(combined_data$RIDEXPRG)
combined_data$RIDEXPRG[combined_data$RIDEXPRG == 3] <- NA

# re-code the pregnant status as 1 for pregnant and 0 for not pregnant
combined_data$RIDEXPRG <- abs(combined_data$RIDEXPRG - 2)

# set pregnant status to 0 for male-identified participants
combined_data$RIDEXPRG[combined_data$RIAGENDR == "Male"] <- 0

# rename variables of interest from the data to make easy to interpret
combined_data <- combined_data %>% rename(Body_Fat_Percentage = DXDTOPF, Pregnant = RIDEXPRG,
                                          Education = DMDEDUC2, Race_Ethnicity = RIDRETH1,
                                          Arm_Circumference = BMXARMC, Waist_Circumference = BMXWAIST,
                                          Gender = RIAGENDR, Age = RIDAGEYR, Height = BMXHT,
                                          Weight = BMXWT, BMI = BMXBMI, Systolic_Blood_Pressure1 = BPXSY1)

# select variables of interest
combined_data <- combined_data %>% select(Body_Fat_Percentage, Pregnant, Education,
                                          Race_Ethnicity, Arm_Circumference, Waist_Circumference,
                                          Gender, Age, Height, Weight, BMI, combined_weight, year, Systolic_Blood_Pressure1)

# add id variable
combined_data[, 'id'] <- 1:nrow(combined_data)

# set the ids in BFP_imputations
BFP_imputations$id <- rep(combined_data$id, each = 5)

# save the data
save(combined_data, file = "/Users/eliabwoldegebriel/Documents/predictbodyfat/R/combined_data.RData")
