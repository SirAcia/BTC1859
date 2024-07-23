#' BTC1859 Data Science in Health I, Assignment 4
#' Prof. Nicholas Mitsakakis, Summer Term, 2024
#' Zachery Chan, 1005468012

# Github repo for R: https://github.com/SirAcia/BTC1859.git

# 

#' ---------------------------------------------------------------------------------------------

library(UsingR)
?babies

#' ---------------------------------------------------------------------------------------------
# QUESTION 1
# Saving original dataset to preserve original data integrity 
data <- babies 

# Cleaning data 
# Also cleaning data for later questions 

summary(babies)

# From summary and description in help window, see that there are missing values listed as 9, 99, 999, etc. 
# Converting coded missing values into NAs 

# Converting 99s in mothers height into NAs 
data$ht[data$ht == 99] <- NA
anyNA(data$ht)
summary(data$ht)
#From summary, no evidently extreme outliers 

# Converting 9s in smoke into NAs 
data$smoke[data$smoke == 9] <- NA
anyNA(data$smoke)

# Converting 999s in mothers weight into NAs 
data$wt1[data$wt1 == 999] <- NA
anyNA(data$wt1)
#From summary, no evidently extreme outliers 
summary(data$wt1)

summary(data$gestation)
# From summary of gestation, see presence of 999, likely outlier or NA (not explicitly specified in help)
gest_sort <- sort(data$gestation, decreasing = T, 10)
head(gest_sort, 20)
# Removing 999s, setting as NAs 
data$gestation[data$gestation == 999] <- NA
anyNA(data$gestation)

# Converting 99s in mothers race into NAs 
data$race[data$race == 99] <- NA
anyNA(data$race)

# Factoring race using ifelse to specify sorting bins
data$race_fctr <- ifelse(data$race <= 5, "white",
                         ifelse(data$race == 6, "mex", 
                                ifelse(data$race == 7, "black", 
                                       ifelse(data$race == 8, "asian", "mixed"))))

# Setting levels in factored race, setting white as default in logarithmic regression 
data$race_fctr <- factor(data$race_fctr, levels = c("white", "black", "mex", "asian", "mixed"))

# Creating BMI variable
data$BMI <-  (data$wt1/2.2)/(data$ht*2.54/100)^2

# Creating binary gestation variable, 1 = premature (SUCCESS), 0 = normal (FAILURE)
data$premature <- ifelse(data$gestation < (37*7), 1, 0)
# Using < (37*7) as gestation is in days, but want to sort by whether gestation > 37 weeks

# From earlier summary, only 0-3 values in smoke after cleaning 
# So only factoring into 4 bins
data$smoke_fctr <- factor(data$smoke, levels =c(0, 1, 2, 3), labels = c("Never", "Still", "During", "Within 1 yr"))

# Using na.omit to remove rows with NAs, allows for the two models (created below) 
# to be compared as they will be using the same dataset with the same length & size 
data2 <- na.omit(data)

#' ---------------------------------------------------------------------------------------------
# QUESTION 2

# Creating first logarithmic model predicting binary response variable, premature, 
# from predictor variables BMI & smoke (4 level factor, default as non-smoker)
BMI_model <- glm(premature~BMI+smoke_fctr,data = data2,family = binomial)
summary(BMI_model)

# The interpretation of the model is contained within the written report 


#' ---------------------------------------------------------------------------------------------
# QUESTION 3 

# Calculating Odds Ratio for each coefficient
round(exp(BMI_model$coefficients),2)

# Calculating 95% confidence levels for each coefficient 
round(exp(confint(BMI_model, level = 0.95)),2)
# Note that the levels = 0.95 argument is not technically needed as 
# default is 0.95, kept for readability

# Interpretation of the odds ratios & CIs are contained within the written report 


#' ---------------------------------------------------------------------------------------------
# QUESTION 4 

# Creating second logarithmic model predicting binary response variable, premature, 
# from predictor variables BMI, smoke (4 level factor, reference as non-smoker), and 
# race (4 level factor with white as reference level)
BMI_model_2 <- glm(premature~BMI+smoke_fctr+race_fctr,data = data2,family = binomial)
summary(BMI_model_2)

# Using ANOVA to perform goodness-of-fit test between the models
anova(BMI_model, BMI_model_2, test = "Chisq")

# Interpretation and comparison of the two models is in the written report


#' ---------------------------------------------------------------------------------------------
# QUESTION 5

# Inputting new data for each variable to get exact prediction
newdata <- data.frame(BMI=21, race_fctr="black", smoke_fctr="Never")

# Exact prediction from second logarithmic model for specified predictor values
predict(BMI_model_2, newdata = newdata, type = "response")








