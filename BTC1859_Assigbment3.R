#' BTC1859 Data Science in Health I, Assignment 3 
#' Prof. Nicholas Mitsakakis, Summer Term, 2024
#' Zachery Chan, 1005468012

# Github repo for R: https://github.com/SirAcia/BTC1859.git

#' ---------------------------------------------------------------------------------------------

# QUESTION 1 
# Attaching library to use in analysis 
library(UsingR)

# Using help window to understand variables
?babies

# Examining the babies data set (not asked for in question, 
# kept in for good practice to examine the data beyond reading the help window)
summary(babies)

str(babies)

head(babies)

# Seeing if any NAs in the data
anyNA(babies)

#' Most notable concerns after examining the data is the max of 999 for 
#' gestation, that the data contains only males (may be due to original data set),
#' no NAs in the original dataframe, no use of factoers for categorical variables 

attach(babies)
# Creating dataframe containing mother's weight and babies' weight from original data
wgt <- data.frame(baby_wgt = c(babies$wt), mother_wgt = c(babies$wt1))

summary(wgt)

# Already checked for extreme values earlier, creating new dataframe to address NAs
# (listed as 999) in mother's weight. using new dataframe to ensure data integrity
wgt1 <- wgt

# New column for mother's wieght + NAs 
wgt1$mother_wgt_alt <- c(wgt1$mother_wgt)

# Converting 999 to NA as defined by help window 
wgt1$mother_wgt_alt[wgt1$mother_wgt == 999] <- NA

anyNA(wgt1$mother_wgt_alt)

summary(wgt1)

# Creating scatter plot between mother's weight and babies birthweight 
x <- wgt1$mother_wgt_alt
y <- wgt1$baby_wgt
plot(x,y, xlab = "Mother's Pre-Birth Weight (lbs)", ylab = "Newborn Birthweight (oz)")
title(main = "Mother's Weight vs Babies' Birthweight")

# Observations are detailed in written report 


#' ---------------------------------------------------------------------------------------------

# QUESTION 2

# Defining linear model 
wgt_model <- lm(baby_wgt~mother_wgt_alt, data = wgt1)

# examining p-values, r-squared, conf. intervals, etc. 
summary(wgt_model)
confint(wgt_model)

#plotting scatter 
plot(wgt_model)


#' ---------------------------------------------------------------------------------------------

# QUESTION 3

# Defining new data frame with same column name/predictor variable name to match linear model 
msX <- data.frame(mother_wgt_alt = c(160))

# Using predict(..., type = "p") as this is a prediction for new data with an exact value 
predict(wgt_model, newdata = msX, int = "p", level = 0.95)
#level argument here can be considered redundant but included for sake of readability & clarity 

#' ---------------------------------------------------------------------------------------------

# QUESTION 4

# Using predict(..., type = "c") as this is a prediction for an expected average mean 
predict(wgt_model, newdata = msX, int = "c", level = 0.95)
#level argument here can be considered redundant but included for sake of readability & clairity 


#' ---------------------------------------------------------------------------------------------

# QUESTION 5
# Question 5 is answered in the word document


#' ---------------------------------------------------------------------------------------------

# QUESTION 6

# Running a correlation test for the response and predictor variables
wgt_corr <- cor.test(wgt1$baby_wgt, wgt1$mother_wgt_alt, alternative = "two.sided", method = "pearson", conf.level = 0.95)
# Defining levels here is redundant as default is 0.95 but kept for clarity 
wgt_corr

