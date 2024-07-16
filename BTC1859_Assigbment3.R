#' BTC1859 Data Science in Health I, Assignment 3 
#' Prof. Nicholas Mitsakakis, Summer Term, 2024
#' Zachery Chan, 1005468012

# Github repo for R: https://github.com/SirAcia/BTC1859.git

#' ---------------------------------------------------------------------------------------------

# QUESTION 1 

library(UsingR)
library(dplyr)
library(ggplot2)

?babies

summary(babies)

str(babies)

head(babies)

str(babies)

anyNA(babies)

attach(babies)

#' Most notable concerns is the max of 999 for gestation, only males 
#' (may be due to original data set), no NAs in the original dataframe, no use of factoers for categorical variables 

wgt <- data.frame(baby_wgt = c(babies$wt), mother_wgt = c(babies$wt1))

summary(wgt)

wgt1 <- wgt

wgt1$mother_wgt_alt <- c(wgt1$mother_wgt)

wgt1$mother_wgt_alt[wgt1$mother_wgt == 999] <- NA
  
head(wgt1)

anyNA(wgt1$mother_wgt_alt)

summary(wgt1)

x <- wgt1$mother_wgt_alt
y <- wgt1$baby_wgt
plot(x,y, xlab = "Mother's Pre-Birth Weight (lbs)", ylab = "Newborn Birthweight (oz)")
title(main = "Mother's Weight vs Babies' Birthweight")

#Observations? 

#' ---------------------------------------------------------------------------------------------

# QUESTION 2
wgt_model <- lm(baby_wgt~mother_wgt_alt, data = wgt1)

summary(wgt_model)

plot(wgt_model)
confint(wgt_model)


#' ---------------------------------------------------------------------------------------------

# QUESTION 3
msX <- data.frame(mother_wgt_alt = c(160))
predict(wgt_model, newdata = msX, int = "p", level = 0.95)
#level argument here can be considered redundant but included for sake of readanbility & clairity 

#' ---------------------------------------------------------------------------------------------

# QUESTION 4
predict(wgt_model, newdata = msX, int = "c", level = 0.95)
#level argument here can be considered redundant but included for sake of readanbility & clairity 

#' ---------------------------------------------------------------------------------------------

# QUESTION 5

#' Given that covariance is between -1 and 1, with 1 being perfect corrwlartion, I would argue this is positive and approx. 0.5
#' This is because the data seesm closely correlated, realtively gerouped together, with variation in either weights matching direction to variation in the other. 
#' (i.e. when mother birthrate is smaller than average, the birthweight is smaller as well and vice-versa) however, ecspecially with the heavier pre-birthweight, this relationship is not as strong
#' justifying the 0.5 coefficient estimate rather than a higher value. Biologically this is supported as well, if a mother is of lower weight due to nutrition or physical stature, there is 
#' biological limitations & pressure for a smaller birthweight (i.e. less nutrients so msaller baby or genetic bias toward individuals of a smaller stature)

#' ---------------------------------------------------------------------------------------------

# QUESTION 6
wgt_corr <- cor.test(wgt1$baby_wgt, wgt1$mother_wgt_alt, alternative = "two.sided", method = "pearson", conf.level = 0.95)
wgt_corr

