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

wgt <- data.frame(data = c(c(babies$wt), c(babies$wt1)), ncol = 2)
colnames(wgt) = c("baby_wgt", "mother_wgt")

summary(wgt)

wgt1 <- wgt

wgt1$mother_wgt_alt <- c(wgt1$mother_wgt)

wgt1$mother_wgt_alt[wgt1$mother_wgt == 999] <- NA
  
head(wgt1)

anyNA(wgt1$mother_wgt_alt)

summary(wgt1)
