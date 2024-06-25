



#' Q4 (4 points) Assume that the recorded values for the ten men were
#'  10, 12, 11.5, 9, 15, 16.5, 12, 8, 14, 15. The 9th woman participant 
#'  did not end up reporting the monthly intake, while values for the rest
#'   were 10, 9, 11, 8, 13, 7, 9.5, 10, 10.5, in the same order as the one 
#'   of the corresponding men.
#'   
#'   Use the proper R functions to perform ALL tests mentioned in Q1-Q3. 
#'   Report and interpret all results.

#Setting Data vectors
MEN <- c(10, 12, 11.5, 9, 15, 16.5, 12, 8, 14, 15)
WOMEN <- c(10, 9, 11, 8, 13, 7, 9.5, 10, NA, 10.5)

#For Q1
t.test(MEN, mu=15, alternative = "less")

#For Q2
t.test(MEN, WOMEN, alternative = "two.sided", na.rm=T)

#For Q3
wilcox.test(MEN, mu=15, alternative = "less")

wilcox.test(MEN, WOMEN, alternative = "two.sided", na.rm=T)


#For Q4


