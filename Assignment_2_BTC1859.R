#' BTC1859 Data Science in Health I, Assignment 2 
#' Prof. Nicholas Mitsakakis, Summer Term, 2024
#' Zachery Chan, 1005468012

# Github repo for R: https://github.com/SirAcia/BTC1859.git

# Please note that questions 1-3 are answered on the word document/written report

#' -------------------------------------

#' Q4 (4 points) Assume that the recorded values for the ten men were
#'  10, 12, 11.5, 9, 15, 16.5, 12, 8, 14, 15. The 9th woman participant 
#'  did not end up reporting the monthly intake, while values for the rest
#'   were 10, 9, 11, 8, 13, 7, 9.5, 10, 10.5, in the same order as the one 
#'   of the corresponding men.
#'   
#'   Use the proper R functions to perform ALL tests mentioned in Q1-Q3. 
#'   Report and interpret all results.

#Q4

#Setting Data vectors
MEN <- c(10, 12, 11.5, 9, 15, 16.5, 12, 8, 14, 15)
WOMEN <- c(10, 9, 11, 8, 13, 7, 9.5, 10, NA, 10.5)

#For Q1, the code which computes the statistical test is: 
t.test(MEN, mu=15, alternative = "less")
#' This test reports: 
#' ------------------------------------------------
#' t = -3.0485, df = 9, p-value = 0.006913
#' alternative hypothesis: true mean is less than 15
#' 95 percent confidence interval:
#'  -Inf 13.92357
#'  sample estimates:
#'  mean of x: 12.3 
#' ------------------------------------------------
#' Interpreting this test, the t-statistic is large and negative (inditcating a leftward psoition on the assumned normal distribution of the data)
#' This is validated by the extremely small p value of 0.000613. As thsi p-value is below 
#' the 0.05 threshold, we can reject the null hypothesis as this indicates there is an approx. 
#' 0.6913% chnace of random variotion in sampling resulting in a observed mean as small as 12.3 or smaller given taht teh null is true. 
#' The confidence interval (CI) given by this test also supports the 
#' alternatuve hypothesis, indicating taht for this analysis framewokr, 
#' 95% of the time the true populauton mean for men in the dietary program lies 
#' bwteen engative infinity and 13.92, well below 15
#' 
#' Thus, given the small p-value (below 0.05) we can state the following: 
#' For the male smaple group from this dietary program, this statitsical test provides string evidence to confidently support rejecting the null hypothesis, 
#' providing strong evidence that the monthly intake for the same group in this dietary program is likely less than average of the general populatuon of 15g. 



#For Q2, the code which computes the statistical test is: 
t.test(MEN, WOMEN, alternative = "two.sided", na.rm=T)
#' This test reports: 
#' ------------------------------------------------
#' data:  MEN and WOMEN
#' t = 2.3848, df = 15.198, p-value = 0.03053
#' alternative hypothesis: true difference in means is not equal to 0
#' 95 percent confidence interval:
#'    0.2705191 4.7739254
#' sample estimates:
#' mean of x mean of y 
#'    12.300000,  9.777778 
#' ------------------------------------------------
#' Interpreting this test, the t-statistic is large and positive, resulting in a p-value of 0.03053
#' As this p-value is below the 0.05 threshold, we can reject the null hypothesis as this indicates 
#' there is an approx. 3.05% chnace of the observed difference between the men and women groups being as extreme as 
#'  (12.3-9.7) or more being attributed to randowm samplong variation under the null hypothesis
#' Given the small p-value, this gives strong evidence to reject the null hypothesis which is corroborated 
#' by the 95% CI interval of 0.2705191 - 4.7739254 as ___ lies within that range. As mentioned earlier, indictaing taht the 
#' difference between the true population paraemters lies between 0.2705191 - 4.7739254 95% 
#' of the time given using this specific analysis. 
#' 
#' Thus, given the small p-value (below 0.05) we can state the following: 
#' For the male and female sample group from this dietary program, this two-sided, two-sample t-test provides string evidence to support rejecting the null hypothesis, 
#' providing evidence top suggest taht it is. ikely that the monthly intake of linoleic acid is different between the male and female sample group. 

#For Q3, the code to run a Wilcoxon, one-sample, one-sided test: 
wilcox.test(MEN, mu=15, alternative = "less")
#' This test reports:
#' ------------------------------------------------
#' data:  MEN
#' V = 2, p-value = 0.01489
#' alternative hypothesis: true location is less than 15
#' ------------------------------------------------
#' Importantly, a Wilcoxon test measures the median (and by proxy, the variance) of data rather than the mean 
#' so this test gives evidence to determine whether the median of the sample group is less than the proposed median of 15 for the general populatin 
#' Similar to earlier the one-sided  t-test, the small V value suggests that the median of the sample group is on the left of the distribution, below the median of the general population 
#' This is further supported by the small p-value of 0.01489 suggesting that there is a 1.49% chnace of the sample median being less than 15 being attributed to sampling/statitsical error assumging the null hypoethsis is correct
#For Q3, the code to run a Wilcoxon, two-sample, two-sided test: 
wilcox.test(MEN, WOMEN, alternative = "two.sided", na.rm=T)
#' This test reports:
#' ------------------------------------------------
#' data:  MEN and WOMEN
#' W = 69, p-value = 0.05416
#' alternative hypothesis: true location shift is not equal to 0
#' ------------------------------------------------



#For Q5
#for part 1
#' I would use a 2 sample t-test 
#' Assumptions: Normal distribution, unequal variance
#' 
#' 










