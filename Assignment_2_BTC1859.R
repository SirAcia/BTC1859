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
#' Importantly, a Wilcoxon test measures the median (and by proxy, the variance and distribution) of data rather than the mean 
#' so this test gives evidence to determine whether the median of the sample group is less than the proposed median of 15 for the general populatin 
#' Similar to earlier the one-sided  t-test, the small V value suggests that the median of the sample group is on the left of the distribution, below the median of the general population 
#' This is further supported by the small p-value of 0.01489 suggesting that there is 
#' a 1.49% chnace of the sample median being less than 15 being attributed to 
#' sampling/statitsical error assumging the null hypoethsis is correct. 
#' 
#' Thus, this wilcoxon test provides confidence to reject the null hypothesis as it is likely that median of the sample group is not the same as the true median of the general population for monthly linoleic acid intake. 


# For Q3, the code to run a Wilcoxon, two-sample, two-sided test: 
wilcox.test(MEN, WOMEN, alternative = "two.sided", na.rm=T)
#' This test reports:
#' ------------------------------------------------
#' data:  MEN and WOMEN
#' W = 69, p-value = 0.05416
#' alternative hypothesis: true location shift is not equal to 0
#' ------------------------------------------------
#' This analysis and interpretation is veyer simialr to the previous analysis of the one-sided Wilcoxon test. 
#' The two-sample, two-sided Wilcoxon test analyses the distribution (and median) of the two samples.
#' Given the high W value and a p-value of 0.05416, this two-sided Wilcoxon test does not provide 
#' sufficient confidence to reject the null hypothesis as it is above an alpha level of 0.05
#' 
#' Overall, this two-sided, two-sample, Wilxcoxon test doe snot privides sufficient confidence reject the null hypothesis
#' prividing insufficient evidence to suggets taht there is a difference in the medians, and thus ditributions, of the male and female sample groups for this specific dietary program. 



#For Q5
# Part 1
#' This question deals with 2 distinct groups, a control and a treatment group, each with a given probability of acquirinf a disease state (i.e. having a myocardial infarction or MI). 
#' 
#' Thus, even though we look at overall number of cases in each group, each sample consists of an aggregatation of individual bernoulli ____ (each individual has an odds to have an MI). 
#' Therefor, I would use a chi-square test (note that a fisher exact test can also be used here but I chose a chi square as its conditions are met and it provides a grester power)
#' 
#' Part 2: 
#' The assumoptions which need to hold for this test include: 
#' - Adequatre sample size which requires 2 assumptions:
#'    - (n_1)*p*(1 − p) > 5
#'    - (n_2)*p*(1 − p) > 5
#' - independence of observations (each unit of observation must be indpedent of others)
#' - Mutual exclusvitiy (each state, having an Mi or not having an MI, must be mutually exclusive)
#' Assumptions: Normal distribution, unequal variance

#' PART 3 
#' Visualizing the data 
#' Constructing data table 
q3_data <- matrix(c(19, 13, 189, 87), nrow = 2, byrow = TRUE)
colnames(q3_data) <- c("Streptokinase", "Control")
rownames(q3_data) <- c("Died", "Survived")
print (q3_data)

#' Constructing contingency table
q3_contingency <- matrix(c(19, 13, 32, 189, 87, 276, 208, 200, 716), nrow = 3, byrow = TRUE)
colnames(q3_contingency) <- c("Streptokinase", "Control", "Total")
rownames(q3_contingency) <- c("Died", "Survived", "Total")
print(q3_contingency)

# Calculating proportions and testing normal adssumption, without pooled 
p_strep <- 19/189
p_control <- 13/87
strep_size <- 189*p_strep*(1-p_strep)
control_size <- 87*p_control*(1-p_control)

# With pooled 
p_hat <- (19+13)/(189+87)
pooled_check_strep <- p_hat*189*(1-p_hat)
pooled_check_control <- p_hat*87*(1-p_hat)
# as these chekcs fulfill the >5 condition, the sample size is large enough to assume a normal approximation 

#Conducting two-samnple, test for binomial 
q3_result <- prop.test(q3_data)
print (q3_result)

#' This test reports:
#' ------------------------------------------------
#' data:  q3_data
#' X-squared = 0.70836, df = 1, p-value = 0.4
#' alternative hypothesis: two.sided
#' 95 percent confidence interval:
#'   -0.2872446  0.1051794
#' sample estimates:
#'  prop 1    prop 2 
#'  0.5937500 0.6847826 
#' ------------------------------------------------
#'

# PART 4 
#Conductign Chi-square for independence 
#do not need to use x,y argiments as x isa matrix so y is ignored
q3_independence <- chisq.test(q3_data)
print(q3_independence)
#' This test reports:
#' ------------------------------------------------
#' data:  q3_data
#' X-squared = 0.70836, df = 1, p-value = 0.4
#' #' ------------------------------------------------
#'

# Part 5
#' Given that you would be examining data onyl from marhc of 1985, it is very leikely the sample size for either gorup would be much smaller. 
#' Due to the smaller sample size, ti is likely that a chi-square test cannot be
#'  used as it does not fulfill the requiremnets of _____ >5 . Therefore, I would use a Fisher Exact test to test for independence of variables
#'  (i.e. testing to see if teh effects of the streptokinase is indpendent of the control). 


# Q6 
#' Constructing data table
q6_data <- matrix(c( 30, 15, 35, 420), nrow = 2, ncol = 2,byrow = TRUE, dimnames = list(DrugA = c("Premature", "Normal"), Placebo = c("Premature", "Normal")))
print(q6_data)

#' Constructing contingency table
q6_contingency <- matrix(c("Drug A/Placebo", "Premature", "Normal", "Total", "Premature", 30, 15, 45, "Normal", 35, 420, 455,"Total", 65, 435, 500), nrow = 4, byrow = TRUE)
print(q6_contingency)
#Seeing the discordant pairs heterogenous pairs (i.e. pre&normal or normal&pre), 
# thus they correspond to the values [3,2] & [2,3]
 
#The condition to be satisfied for a specific McNemar test is: n_d >= 20 
# Need to use as.numeric here as the matrix stores all data as "chatacter" as strings were enbtered with numbers
n_d_check <- sum(as.numeric(q6_contingency[3,2]), as.numeric(q6_contingency[2,3]))
print(n_d_check)

#As n_d > 20, this fulfills the condition for McNemar. The other assumptions required for a McNemar test are:
#' - Paired data (in this case, the data is intentionally matched as each women is paired with another with a similar weight). 
#' -independence of observations (each unit of observation, in this case a pair, must be indpedent of others)
#' - Mutual exclusvitiy (each state, having an premature birth or not, must be mutually exclusive)
#' - analyses bionomial data (only 2 outcomews with a certain pribability). 
#' 

#Condcuting Mcnemar test 
q6_result <- mcnemar.test(q6_data)
q6_result
#' This test reports:
#' ------------------------------------------------
#' data:  q6_data
#' McNemar's chi-squared = 7.22, df = 1, p-value = 0.00721
#' #' ------------------------------------------------
#'


