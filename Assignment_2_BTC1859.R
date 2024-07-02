#' BTC1859 Data Science in Health I, Assignment 2 
#' Prof. Nicholas Mitsakakis, Summer Term, 2024
#' Zachery Chan, 1005468012

# Github repo for R: https://github.com/SirAcia/BTC1859.git

# Please note that questions 1-3 are answered on the word document/written report
# Additionally, questions 4-6 flip between the word report and the R file (for interpretations, etc.) 

#' ---------------------------------------------------------------------------------------------

# QUESTION 4 
#Setting data vectors, used NA here as the question states that the 9th women did not record any data
MEN <- c(10, 12, 11.5, 9, 15, 16.5, 12, 8, 14, 15)
WOMEN <- c(10, 9, 11, 8, 13, 7, 9.5, 10, NA, 10.5)

#For Q1, the code which computes the statistical test is: 
t.test(MEN, mu=15, alternative = "less")
#' This test reports: 
#' 
#' t = -3.0485, df = 9, p-value = 0.006913
#' alternative hypothesis: true mean is less than 15
#' 95 percent confidence interval:
#'  -Inf 13.92357
#'  sample estimates:
#'  mean of x: 12.3 
#'
#' The interpretation of this test is in the written report

#For Q2, the code which computes the statistical test is: 
t.test(MEN, WOMEN, alternative = "two.sided", na.rm=T)
#' This test reports: 
#' 
#' data:  MEN and WOMEN
#' t = 2.3848, df = 15.198, p-value = 0.03053
#' alternative hypothesis: true difference in means is not equal to 0
#' 95 percent confidence interval:
#'    0.2705191 4.7739254
#' sample estimates:
#' mean of x mean of y 
#'    12.300000,  9.777778 
#' 
#' The interpretation of this test is in the written report

#For Q3, the code to run a Wilcoxon, one-sample, one-sided test: 
wilcox.test(MEN, mu=15, alternative = "less")
#' This test reports:
#'
#' data:  MEN
#' V = 2, p-value = 0.01489
#' alternative hypothesis: true location is less than 15
#' 
#' The interpretation of this test is in the written report

# For Q3, the code to run a Wilcoxon, two-sample, two-sided test: 
wilcox.test(MEN, WOMEN, alternative = "two.sided", na.rm=T)
#' This test reports:
#' 
#' data:  MEN and WOMEN
#' W = 69, p-value = 0.05416
#' alternative hypothesis: true location shift is not equal to 0
#'
#' The interpretation of this test is in the written report

# QUESTION 5 

# Part 1:
#' This question deals with 2 distinct groups, a control and a treatment group, each with a given 
#' probability of acquiring a disease state (i.e. having a myocardial infarction or MI). 
#' 
#' Even though we look at overall number of cases in each group, each sample consists of an 
#' aggregation of individual Bernoulli trials (each individual has an given odds to have an MI). 
#' Therefore, I would use a chi-square test (note that a fisher exact test can also be used here
#'  but I chose a chi square as its conditions are met and it provides a greater power). 
#' 
#' Part 2: 
#' The assumptions which need to hold for this test include: 
#' - Adequate sample size which requires 2 assumptions:
#'    - (n_1)*p*(1 − p) > 5, where p is the pooled probability from the 2 samples
#'    - (n_2)*p*(1 − p) > 5, where p is the pooled probability from the 2 samples
#' - Independence of observations (each unit of observation must be independent of others)
#' - Mutual exclusivity (each state, having an Mi or not having an MI, must be mutually exclusive from each other)

#' PART 3 
#' Visualizing the data 
#' Constructing data table 
q5_data <- matrix(c(19, 13, 189, 87), nrow = 2, byrow = TRUE)
colnames(q5_data) <- c("Streptokinase", "Control")
rownames(q5_data) <- c("Died", "Survived")
q5_data

#' Constructing contingency table
q5_contingency <- matrix(c(19, 13, 32, 189, 87, 276, 208, 200, 716), nrow = 3, byrow = TRUE)
colnames(q5_contingency) <- c("Streptokinase", "Control", "Total")
rownames(q5_contingency) <- c("Died", "Survived", "Total")
q5_contingency

# Calculating proportions and testing with pooled estimate of probability, p_hat
p_strep <- 19/189
p_control <- 13/87
p_hat <- (19+13)/(189+87)
pooled_check_strep <- p_hat*189*(1-p_hat)
pooled_check_control <- p_hat*87*(1-p_hat)
pooled_check_strep
pooled_check_control
# As these checks fulfill the >5 condition, the sample size is large enough to assume a normal approximation 

# Conducting two-sample, test for binomial distribution 
q5_result <- prop.test(q5_data)
q5_result

#' This test reports:
#' 
#' data:  q3_data
#' X-squared = 0.70836, df = 1, p-value = 0.4
#' alternative hypothesis: two.sided
#' 95 percent confidence interval:
#'   -0.2872446  0.1051794
#' sample estimates:
#'  prop 1    prop 2 
#'  0.5937500 0.6847826 
#'  
#' The interpretatuon of this test is in the written report 

# PART 4 
# Conducting Chi-square for independence 
# Do not need to use x,y arguments as x is a matrix so y is ignored
q5_independence <- chisq.test(q5_data)
q5_independence

#' This test reports:
#' 
#' data:  q5_data
#' X-squared = 0.70836, df = 1, p-value = 0.4
#' 
#' The interpretatuon of this test is in the written report 

# Part 5
#' Given that you would be examining data only from March of 1985, it is very 
#' likely the sample size for either group would be much smaller. Due to the 
#' smaller sample size, it is likely that a chi-square test cannot be used as 
#' it does not fulfill the requirements of p*n*(1-p) > 5. Therefore, I would 
#' use a Fisher Exact test to test for independence of variables
#' (i.e. testing to see if the effects of the streptokinase is independent of the control). 



#' ---------------------------------------------------------------------------------------------

# QUESTION 6

#' PART 1:
#' The statistical test that I would use for this question is the McNemar test. 
#' This is due to the data being examined is binomial (only 2 states, premature and normal)
#'  and categorical as well as the data is paired. Specifically, the data is paired 
#'  as it is intentionally matched to minimize other variables, pairing women of
#'   similar weight to better isolate the effect of treatment. 

#' The assumptions required for a McNemar test are:
#' - Paired data (in this case, the data is intentionally matched as each women is 
#'    paired with another with a similar weight). 
#' - Independence of observations (each unit of observation, in this case a pair,
#'    must be independent of others)
#' - Mutual exclusivity (each state, having an premature birth or not, must be mutually exclusive)
#' - Analyses binomial data (only 2 outcomes with a certain probability). 
#' - n_d (the number of discordant pairs) > 20  

#' PART 2:
#' Constructing data table
q6_data <- matrix(c( 30, 15, 35, 420), nrow = 2, ncol = 2,byrow = TRUE, 
                  dimnames = list(DrugA = c("Premature", "Normal"), 
                                  Placebo = c("Premature", "Normal")))
q6_data

#' Constructing contingency table
q6_contingency <- matrix(c("Drug A/Placebo", "Premature", "Normal", "Total", 
                           "Premature", 30, 15, 45, "Normal", 35, 420, 455,"Total", 
                           65, 435, 500), nrow = 4, byrow = TRUE)
q6_contingency
# The discordant pairs are premature/normal as well as normal/premature,  
# they correspond to the values [3,2] & [2,3] in the matrix q6_data

#' The condition to be satisfied for a specific McNemar test is: n_d >= 20 
#' Need to use as.numeric here as the matrix stores all data as "character"
#' as strings were enbtered with numbers
n_d_check <- sum(as.numeric(q6_contingency[3,2]), as.numeric(q6_contingency[2,3]))
n_d_check

# Since n_d > 20, we fulfill all assumption for the McNemar test and can conduct the test

#' PART 3:
# Conducting Mcnemar test 
q6_result <- mcnemar.test(q6_data)
q6_result

#' This test reports:
#' 
#' data:  q6_data
#' McNemar's chi-squared = 7.22, df = 1, p-value = 0.00721
#' 
#' The interpretation of this test is in the written report 

