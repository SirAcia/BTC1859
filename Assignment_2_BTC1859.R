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

#For Q3, the code to run a Wilcoxon, one-sample, one-sided, rank-sign test: 
wilcox.test(MEN, mu=15, alternative = "less")

#' This test reports:
#'
#' data:  MEN
#' V = 2, p-value = 0.01489
#' alternative hypothesis: true location is less than 15
#' 
#' The interpretation of this test is in the written report

# For Q3, the code to run a Wilcoxon, two-sample, two-sided rank-sum test: 
wilcox.test(MEN, WOMEN, alternative = "two.sided", na.rm=T)

#' This test reports:
#' 
#' data:  MEN and WOMEN
#' W = 69, p-value = 0.05416
#' alternative hypothesis: true location shift is not equal to 0
#'
#' The interpretation of this test is in the written report



#' ---------------------------------------------------------------------------------------------
# QUESTION 5 

#' PART 3 
#' Visualizing the data 
#' Constructing data table 
strep_failures <- 189 - 19
control_failures <- 87 - 13
strep = c(19, strep_failures)
control = c(13, control_failures)
q5_data <- as.table(matrix(c(strep, control),nrow=2,byrow = T))
colnames(q5_data) <- c("Treatment.success", "Treatment.failure")
rownames(q5_data) <- c("Control.success", "Control.failure")
q5_data

#' Constructing contingency table
q5_contingency <- matrix(c(19, 170, 189, 13, 74, 87, 32, 244, 276), nrow = 3, byrow = TRUE)
colnames(q5_contingency) <- c("Treatment.success", "Treatment.failures", "Total")
rownames(q5_contingency) <- c("Control.success", "Control.failures", "Total")
q5_contingency

# I constructed a matrix for just the data for analysis, the constructed contingency 
# table (see below) was constructed to better visualize the data  

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
#' data:  q5_data
#' X-squared = 0.95354, df = 1, p-value = 0.3288
#' alternative hypothesis: two.sided
#' 95 percent confidence interval:
#'   -0.14360110  0.04580872
#' sample estimates:
#'   prop 1    prop 2 
#'   0.1005291 0.1494253 

#' The interpretation of this test is in the written report 


# PART 4 
# Conducting Chi-square for independence 
# Do not need to use x,y arguments as x is a matrix so y is ignored
q5_independence <- chisq.test(q5_data)
q5_independence

#' This test reports:
#' 
#' data:  q5_data
#' X-squared = 0.95354, df = 1, p-value = 0.3288
#' 
#' The interpretation of this test is in the written report 



#' ---------------------------------------------------------------------------------------------

# QUESTION 6

#' Note that the hypothesis for this question 
#' are stated in the written report

#' PART 2:
#' Constructing data table
q6_data <- matrix(c( 30, 15, 35, 420), nrow = 2, ncol = 2,byrow = TRUE, 
                  dimnames = list(DrugA = c("Premature", "Normal"), 
                                  Placebo = c("Premature", "Normal")))
q6_data
# I constructed a matrix for just the data for analysis, the constructed contingency 
# table (see below) was constructed to better visualize the data & discordant pairs 

#' Constructing contingency table
q6_contingency <- matrix(c("Drug A/Placebo", "Premature", "Normal", "Total", 
                           "Premature", 30, 15, 45, "Normal", 35, 420, 455,"Total", 
                           65, 435, 500), nrow = 4, byrow = TRUE)
q6_contingency
# The discordant pairs are premature/normal as well as normal/premature,  
# they correspond to the values [3,2] & [2,3] in the matrix q6_data

#' The condition to be satisfied for a specific McNemar test is: n_d >= 20 
#' Need to use as.numeric here as the matrix stores all data as "character"
#' as strings were entered with numbers
n_d_check <- sum(as.numeric(q6_contingency[3,2]), as.numeric(q6_contingency[2,3]))
n_d_check

# Since n_d > 20, we fulfill all assumption for the McNemar test and can conduct the test

#' PART 3:
# Conducting McNemar test 
q6_result <- mcnemar.test(q6_data)
q6_result

#' This test reports:
#' 
#' data:  q6_data
#' McNemar's chi-squared = 7.22, df = 1, p-value = 0.00721
#' 
#' The interpretation of this test is in the written report 

