#' BTC1859 Data Science in Health I, Assignment 5
#' Prof. Nicholas Mitsakakis, Summer Term, 2024
#' Zachery Chan, 1005468012

# Github repo for R: https://github.com/SirAcia/BTC1859.git

#' ---------------------------------------------------------------------------------------------

library(pwr)

#' ---------------------------------------------------------------------------------------------
# QUESTION 1

# Examining the context of the study, it is a two-arm, paired study, observing 2 groups
# (control/PSG and Breadth-IDX/treatment) with an initial and a follow-up 6 months later 

# As the study is paired, rho is assumed to be contained between [0,1] (i.e. no 
# assumption of potential negative correlation)


# Defining rho values and sample sizes
rho_values <- seq(0, 1, by = 0.1)
n_vals <- seq(10,110, by = 1)

# As it is paired, calculating difference in standard deviations 
sd_diff <- 4 * sqrt(2 - rho_values)

# Calculating power values at different rho values, storing in a matrix 
# using delta = 3, and a significance level of 0.05 
powervals <- cbind(power.t.test(n = n_vals, delta = 3, sd = (4 * sqrt(2 -0.0)), sig.level = 0.05, type = "paired")$power, 
                   power.t.test(n = n_vals, delta = 3, sd = (4 * sqrt(2 -0.1)), sig.level = 0.05, type = "paired")$power, 
                   power.t.test(n = n_vals, delta = 3, sd = (4 * sqrt(2 -0.2)), sig.level = 0.05, type = "paired")$power, 
                   power.t.test(n = n_vals, delta = 3, sd = (4 * sqrt(2 -0.3)), sig.level = 0.05, type = "paired")$power, 
                   power.t.test(n = n_vals, delta = 3, sd = (4 * sqrt(2 -0.4)), sig.level = 0.05, type = "paired")$power, 
                   power.t.test(n = n_vals, delta = 3, sd = (4 * sqrt(2 -0.5)), sig.level = 0.05, type = "paired")$power, 
                   power.t.test(n = n_vals, delta = 3, sd = (4 * sqrt(2 -0.6)), sig.level = 0.05, type = "paired")$power, 
                   power.t.test(n = n_vals, delta = 3, sd = (4 * sqrt(2 -0.7)), sig.level = 0.05, type = "paired")$power, 
                   power.t.test(n = n_vals, delta = 3, sd = (4 * sqrt(2 -0.8)), sig.level = 0.05, type = "paired")$power, 
                   power.t.test(n = n_vals, delta = 3, sd = (4 * sqrt(2 -0.9)), sig.level = 0.05, type = "paired")$power, 
                   power.t.test(n = n_vals, delta = 3, sd = (4 * sqrt(2 -1)), sig.level = 0.05, type = "paired")$power 
)

# Plotting power curves 
# Setting blank plot 
plot(range(n_vals), range(powervals), type = "n", xlab = "Sample Size Per Arm (n)",ylab = "Power", main = "Power Curves for Different Rho Values")

# Setting colors for power curves
colors <- c("blue", "orange", "black", "red", "green", "darkgreen", "lightgreen", "lightblue", "tomato", "darkred", "brown")

# Graphing power curves 
matlines(n_vals, powervals, type="l", lty = 1, col = colors)

# Setting legend for graph 
legend("bottomright", legend = paste("rho =", rho_values), col = colors, lty = 1, 
      title = "Rho Values", cex = 0.6)


#' ---------------------------------------------------------------------------------------------
# QUESTION 2

# Setting rho to different values to test for alternative possibilities 
rho <- seq(0.1, 0.9, by = 0.1)

# Initializing a vector to store delta values
delta <- c()

# Using for loop to calculate power for each rho value
for (i in rho) {
  
  sd_diff <- 4 * sqrt(2 - i)
  # Using n =80 here as it is n per arm
  power_result <- power.t.test(n = 80, power = 0.8, sd = sd_diff, sig.level = 0.05, type = "paired")
  
  delta <- c(delta, power_result$delta)
}

# Vector of delta values 
delta

# Making matrix to list power for each rho 
deltas <- cbind(rho, delta)

deltas 

#' ---------------------------------------------------------------------------------------------
# QUESTION 3

# Calculating sample size for a two-proportion test with power = 0.8, significance level of 0.05
new_result <- power.prop.test(p1=0.4,p2=0.3, power = 0.8, sig.level = 0.05, alternative = "one.sided")

new_result 







