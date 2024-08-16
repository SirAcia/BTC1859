
library(pwr)

# Define rho values and sample sizes
rho_values <- seq(0, 1, by = 0.1)
n_vals <- seq(10,110, by = 1)

sd_diff <- 4 * sqrt(2 - rho_values)

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

plot(range(n_vals), range(powervals), type = "n", xlab="sample size",ylab="power",main="power curves")

colors <- c("blue", "orange", "black", "red", "green", "darkgreen", "lightgreen", "lightblue", "tomato", "darkred", "brown")

matlines(n_vals, powervals, type="l", lty = 1, col = colors)

legend("bottomright",legend = paste("rho=",rho_values,sep=""),col = colors)



#Q2
rho <- seq(0.1, 0.9, by = 0.1)
delta <- c()

for (i in rho) {
  
  sd_diff <- 4 * sqrt(2 - i)
  
  power_result <- power.t.test(n = 80, power = 0.8, sd = sd_diff, sig.level = 0.05, type = "paired")
  
  delta <- c(delta, power_result$delta)
}

delta

# Q3
new_result <- power.prop.test(p1=0.4,p2=0.3, power = 0.8, sig.level = 0.05, alternative = "one.sided")
9