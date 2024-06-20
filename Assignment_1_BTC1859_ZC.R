#' BTC1859 Data Science in Health I, Assignment 1 
#' Prof. Nicholas Mitsakakis, Summer Term, 2024
#' Zachery Chan, 1005468012

# Github repo for R: https://github.com/SirAcia/BTC1859.git

# Please note that questions 2-4 are answered on the word document/written report

#' -------------------------------------

#' Question 1:

#' Using the function pbinom() as this scenario deals with desecretized, 
#' numerical data. Also assuming that there are only 2 states for this sickness:
#' sick or not sick, thus fulfilling the requirements (& conditions) necessary 
#' to use a binomial distribution 

#' Let p be the national probability of falling sick where p = 0.2, 

# Finding the complement of the probability of 5 or less kids (=<5) get sick in a class of 15,
# given that the students are in the population of students affected by the national 
# of getting sick (i.e. lie in the the population where parameter p = 0.2)

# Finding complement of pbinom(5,size = 15,prob = 0.2), thus the probability of 
# at least 6 kids gettign sick 
prob_at_least_6 <- 1 - pbinom(5,size = 15,prob = 0.2)

#outputting probability to console window
prob_at_least_6

# prob_at_least_6 = 0.06105143

#' -------------------------------------

# Question 5

#Setting sequence for x-axis, using seq function to ensure smooth line (has many, many points)
x <- seq(from = -4, to = 8, length.out = 1000)

#PLotting normal distribution as y to plot on graph, using standard normal distribution (mean = 0, var = 1)
y_norm <- dnorm(x, mean = 0, sd = sqrt(1), log = FALSE)

#Plotting the normal distribution first as it well be tallest and set the zoom for y-axis accordingly
plot(x, y_norm, type = "l",col = "#000339", xlab = " ",
     ylab = " ", yaxt = "n", frame.plot = FALSE, lwd = 1.5, )

#Creating tick marks on the graph
axis(side = 1, at = c(-3, -1, 1, 3, 5, 7),labels = F)

#Creating vectors for the given degrees of freedom for the t-distributions
df_8 <- 8
df_4 <- 4
df_2 <- 2
df_1 <- 1

#Using points() to plot each t-distribution on the original graph, also set lwd = 1.5 for clarity
y_8 <- dt(x, df_8)
points(x, y_8, type = "l", col = "#0000CC", lwd = 1.5)

y_4 <- dt(x, df_4)
points(x, y_4, type = "l", col = "#0099CC", lwd = 1.5)

y_2 <- dt(x, df_2)
points(x, y_2, type = "l", col = "#00CCCC", lwd = 1.5)

y_1 <- dt(x, df_1)
points(x, y_1, type = "l", col = "#33FFFF", lwd = 1.5)

# Creating legend for the graph, used darker colours so it is more visisble 
# (found the one in the slides hard to read). Set the color of the normal 
# distribution darker for better disticntion & ease of viewing 
legend(x = 4.5, y = 0.39, legend = c("normal", "t, df = 8","t, df = 4", "t, df = 2", "t, df = 1"), 
       col = c("#000339", "#0000CC", "#0099CC", "#00CCCC", "#33FFFF"), lty = 1, border = "black", 
       text.col = c("#000339", "#0000CC", "#0099CC", "#00CCCC", "#33FFFF"), lwd = 2.5, cex = 1.15)

#Saved graph as file as placed into word report 



