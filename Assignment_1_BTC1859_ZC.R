#' BTC1859 Data Science I, Assigbnment 1 
#' Zacehry Chan 

#' -------------------------------------

#' Question 1: Suppose 6 out of 15 students in a class develop influenza, 
#' whereas there is 20% chance for a student in similar school classes 
#' nationwide to develop influenza. Is there evidence of an excessive number 
#' of cases in the class? That is, what is the probability of obtaining at 
#' least 6 cases in this class if the nationwide rate holds true?


#' Using the fucntion pbinopm() as this scenario deals with desecretized, 
#' numerical data. Also assuming taht there are only 2 states for this sickness,
#' sick or not sick, thus fulfilling the requiremnets (&conditions) necessary 
#' to use a binomial distribution 
#' 
#' Given p = 0.2, _________

prob_at_least_6 <- 1 - pbinom(5,size = 15,prob = 0.2)

#outputting probability to console window
prob_at_least_6

# prob_at_least_6 = 0.06105143

#' Calculates the porbability fo there being at least 5 (x <= 5) 
#' takes the complement to see the probablity of there being at least 6 cases 
#' of sickeness in a given class, given the parameters 

#' -------------------------------------

# Question 5

#Setting sequence for x-axis, using seq func to ensure smooth line (has many, many points)
x <- seq(from=-4,to=8,length.out = 1000)

#PLotting normal distribution as y to plot on graph, using standard normal distribution (mean = 0, var = 1)
y_norm <- dnorm(x, mean = 0, sd = sqrt(1), log = FALSE)

#Plotting the normal distribution first as it well be tallest and set the zoom for y-axis accordingly
plot(x,y_norm, type = "l", main="t-distribution, degrees of freedom",col = "blue", xlab = " ",ylab = " ")

#Creating vectors for the given degrees of freedom for the t-distributions
df_8 <- 8
df_4 <- 4
df_2 <- 2
df_1 <- 1

#Using points() to plot each t-distribution on the original graph 
y_8 <- dt(x, df_8)
points(x,y_8, type="l", col="red")

y_4 <- dt(x, df_4)
points(x,y_4, type="l", col="green")

y_2 <- dt(x, df_2)
points(x,y_2, type="l", col="black")

y_1 <- dt(x, df_1)
points(x,y_1, type="l", col="orange")

#Creating legend for the graph
legend("topright", legend = c("normal", "t, df = 8","t, df = 6", "t, df = 4", "t, df = 2"), lty = 1, border = "black", title = "Degrees of Freedom")

