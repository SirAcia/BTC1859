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

prob_at_least_6 <- pbinom(5,size = 15,prob = 0.2)
