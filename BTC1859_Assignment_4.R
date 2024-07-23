#' BTC1859 Data Science in Health I, Assignment 4
#' Prof. Nicholas Mitsakakis, Summer Term, 2024
#' Zachery Chan, 1005468012

# Github repo for R: https://github.com/SirAcia/BTC1859.git

# 

#' ---------------------------------------------------------------------------------------------

library(UsingR)
?babies

data <- babies 

data$ht[data$ht == 99] <- NA
anyNA(data$ht)

data$smoke[data$smoke == 9] <- NA
anyNA(data$smoke)

data$wt1[data$wt1 == 999] <- NA
anyNA(data$wt1)

summary(data$gestation)
data$gestation[data$gestation == 999] <- NA
anyNA(data$gestation)

data$race[data$race == 99] <- NA
anyNA(data$race)

data$race_fctr <- ifelse(data$race <= 5, "white",
                         ifelse(data$race == 6, "mex", 
                                ifelse(data$race == 7, "black", 
                                       ifelse(data$race == 8, "asian", "mixed"))))
data$race_fctr <- factor(data$race_fctr, levels = c("white", "black", "mex", "asian", "mixed"))

data$BMI <-  (data$wt1/2.2)/(data$ht*2.54/100)^2

data$premature <- ifelse(data$gestation < (37*7), 1, 0)

#Only factoring into 3 bins as summary showed only 0-3 post NAs
data$smoke_fctr <- factor(data$smoke, levels =c(0, 1, 2, 3), labels = c("Never", "Still", "During", "Within 1 yr"))

BMI_model <- glm(premature~BMI+smoke_fctr,data=data,family = binomial)
summary(BMI_model)

round(exp(BMI_model$coefficients),2)

round(exp(confint(BMI_model)),2)

BMI_model_2 <- glm(premature~BMI+smoke_fctr+race_fctr,data=data,family = binomial)
summary(BMI_model_2)

anova(BMI_model, BMI_model_2)







