library(sqldf)
# exercise 1
mean_calc = 9.5
# unknown standard deviation
# H0: mean = mean_find
# HA: mean_calc != mean_find

n = 20
S = 1.1
x_mean = 9.2
t_stat = (x_mean - mean_calc)/(S/sqrt(n))
2*pt(t_stat, df = n-1)
# Exercise 3
mean_temp = 98.6
sample_temp <- c(98.0, 98.9, 99.0, 98.9, 98.8, 98.6, 99.1, 98.9, 98.5, 98.9, 98.9, 98.4,99.0, 99.2,
                 98.6,98.8,98.9,98.7)
n= length(sample_temp)
# HO : mean_temp = mean_sample
# HA: mean_sample > mean_temp
t_stat = (mean(sample_temp) - mean_temp)/(sd(sample_temp)/sqrt(n))
1- pt(t_stat,df = n-1 )
# p value = 0.007 hence it is significant at 1% hence we reject the Null H0 
# Exercise 5 Welch's approoximation
location_A <- c(8.53, 8.52, 8.01, 7.99, 7.93, 7.89, 7.85, 7.82, 7.80)
location_B <- c(7.85, 7.73, 7.58, 7.40, 7.35, 7.30, 7.27, 7.27, 7.23)
t.test(location_A,location_B , alternative = "two.sided")
# P value = 0.0002
Alelager <- read.csv("Data/Alelager.csv")
head(Alelager)
Ale <- subset(Alelager, select = Calories,subset = Type == "Ale", drop = TRUE )
Lager <-  subset(Alelager, select = Calories,subset = Type == "Lager", drop = TRUE )
t.test(Ale,Lager,alternative = "greater") 
# Exercise 9
prop.test(c(100,39.1),c(130,62.4), correct = FALSE)
# Exercise 11
prop.test(c(505,240),c(773,192),correct = FALSE)
p_hat = (192+ 240)/(505+ 773)
SE = sqrt(p_hat*(1- p_hat)*(1/505 + 1/773))
Z = ( 0.38 - 0.31)/SE
2*(1 - pnorm(Z))
# Exercise 13

