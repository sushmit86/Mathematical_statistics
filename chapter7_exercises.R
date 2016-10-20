# Exercise 3
library(sqldf)
mu = 210 
sigma = 50
n = 100
# (a) conf ineterval
conf = c(210 - qnorm(0.95)* (sigma/sqrt(n)) , 210 + qnorm(0.95)* (sigma/sqrt(n)))
print(conf)
# 
n1 = (qnorm(1- 0.005) * 50/10)^2
print(n1)
# exercise 7
120 - qt(0.95,df = 99)*12/10
# 
# Exercise 9
spruce = read.csv("Data/Spruce.csv")
head(spruce)
Ht.change = spruce[,c("Ht.change")]
hist(Ht.change)
qqnorm(Ht.change,distribution = t)
t.test(Ht.change,conf.level = 0.95)$conf
#####
### Exercise 13
Ht.change_F = subset(spruce, select = Ht.change, Fertilizer == "F", drop = T )
Ht.change_NF = subset(spruce, select = Ht.change, Fertilizer == "NF", drop = T )
hist(Ht.change_F)
hist(Ht.change_NF)
t.test(Ht.change_F,Ht.change_NF, alt= "greater")$conf
### Exercise 11
Girls_2004 = read.csv("Data/Girls2004.csv")
head(Girls_2004)
girls_WY = subset(Girls_2004, select = Weight, State == "WY", drop = T )
girls_AK = subset(Girls_2004, select = Weight, State == "AK", drop = T )
t.test(girls_WY,girls_AK)$conf

# Exercise 15
data =  c(60.3 , 62.0, 65.0, 54.7, 65.6, 66.5, 60.7, 53.2, 68.7, 63.2, 72.9, 85.5)
#hist(data)
qqnorm(data, distribution = t)
qqline(data)
t.test(data, conf.level = 0.95)$conf
data <- data[-which(data==85.5)]
t.test(data, conf.level = 0.95)$conf
N = 10000
par(mfrow=c(1,1))
x_t_vec = numeric(N)
# Exercise 17
for (i in 1:N)
# plotting a gamma distribution
{
  n = 100
  shape = 5
  rate = 2
  x = rgamma(n , shape = shape, rate = 2)
  mu = shape/rate
  x_t_vec[i] =  sqrt(n)*(mean(x) - mu)/sd(x)

}
hist(x_t_vec,freq = FALSE)
t_x <- seq(-4, 4, length=N)
lines(t_x, dnorm(t_x), lwd=2, col='red')


par(mfrow=c(1,1))
x_t_vec = numeric(N)
# Exercise 17
for (i in 1:N)
  # plotting a gamma distribution
{
  n = 100
  lambda = 2
  x = rpois(n , lambda = lambda)
  mu = lambda
  x_t_vec[i] =  sqrt(n)*(mean(x) - mu)/sd(x)
  
}
hist(x_t_vec,freq = FALSE)
t_x <- seq(-4, 4, length=N)
lines(t_x, dnorm(t_x), lwd=2, col='red')
# Exercise 19
n = 500
mu = 5.29
sigma = 3.52
q = qt(0.875, df = n-1)
mu - q * sigma/(sqrt(n))
# exercise 23
prop.test(x=34, n= 350, conf.level = 0.95, correct = TRUE)$conf 
prop.test(x=56, n= 350, conf.level = 0.95, correct = TRUE)$conf 

# note the intervals overlap hence we cannot conclude
# Exercise 25

Bangladesh_data = read.csv("Data/Bangladesh.csv")
head(Bangladesh_data)
anyNA(Bangladesh_data[,c("Chlorine")])
chlorine = with(Bangladesh_data, Chlorine[!is.na(Chlorine)])
hist(chlorine)
qqnorm(chlorine)
qqline(chlorine)
xbar = mean(chlorine)
N = 10^4
n = length(chlorine)
chlorine_bootstrap = numeric(N)
chlorine_bootstrap_t = numeric(N)
for (i in 1:N)
{
x = sample(chlorine, size = n, replace = TRUE)
chlorine_bootstrap[i] = mean(x)
chlorine_bootstrap_t[i] =  (mean(x) - xbar)/(sd(x)/sqrt(n))
}
  
print(t.test(chlorine,conf.level = 0.95)$conf)

xbar - quantile(chlorine_bootstrap_t, c(0.975,0.025))*(sd(chlorine)/sqrt(n))

quantile(chlorine_bootstrap, c(0.025,0.975))
# Exercise 27
TXBirts_2004 = read.csv("Data/TXBirths2004.csv")
head(TXBirts_2004)
count_string <- "select Smoker, count(*) as count from
TXBirts_2004 group by 1"
sqldf(count_string,stringsAsFactors = FALSE)
Weight_Smoker = subset(TXBirts_2004,select = Weight, subset=Smoker == "No", drop = T)
Weight_NonSmoker = subset(TXBirts_2004,select = Weight,subset= Smoker == "Yes", drop = T)
hist(Weight_Smoker)
hist(Weight_NonSmoker)
qqnorm(Weight_Smoker)
qqline(Weight_Smoker)
qqnorm(Weight_NonSmoker)
qqline(Weight_NonSmoker)
print(length(Weight_NonSmoker))
print(length(Weight_Smoker))

N = 10^4
thetahat = mean(Weight_Smoker) - mean(Weight_NonSmoker)
nx = length(Weight_Smoker)
ny = length(Weight_NonSmoker)
boot = numeric(N)
boot_t = numeric(N)
SE = sqrt(var(Weight_Smoker)/nx + var(Weight_NonSmoker)/ny)
for (i in 1:N)
{
  bootx = sample(Weight_Smoker,size = nx ,replace = TRUE)
  booty = sample(Weight_NonSmoker, size = ny, replace = TRUE)
  boot[i] = mean(bootx) - mean(booty)
  boot_t[i] =  mean(bootx) - mean(booty) - thetahat/sqrt(var(bootx)/nx + var(booty)/ny)
}
t.test(Weight_Smoker,Weight_NonSmoker)$conf
quantile(boot,0.025,0.975)
