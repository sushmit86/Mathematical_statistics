Flights_delay <- read.csv("/Users/sushmitroy/Mathematical_statistics/Data/FlightDelays.csv")
names(Flights_delay)
head(Flights_delay)
tail(Flights_delay)
dim(Flights_delay)
table(Flights_delay$Carrier)
barplot(table(Flights_delay$Carrier))
table(Flights_delay$Carrier, Flights_delay$Delayed30)
hist(Flights_delay$Delay)
delay <- Flights_delay$Delay
mean(delay)
median(delay)
mean(delay , trim = 0.25)
max(delay)
min(delay)
range(delay)
var(delay)
sd(delay)
quantile(delay)
n <- length(delay)
(n-1)/n * var(delay)
tapply(delay, Flights_delay$Carrier, mean)
tapply(delay, Flights_delay$DepartTime, median)
summary(delay)
boxplot(delay)
tapply(delay, Flights_delay$Day, summary)
boxplot(Delay ~ Day, data = Flights_delay)
tapply(delay, Flights_delay$DepartTime, summary)
boxplot(Delay ~ DepartTime, data = Flights_delay)
1:10
5:-3
length(seq(0,3, by = 0.2))
seq(0,3,length = 15)
delay
quantile(delay, seq(0, 1, by = .1)) #deciles of delay
z <- c(8, 3, 0, 9, 9, 2, 1, 3)
length(z)
z[4]
z[1]
z[-c(1, 3, 4)]
z[11]
which(z < 4)
index <- which(z < 4)
z[index]
delay
delay <- subset(Flights_delay, select = Delay, drop = TRUE)
delay
subset(Flights_delay, select = Delay)
delay2 <- subset(Flights_delay, select = Delay, subset = Day != "Mon",
                 drop = TRUE)
mean(delay2)







