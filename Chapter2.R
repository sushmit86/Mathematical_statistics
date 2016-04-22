library(descr)
library(mosaic)
#Examples
Flightsdelay <- read.csv("Data/FlightDelays.csv")
head(Flightsdelay)
table(Flightsdelay$Carrier)
barplot(table(Flightsdelay$Carrier))
addmargins(table(Flightsdelay$Carrier,Flightsdelay$Delayed30))
hist(Flightsdelay$Delay, breaks = seq(-50,800,25),right = FALSE)
delayUA= subset(Flightsdelay, select = Delay, subset = Carrier == "UA" , drop = TRUE)

br = seq(-50,500,by=50)
freq_data <- hist(delayUA , breaks = br,include.lowest=TRUE, plot = FALSE)
ranges = paste(head(br,-1), br[-1], sep=" - ")
data.frame(range = ranges, frequency = freq_data$counts)
hist(delayUA , breaks = br,include.lowest=TRUE)
summary(delayUA)
mean(delayUA , trim = 0.25)

sqrt((length(delayUA)-1)/length(delayUA))*sd(delayUA) # population standard deviation

delayboxplot <- subset(Flightsdelay ,Carrier == "UA" , select = c(Day,Delay))

boxplot(Delay~Day,data=delayboxplot, varwidth = TRUE)
qnorm(0.25)
qnorm(0.6,3,5)

NCBirths <- read.csv("Data/NCBirths2004.csv")
x <- c(21.7, 26.1, 28.3, 30, 31.2, 31.5, 33.5, 34.7, 36)
qqnorm(x)
qqline(x)
x <- c(3,6,15,15,17,19,24)
plot.ecdf(x)
x <- rnorm(25)
plot.ecdf(x, xlim = c(-4,4))
curve(pnorm(x), col = "blue", add = T)
# Beer data
Beerwings <- read.csv("Data/Beerwings.csv")
beerM <- subset(Beerwings, select = Beer, subset = Gender == "M", drop = T)
beerF <- subset(Beerwings, select = Beer, subset = Gender == "F", drop = T)
plot.ecdf(beerM, xlab = "ounces")
plot.ecdf(beerF, col = "blue" , pch = 2, add = T)
abline(v = 25, lty = 2)
legend("topleft", legend = c("Males", "Females"), col = c("black", "blue"), pch = c(19,2))
plot(Beerwings$Hotwings, Beerwings$Beer, xlab = "Hot wings eaten", ylab = "Beer consumed")

############ exercises #################
### exercise 1
data1 <- c(3,5,8,15,20,21)
mean(data1)
mean(log(data1))
median(data1)
median(log(data1))

#### exercise 4
Flightsdelay <- read.csv("Data/FlightDelays.csv")
table(Flightsdelay$DepartTime)
barplot(table(Flightsdelay$DepartTime))
addmargins(table(Flightsdelay$Day,Flightsdelay$Delayed30))
boxplot(Flightsdelay$FlightLength ~ Flightsdelay$Delayed30 ,data = Flightsdelay , ylab = "Duration of Flight"
        , xlab = "Delayed 30 Mins or not")
### May be there is no relationship 
tbl <- table(Flightsdelay$Day,Flightsdelay$Delayed30)

#(tbl <- cbind(tbl, rowSums(tbl), rowSums(tbl=="No") / sum(tbl)))
rm(tbl)
prop.table(tbl)



