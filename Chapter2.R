library(descr)
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
