# Forecast-U.S.-Labour-Participation-Rates
#Project in R to be completed in December, 2017
# dataset = https://fred.stlouisfed.org/series/CIVPART#

library(fpp)

finalproj = read.csv("C:/schulich/Econ R/Final project/LbrForce.csv")
print(finalproj)
# tell R its a TS
lbrdata = ts(finalproj, start = 1948, frequency = 12)
# end data set in June 2017
lbrset = window(lbrdata, end = c(2017, 6))
# assign easy names to columns
lbrforce = lbrset[,2]
years = lbrset[,1]

# Assignment part 1 - plot TS 
title1 = "U.S. Labour Force Participation Rate"
plot(lbrforce, main = title1, xlab = "Years (Monthly)", ylab = "%", col = "purple")
# Assignment part 1 - analyze data + summary stats
hist(lbrforce, main = title1, col = "purple", xlab = "Participation Rate")
seasonplot(lbrforce, main = title1, col = "purple", year.labels = TRUE)
# recent = window(lbrset, start = c(2000,1), end = c(2017,12))
#seventyon = window(lbrset, start = c(1975,1), end = c(1999,12))
#old = window(lbrset, start = c(1948,1), end = c(1974,12))
#seasonplot(recent, main = title1, col = "purple", year.labels = TRUE)
#seasonplot(seventyon, main = title1, col = "purple", year.labels = TRUE)
#seasonplot(old, main = title1, col = "purple", year.labels = TRUE)
monthplot(lbrforce, main = title1, col = "purple", xlab = "Months", ylab = "%")
summary(lbrforce, main = title1, col = "purple")
lag.plot(lbrforce, lags = 12, col = "purple")
acf(lbrforce, main = title1, col = "purple")

# Assignment part 1 - difference analysis
MOMchange = diff(lbrforce)
MOMlog = diff(log(lbrforce))

title2 = "Change in Rate Month Over Month"
plot(MOMchange, main = title2, xlab = "Years (Monthly)", ylab = "%", col = "purple")
hist(MOMchange, main = title2, col = "purple", xlab = "Participation Rate")
seasonplot(MOMchange, main = title2, col = "purple", year.labels = TRUE, col = "purple")
monthplot(MOMchange, main = title2, col = "purple", xlab = "Months", ylab = "%")
summary(MOMchange, main = title2, col = "purple")
lag.plot(MOMchange, lags = 12, col = "purple")
acf(MOMchange, main = title2, col = "purple")

title3 = "Change in Log of Rate Month Over Month"
plot(MOMlog, main = title3, xlab = "Years (Monthly)", ylab = "%", col ="purple")
hist(MOMlog, main = title3, col = "purple", xlab = "Participation Rate")
seasonplot(MOMlog, main = title3, col = "purple", year.labels = TRUE)
monthplot(MOMlog, main = title3, col = "purple", xlab = "Months", ylab = "%")
summary(MOMlog, main = title3, col = "purple")
lag.plot(MOMlog, lags = 12, col = "purple")
acf(MOMlog, main = title3, col = "purple")

currentyear = window(lbrset, start= c(2013,6), end = c(2017,6))
plot(currentyear[,2])
