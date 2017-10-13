library(fpp)
library(knitr)

finalproject = read.csv("C:/schulich/Econ R/Final project/LbrForce.csv")
head(finalproject)
lbrts = ts(finalproject, start = 1948, end = c(2017,8), frequency = 12)
trainlbr = window(lbrts[,2], start = c(1948, 1), end = c(2003, 12))
traindt = window(lbrts[,1], start = c(1948, 1), end = c(2003, 12))
testlbr = window(lbrts[,2], start = c(2004, 1), end =c(2017,8))
testldt = window(lbrts[,1], start = c(2004, 1), end =c(2017,8))

#train data 
h = length(testlbr)

fitmean = meanf(trainlbr, h=h)
fitnaive = naive(trainlbr, h=h)
fitseason = snaive(trainlbr, h=h)
fitlm = tslm(trainlbr ~ trend)
fcstlm = forecast(fitlm, h=h)


#test data

accm = accuracy(fitmean, testlbr)
accn = accuracy(fitnaive, testlbr)
accs = accuracy(fitseason, testlbr)
acclm = accuracy(fcstlm, testlbr)


#put accuracy into a table

acctable = rbind(accm, accn, accs, acclm)
row.names(acctable) = c('Mean training','Mean test', 'Naive training', 'Naive test', 
                        'S. Naive training', 'S. Naive test', 'Linear training', 'Linear test')
print(acctable)



#make a plot
plot(fitmean, PI= FALSE #removes confidence interval#
     , main = "U.S. Labour Force Participation Rate Forecasts", xlab = 'Year', ylab = "%")
lines(fitnaive$mean, col = 'Green')
lines(fitseason$mean, col = 'Pink')
lines(fcstlm$mean, col = "Red")
lines(testlbr)
legend('topleft', lty =1, col =c("Blue",'Green','Pink', 'Red'), 
       legend = c("Mean", "Naive", "Seasonal Naive", "Linear"))

# forecast 6 periods into the future
future = naive(lbrts[,2], h=6)
print(future)

#make a plot
plot(future, PI= FALSE #removes confidence interval#
     , main = "U.S. Labour Force Participation Rate Forecasts")
lines(window(lbrts[,2], start = c(2000,1)))
legend('topleft', lty =1, col ="Blue", 
       legend =  "Naive")
