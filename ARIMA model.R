set.seed(121118); 
y=arima.sim(list(order=c(1,1,0),ar=0.7), n=300)
x<-seq(from = 1, to=301, by= 1)
plot.ts(x,y=y,plot.type = "single",type ="l", xlab="Time",ylab="Simulated 
        Values from an ARIMA(1,1,0) Time Series", main="Time series Plot",
         pch=1, col="red" )
Least_squares <- lm(y~x)
summary(Least_squares)
abline(Least_squares)
line(Least_squares$fitted.values)

#Part (iv)
plot(Least_squares$residuals)
acf(Least_squares$residuals)

acf(y,) 
acf(y, lag.max = NULL,type = "correlation",
    plot = TRUE)
acf(y, lag.max = NULL,type = "partial",
    plot = TRUE)
install.packages("forecast")
library(forecast)
tsdisplay(diff(y))

#Part (v)

fit10 <- arima(diff(y),order=c(1,0,0))
fit10$aic

fit10 <- arima(diff(y),order=c(1,0,1))
fit10$aic

fit10 <- arima(diff(y),order=c(1,1,1))
fit10$aic

fit10 <- arima(diff(y),order=c(1,0,2))
fit10$aic

fit10 <- arima(diff(y),order=c(1,2,0))
fit10$aic

fit10 <- arima(diff(y),order=c(1,2,2))
fit10$aic

tsdiag(fit10)

