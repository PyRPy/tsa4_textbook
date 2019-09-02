# 2.1 Moving average models

# 2.1 Moving average models -----------------------------------------------
# exmaple 2.1
# theta1 = 0.7
acfma1 <- ARMAacf(ma=c(0.7), lag.max = 10) 
lags <- 0:10
plot(lags, acfma1, xlim = c(1, 10), ylab = "r", type = "h", 
     main = "ACF for MA(1) with theta1 = 0.7")
abline(h = 0)

# example  simuulated MA(1)
xc <- arima.sim(n=150, list(ma=c(0.7)))
x <-  xc + 10
plot(x, type="b", main = "simulated MA(1) data")
acf(x, xlim=c(1, 10), main="ACF for simulated sample data")

# example 2.2
acfma2 <- ARMAacf(ma=c(0.5,0.3), lag.max=10)
acfma2
lags <- 0:10
plot(lags,acfma2,xlim=c(1,10), ylab="r",type="h", 
     main = "ACF for MA(2) with theta1 = 0.5,theta2=0.3")
abline(h=0)

xc <- arima.sim(n=150, list(ma=c(0.5, 0.3)))
x <- xc+10
plot(x, type="b", main = "Simulated MA(2) Series")
acf(x, xlim=c(1,10), main="ACF for simulated MA(2) Data")


# 2.2 Partial Autocorrelation Function (PACF) -----------------------------

ma1pacf <- ARMAacf(ma=c(0.7), lag.max = 36, pacf = TRUE)
plot(ma1pacf, type = "h",
     main = "Theoretical PACF of MA(1) with theta = 0.7")
abline(h = 0)
