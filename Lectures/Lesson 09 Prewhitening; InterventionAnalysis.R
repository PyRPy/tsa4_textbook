# Lesson 9: Prewhitening; InterventionAnalysis

# 9.1 Pre-whitening as an Aid to Interpretingthe CCF ----------------------

# enerates random data fromARIMA(1,1,0).  This will generate a new data set 
# for each cal
x <- arima.sim(list(order=c(1,1,0), ar=0.7), n=200)

# reates a matrix z with columns, xt, xt-3,and xt-4
z <- ts.intersect(x, lag(x, -3), lag(x, -4))

# Creates y from lags 3 and 4 of randomly generated x
y <- 15 + 0.8*z[,2] + 1.5*z[,3]
plot(y, type="l")

# CF between x and y
ccf(z[,1], y, na.action = na.omit)
library(fpp2)
autoplot(z, facets = TRUE)

acf(x)
diff1x <- diff(x, 1)
acf(diff1x, na.action = na.omit)
pacf(diff1x, na.action = na.omit)
ar1model <- arima(x, order = c(1,1,0))
ar1model

pwx <- ar1model$residuals
newpwy <- filter(y, filter = c(1, -1.6783, 0.6783), sides = 1)
ccf(pwx, newpwy, na.action = na.omit)

