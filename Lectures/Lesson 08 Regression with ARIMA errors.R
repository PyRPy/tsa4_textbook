# Lesson 8: Regression with ARIMA errors, Cross correlation functions, and 
# Relationships between 2 Time Series

# 8.1 Linear Regression Models with Autoregressive Errors -----------------

# Example 1: Economic Measure 
# Suppose x is a leading economic indicator (predictor) for a country and 
# y = a measure of the state of the economy
library(astsa)
x <- ts(scan("l8.1.x.dat"))
y <- ts(scan("l8.1.y.dat"))
plot(x, y, pch=20, main="X vs. Y")

# first ordinary regression
trend <- time(y)
regmodel <- lm(y ~ trend + x)

# first ordinary regression with detrended x, dtx
dtx <- residuals(lm(x ~ time(x))) 
regmodel <- lm(y ~ trend + dtx)

# the regression results
summary(regmodel)

# ACF and PACF of the residuals
acf2(resid(regmodel))

# This is the adjustment regression with MA(1) residuals
adjreg <- sarima (y, 0,0,1, xreg=cbind(trend, dtx)) 

# Results of adjustment regression. White noise should be suggested
adjreg 
acf2(resid(adjreg$fit))

# Example 3: Glacial Varve 
# The response is a measure of the thickness of deposits of sand and silt
# (varve) left by spring melting of glaciers about 11,800 years ago.  
# The response is a measure of the thickness of deposits of sand and 
# silt (varve) left by spring melting of glaciers about 11,800 years ago

library(astsa)
varve <- scan("varve.dat")
varve <- ts(varve[1:455])
lvarve <- log(varve, 10)
trend <- time(lvarve) - mean(time(lvarve))
trend2 <- trend^2

# first ordinary regression.
regmodel <- lm(lvarve~trend + trend2)
summary(regmodel)

acf2(resid(regmodel))
# AR(1) for residuals
adjreg <- sarima(lvarve, 1,0,0, xreg = cbind(trend, trend2))
adjreg
# Note that the squared trend is not significant and may be dropped

adjreg$fit$coef 
# Note that R actually prints 0's for trend2 because the estimates are 
# so small. This command prints the actual values.


# 8.2 Cross Correlation Functions and Lagged Regressions ------------------

# Example: Southern Oscillation Index and Fish Populations in the southern 
# hemisphere
soi <- scan("soi.dat")
rec <- scan("recruit.dat")
soi <- ts(soi)
rec <- ts(rec)
ccf(soi, rec)

ccfvalues <- ccf(soi, rec)
ccfvalues

# Scatterplots
lag2.plot(soi, rec, 10)

# Regression Models 

alldata <- ts.intersect(rec,reclag1=lag(rec,-1), 
                        reclag2=lag(rec,-2), 
                        soilag5 = lag(soi,-5),
                        soilag6=lag(soi,-6), 
                        soilag7=lag(soi,-7), 
                        soilag8=lag(soi,-8), 
                        soilag9=lag(soi,-9),
                        soilag10=lag(soi,-10))

tryit <- lm(rec~soilag5+soilag6+soilag7+soilag8+soilag9+soilag10, 
            data = alldata)
summary (tryit)
acf2(residuals(tryit))

tryit2 <- lm(rec~reclag1+reclag2+soilag5+soilag6+soilag7+soilag8+
               soilag9+soilag10, data = alldata)
summary (tryit2)
acf2(residuals(tryit2))

tryit3 <- lm(rec~reclag1+reclag2+ soilag5+soilag6, data = alldata)
summary (tryit3)
acf2(residuals(tryit3))
