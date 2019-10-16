# 3.8 Regression with Autocorrelated Errors

# Example 3.44 Mortality, Temperature and Pollution -----------------------

library(astsa)
trend <- time(cmort)
temp <- tempr - mean(tempr)
temp2 <- temp^2
fit <- lm(cmort ~ trend + temp + temp2 + part, na.action = NULL)
summary(fit)
acf2(resid(fit), 52) # PACF suggests A(2) for rediduals

sarima(cmort, 2,0,0, xreg = cbind(trend, temp, temp2, part))


# Example 3.45 Regression with Lagged Variables (cont) --------------------
# in Example 2.9 , residuals aer not white noise
dummy <- ifelse(soi<0, 0, 1)
fish <- ts.intersect(rec, soiL6=lag(soi,-6), dL6=lag(dummy,-6), dframe=TRUE)
summary(fit <- lm(rec ~soiL6*dL6, data=fish, na.action=NULL))
attach(fish)
plot(resid(fit))
acf2(resid(fit)) # indicates AR(2)

intract <- soiL6*dL6
sarima(rec, 2,0,0, xreg = cbind(soiL6, dL6, intract))
