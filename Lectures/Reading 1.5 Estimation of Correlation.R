# Reading 1.5 Estimation of Correlation

# Example 1.28 SOI and Recruitment Correlation Analysis -------------------
library(astsa)
par(mfrow=c(3,1))
acf(soi, 48, main="Southern Oscillation Index")
acf(rec, 48, main="Recruitment")
ccf(soi, rec, 48, main="SOI vs Recruitment", ylab="CCF")


# Example 1.29 Prewhitening and Cross Correlation Analysis ----------------
set.seed(1492)
num=120
t=1:num
X = ts(2*cos(2*pi*t/12) + rnorm(num), freq=12)
Y = ts(2*cos(2*pi*(t+5)/12) + rnorm(num), freq=12)
Yw = resid( lm(Y~ cos(2*pi*t/12) + sin(2*pi*t/12), na.action=NULL) )

par(mfrow=c(3,2))
plot(X)
plot(Y)
acf(X,48, ylab='ACF(X)')
acf(Y,48, ylab='ACF(Y)')
ccf(X,Y,24, ylab='CCF(X,Y)')
ccf(X,Yw,24, ylab='CCF(X,Yw)', ylim=c(-.6,.6))


# Example 2.8 Scatterplot Matrices, SOI and Recruitment -------------------
lag1.plot(soi, 12)
lag2.plot(soi, rec, 8)


# Example 2.9 Regression with Lagged Variables ----------------------------
dummy <- ifelse(soi < 0, 0, 1)
fish <- ts.intersect(rec, soiL6=lag(soi, -6), dL6=lag(dummy, -6), 
                     dframe = TRUE)
summary(fit <- lm(rec ~ soiL6*dL6, data = fish, na.action = NULL))
attach(fish)
par(mfrow = c(1,1))
plot(soiL6, rec)
lines(lowess(soiL6, rec), col=4, lwd=2)
points(soiL6, fitted(fit), pch="+", col=2)
plot(resid(fit))
acf(resid(fit)) # not white noise


# Example 3.8 Parameter Redundancy, Causality, Invertibility --------------

ARMAtoMA(ar = 0.9, ma = 0.5, 10) # first 10 psi weights
ARMAtoMA(ar = -.05, ma = -0.9, 10)
