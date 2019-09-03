### Chapter 2 Time Series Regression and Exploratory Data Analysis

library('astsa')
# source('grid.r')


# 2.1 Classical Regression in the Time Series Context ---------------------

###############
## Example2.1 Estimating a Linear Trend 
## Fig. 2.1. The price of chicken: monthly whole bird spot price, Georgia docks, 
## US cents per pound,August 2001 to July 2016,with fitted linear trend line.
  
head(chicken)
plot(chicken)
class(chicken) # ts class

summary(fit <- lm(chicken~time(chicken))) 
length(time(chicken))

plot(chicken, ylab="cents per pound", ylim=c(58,122), col="blue")
abline(fit) # draw the fitted line


## Example2.2 Pollution,TemperatureandMortality 
## Fig.2.2.Average weekly cardiovascular mortality(top),temperature(middle) and
## particulate pollution(bottom)in LosAngeles County
## There are 508 six-day smoothed averages obtained by filtering daily values over the 
## 10 year period 1970-1979.

par(mfrow = c(3,1))
plot(cmort, main="Cardiovascular Mortality", xlab="", ylab="")
plot(tempr, main="Temperature", xlab="", ylab="")
plot(part, main="Particulates", xlab="", ylab="")

dev.new()
ts.plot(cmort, tempr, part, col=1:3)
## Fig.2.3.Scatter plot matrix showing relations between mortality,temperature,
## and pollution. 
pairs(cbind(Mortality=cmort, Temperature=tempr, Particulates=part))
temp <- tempr - mean(tempr)
temp2 <- temp^2
trend <- time(cmort)
fit <- lm(cmort ~ trend + temp + temp2 + part, na.action = NULL)
summary(fit)

summary(aov(fit))
summary(aov(lm(cmort ~ cbind(trend, temp, temp2, part))))

num <- length(cmort)
AIC(fit)/num - log(2*pi)
BIC(fit)/num - log(2*pi)
(AICc <- log(sum(resid(fit)^2)/num) + (num + 5)/(num - 5 - 2))

## Example 2.3 Regression With Lagged Variables
fish <- ts.intersect(rec, soiL6=lag(soi, -6), dframe = TRUE)
summary(fit1 <- lm(rec ~ soiL6, data = fish, na.action = NULL))

# align the lagged series
library(dynlm)
summary(fit2 <- dynlm(rec ~ L(soi, 6)))


# 2.2 Exploratory Data Analysis -------------------------------------------

## Example2.4 Detrending Chicken Prices  
fit <- lm(chicken~time(chicken), na.action=NULL) # regress chicken on time
dev.off()
plot(resid(fit), xlab="", main="detrended")
plot(diff(chicken),  main="first difference")
dev.off()

## Fig.2.5.Sample ACFs of chicken prices(top),and of the detrended(middle) and
## the differenced (bottom) series.Compare the top plot with the sample ACF of a 
## straight line: acf(1:100).


par(mfrow = c(3,1))
acf(chicken, 48, xlab="", main='', panel.first=grid(lty=1))
mtext("chicken", side=3, line=.1, cex=1, font=2)

acf(resid(fit), 48, xlab="", main='', panel.first=grid(lty=1))
mtext("detrended", side=3, line=.1, cex=1, font=2)

acf(diff(chicken), 48, xlab="", main='', panel.first=grid(lty=1))
mtext("first difference", side=3, line=.1, cex=1, font=2)
mtext("LAG", side=1, line=1.5, cex=.76)
dev.off()

# Example 2.6 Differencing Global Temperature
## page-61
## Fig.2.6.Differenced global temperature series and its sample ACF
par(mfrow = c(2,1))
plot(diff(globtemp), type = "o")

mean(diff(globtemp))  # drift
acf(diff(globtemp), 48)
dev.off()


## Example2.7 Paleoclimatic Glacial Varves
par(mfrow = c(2,1))
plot(varve, main="varve", ylab="")
plot(log(varve), main="log(varve)", ylab="")
dev.off()


## Example2.8 Scatter plot Matrices,SOI and Recruitment 
lag1.plot(soi, 12)
dev.off()

## Fig. 2.9. Scatterplot matrix of the Recruitment series, Rt, on 
## the vertical axis plotted against the SOI series, St???h, 
lag2.plot(soi, rec, 8) # for two ts plot
dev.off()

## Example2.9 Regression with Lagged Variables(cont)  
dummy <- ifelse(soi<0, 0, 1)
fish <- ts.intersect(rec, soiL6=lag(soi,-6), dL6=lag(dummy,-6), dframe=TRUE)
fit <- lm(rec~ soiL6*dL6, data=fish, na.action=NULL)
summary(fit)

attach(fish)
plot(soiL6, rec)

lines(lowess(soiL6, rec), col=4, lwd=2)
points(soiL6, fitted(fit), pch='+', col=2)
detach(fish) # be sure to detach it
dev.off()

## Example2.10 Using Regression to Discover a Signal in Noise
## need to fix the problem, cannot reproduce
par(mfrow=c(2,1))
set.seed(90210)  # so you can reproduce these results
x <- 2*cos(2*pi*1:500/50 + .6*pi) + rnorm(500,0,5)
z1 <- cos(2*pi*1:500/50)
z2 <- sin(2*pi*1:500/50)
fit <- lm(x~ 0+z1+z2) # zero to exclude the intercept, 
summary(fit)
plot.ts(x)

plot.ts(x, ylab=expression(hat(x))) 
lines(fitted(fit), col=2)
dev.off()


# 2.3 Smoothing in the Time Series Context --------------------------------

## Fig.2.12.Moving average smoother of SOI.
## The insert shows the shape of the moving average ("boxcar") kernel
w1 <- c(.5, rep(1,11), .5)/12
soif1 <- filter(soi, sides=2, filter=w1)

plot(soi)
lines(soif1, lwd=2, col=4)

nwgts <- c(rep(0,20),w1,rep(0,20))
plot(nwgts, type="l", ylim = c(-.02,.1), xaxt='n', yaxt='n', ann=FALSE)
dev.off()

## Example2.12 Kernel Smoothing  
plot(soi, ylim=c(-1,1.15))
lines(ksmooth(time(soi), soi, "normal", bandwidth=1), lwd=2, col=4)

## Example2.13 Lowess 

plot(soi)
lines(lowess(soi, f=.05), lwd=2, col=4)  # El Nino cycle
lines(lowess(soi), lty=5, lwd=2, col=2)  # trend (with default span)
dev.off()

# Example2.14 Smoothing Splines 
plot(soi)
# spar = 0.5 to emphasize the El Nino cycle
# spar = 1 to emphasize the trend
lines(smooth.spline(time(soi), soi, spar=.5), lwd=2, col=4)          
lines(smooth.spline(time(soi), soi, spar=1), lty=5, lwd=2, col=2)  
dev.off()


# Example2.15 Smoothing One Series as a Function of Another 
plot(tempr, cmort, main="", xlab="Temperature", ylab="Mortality")
lines(lowess(tempr,cmort), col=4, lwd=2)
dev.off()
 

