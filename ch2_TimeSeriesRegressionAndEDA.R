### Chapter 2 Time Series Regression and Exploratory Data Analysis

library('astsa')
source('grid.r')

###############
## Example2.1 Estimating a Linear Trend 
## Fig. 2.1. The price of chicken: monthly whole bird spot price, Georgia docks, 
## US cents per pound,August 2001 to July 2016,with ???tted linear trend line.
  
par(mar=c(2,2.5,0,0)+.5, mgp=c(1.6,.3,0), tcl=-.2, las=1, cex.axis=.8)
head(chicken)
plot(chicken)
class(chicken) # ts class

summary(fit <- lm(chicken~time(chicken))) 
length(time(chicken))

plot(chicken, ylab="cents per pound",  type='n', ylim=c(58,122), yaxt='n')
grid(lty=1)
abline(fit) # draw the fitted line
lines(chicken, col=4, lwd=2)
axis(2, at = seq(60, 120, by = 10), las=2 )
dev.off()



################
## Example2.2 Pollution,TemperatureandMortality 
## Fig.2.2.Average weekly cardiovascular mortality(top),temperature(middle) and
## particulate pollution(bottom)in LosAngeles County
## There are 508 six-day smoothed averages obtained by ???ltering daily values over the 
## 10 year period 1970-1979.

par(mfrow = c(3,1), mar=c(2,2.5,1,0)+.5, mgp=c(1.6,.6,0))
plot(cmort, main="Cardiovascular Mortality", xlab="", ylab="",   type='n')
grid(lty=1)
lines(cmort)

plot(tempr, main="Temperature", xlab="", ylab="",   type='n')
grid(lty=1)
lines(tempr)

plot(part, main="Particulates", xlab="", ylab="",   type='n')
grid(lty=1)
lines(part)
dev.off()


###########
## Fig.2.3.Scatter plot matrix showing relations between mortality,temperature,
## and pollution. 
pairs(cbind(Mortality=cmort, Temperature=tempr, Particulates=part))
# plot(tempr)
# plot(cmort)
# plot(part)
dev.off()


##########
## Example2.4 Detrending Chicken Prices  
par(mfrow = c(2,1), mar=c(2,2,1,0)+.5, mgp=c(1.6,.6,0), cex.main=1.05)
fit <- lm(chicken~time(chicken), na.action=NULL) # regress chicken on time

plot(resid(fit), xlab="", main="detrended",   type='n')
grid(lty=1)
lines(resid(fit))
class(resid(fit)) # also a ts

plot(diff(chicken),  main="first difference",   type='n')
grid(lty=1)
lines(diff(chicken))
dev.off()


##########
## Fig.2.5.Sample ACFs of chicken prices(top),and of the detrended(middle) and
## the di???erenced (bottom) series.Compare the top plot with the sample ACF of a 
## straight line: acf(1:100).
  
par(mfrow = c(3,1), mar=c(2,2,.75,0)+.5, mgp=c(1.6,.6,0))
acf(chicken, 48, xlab="", main='', panel.first=grid(lty=1))
mtext("chicken", side=3, line=.1, cex=1, font=2)

acf(resid(fit), 48, xlab="", main='', panel.first=grid(lty=1))
mtext("detrended", side=3, line=.1, cex=1, font=2)

acf(diff(chicken), 48, xlab="", main='', panel.first=grid(lty=1))
mtext("first difference", side=3, line=.1, cex=1, font=2)
mtext("LAG", side=1, line=1.5, cex=.76)
dev.off()


##################
## page-61
## Fig.2.6.Di???erenced global temperature series and its sample ACF
par(mfrow = c(2,1), mar=c(1.5,2,1,1)+.5, mgp=c(1.6,.6,0))
plot(diff(globtemp), xlab="",  type='n' )
mtext("Year", side=1, line=1.5)
grid(lty=1)
lines(diff(globtemp))

mean(diff(globtemp))  # drift
acf(diff(globtemp), 25, xlab="", main='', panel.first=grid(lty=1))
mtext("LAG", side=1, line=1)
dev.off()


############
## Example2.7 Paleoclimatic Glacial Varves
par(mfrow = c(2,1), mar=c(2,1.5,.75,0)+.5, mgp=c(1.6,.6,0), cex.main=1.05)
plot(varve, main="varve", ylab="", xlab="",  type='n')
grid(lty=1)
lines(varve)

plot(log(varve), main="log(varve)", ylab="",   type='n')
grid(lty=1)
lines(log(varve))
dev.off()


##############
## Example2.8 Scatter plot Matrices,SOI and Recruitment 
lag1.plot(soi, 12)
dev.off()

## Fig. 2.9. Scatterplot matrix of the Recruitment series, Rt, on 
## the vertical axis plotted against the SOI series, St???h, 
lag2.plot(soi, rec, 8) # for two ts plot
dev.off()


#####################
## Example2.9 Regression with Lagged Variables(cont)  
par(mar=c(3,3,1,1), mgp=c(1.6,.6,0))
dummy <- ifelse(soi<0, 0, 1)
fish <- ts.intersect(rec, soiL6=lag(soi,-6), dL6=lag(dummy,-6), dframe=TRUE)
fit <- lm(rec~ soiL6*dL6, data=fish, na.action=NULL)
summary(fit)

attach(fish)
plot(soiL6, rec, panel.first=grid(lty=1), col=gray(.4))

lines(lowess(soiL6, rec), col=4, lwd=2)
points(soiL6, fitted(fit), pch='+', col=2)
detach(fish) # be sure to detach it
dev.off()


###################
## Example2.10 Using Regression to Discover a Signal in Noise
## need to fix the problem, cannot reproduce
par(mfrow=c(2,1), mar=c(1,2.2,0,0)+.5, mgp=c(1.5,.6,0))
set.seed(90210)  # so you can reproduce these results
x <- 2*cos(2*pi*1:500/50 + .6*pi) + rnorm(500,0,5)
z1 <- cos(2*pi*1:500/50)
z2 <- sin(2*pi*1:500/50)
fit <- lm(x~ 0+z1+z2) # zero to exclude the intercept, 
summary(fit)
plot.ts(x, xlab='', type='n')
grid()
lines(x)

plot.ts(x, ylab=expression(hat(x)), type='n') 
lines(x, col=gray(.5))
grid()
lines(fitted(fit), col=2, lwd=2)
dev.off()


######################
## Fig.2.12.MovingaveragesmootherofSOI.
## Theinsertshowstheshapeofthemovingaverage ("boxcar")kernel
par(mar=c(2.5,2.5,.5,.5), mgp=c(1.6,.6,0))
w1 <- c(.5, rep(1,11), .5)/12
soif1 <- filter(soi, sides=2, filter=w1)

plot(soi, ylim=c(-1,1.15),  type='n')
grid(lty=1)
lines(soi, col=gray(.5))
lines(soif1, lwd=2, col=4)

par(fig = c(.65, 1, .65, 1),   new = TRUE)
nwgts <- c(rep(0,20),w1,rep(0,20))
plot(nwgts, type="l", ylim = c(-.02,.1), xaxt='n', yaxt='n', ann=FALSE)
dev.off()



###############################################
## Example2.12 Kernel Smoothing  
par(mar=c(2.5,2.5,.5,.5), mgp=c(1.6,.6,0))
plot(soi, ylim=c(-1,1.15),   type='n')
grid(lty=1)
lines(soi, col=gray(.5))

lines(ksmooth(time(soi), soi, "normal", bandwidth=1), lwd=2, col=4)

par(fig = c(.65, 1, .65, 1), new = TRUE)
gauss <- function(x) 1/sqrt(2*pi) * exp(-(x^2)/2)
x <- seq(from = -3, to = 3, by = 0.001)

plot(x, gauss(x), type = "l", ylim = c(-.02,.45), lty = 1,  xaxt='n', yaxt='n', ann=FALSE)
dev.off()
 
 
############################# 
## Example2.13 Lowess 
par(mar=c(2.5,2.5,.5,.5), mgp=c(1.6,.6,0))
plot(soi, type='n')
grid(lty=1)
lines(soi, col=gray(.5))

lines(lowess(soi, f=.05), lwd=2, col=4)  # El Nino cycle
lines(lowess(soi), lty=5, lwd=2, col=2)  # trend (with default span)
dev.off()

 
###############################################
## Example2.14 Smoothing Splines 
par(mar=c(2.5,2.5,.5,.5), mgp=c(1.6,.6,0))
plot(soi, type='n')
grid(lty=1)
lines(soi, col=gray(.5))

lines(smooth.spline(time(soi), soi, spar=.5), lwd=2, col=4)          
lines(smooth.spline(time(soi), soi, spar=1), lty=5, lwd=2, col=2)  
dev.off()

 
#############################################
## Example2.15 Smoothing One Series as a Function of Another 
par(mar=c(2.5,2.5,.5,.5), mgp=c(1.6,.6,0))
plot(tempr, cmort, main="", xlab="Temperature", ylab="Mortality",
     col=gray(.5),  panel.first=grid(lty=1))
lines(lowess(tempr,cmort), col=4, lwd=2)
dev.off()
 

