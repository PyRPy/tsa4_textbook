# Chapter 1 Characteristics of Time Series
library('astsa')

# 1.1 The Nature of Time Series Data ------------------------------------------
# Example 1.1 Johnson & Johnson Quarterly Earnings
plot(jj, ylab="Quarterly Earnings per Share",   type='n')
lines(jj, type="o")

############ this is in the EZ version only #########
t=1:(10*2)
x = 100*1.1^t
y = 150*1.1^t
z = 200*1.1^t
w = 75*1.1^t 
culer = c(1,2,'darkgreen',4)
u=t(cbind(x,y,z,w))
x=ts(c(u), freq=4)
plot(x, xlab='quarter', ylab='value', col=gray(.6))
points(x,  pch=c('1','2','3','4'), col=culer, cex=.8)
plot(log(x), xlab='quarter', ylab='log(value)', col=gray(.6))
points(log(x),  pch=c('1','2','3','4'), col=culer, cex=.8)


# Example 1.2 Global Warming
plot(globtemp, ylab="Global Temperature Deviations", type='n')   
lines(globtemp, type='o')

# Speech data
plot(speech,   type='n', ylim=c(0,4100))
lines(speech)

# Dow Jones Industrial Average
library(xts)
library(quantmod)
djia <-  getSymbols(Symbols = "DJIA", src = "yahoo", auto.assign = FALSE) 
head(djia)

djiar <-  diff(log(djia$DJIA.Close))[-1]         

plot(djiar, ylab="DJIA Returns", main='', type='n')
lines(djiar)

# TTR::getYahooData is deprecated and will be removed in a future release.
# Please use quantmod::getSymbols instead.

# El Nino and Fish Population
plot(soi, ylab="", xlab="", main="Southern Oscillation Index",   type='n')
lines(soi)

plot(rec, ylab="", main="Recruitment",  type='n')
lines(rec)

# fMRI imaging
ts.plot(fmri1[,2:5], ylab="BOLD", xlab="", main="Cortex", type='n')
ts.plot(fmri1[,2:5], col=1:4, ylab="BOLD", xlab="", main="Cortex")

ts.plot(fmri1[,6:9], ylab="BOLD", xlab="", main="Thalamus & Cerebellum", type='n')
ts.plot(fmri1[,6:9], col=1:4, ylab="BOLD", xlab="", main="Thalamus & Cerebellum")
mtext("Time (1 pt = 2 sec)", side=1, line=1.5)
dev.off()


# Earthquakes and explosions
plot(EQ5, main="Earthquake", xlab="", type='n')
lines(EQ5)

plot(EXP6, main="Explosion", xlab="", type='n')  
lines(EXP6)
mtext("Time", side=1, line=1.5)

set.seed(1)
w = rnorm(500,0,1)                    # 500 N(0,1) variates
v = filter(w, sides=2, filter=rep(1/3,3))  # moving average
plot.ts(w, main="white noise", type='n')
grid(lty=1, col=gray(.9)); lines(w)
plot.ts(v, ylim=c(-3,3), main="moving average", type='n')
grid(lty=1, col=gray(.9)); lines(v)
dev.off()


# 1.5 Estimate of correlations --------------------------------------------
# exmaple 1.25 sample ACF and scatterplot
(r <- round(acf(soi, 6, plot=FALSE)$acf[-1], 3))
par(mfrow=c(1,2))
plot(lag(soi, -1), soi)
legend('topleft', legend = r[1])
plot(lag(soi, -6), soi)
legend('topleft', legend = r[6])

# example 1.26 a simulated time series - sequence of coin tosses
set.seed(101010)
x1 <- 2*rbinom(11, 1, 0.5) -1
x2 <- 2*rbinom(101, 1, 0.5) -1
y1 <- 5 + filter(x1, sides = 1, filter = c(1, -0.7))[-1]
y2 <- 5 + filter(x2, sides = 1, filter = c(1, -0.7))[-1]

# plot
plot.ts(y1, type='s')
plot.ts(y2, type='s')

# sample means
c(mean(y1), mean(y2))

acf(y1, lag.max = 4, plot = FALSE)
acf(y2, lag.max = 4, plot = FALSE)
dev.off()
# example 1.27 ACF of a speech signal
acf(speech, 250)

# example 1.28 SOI and recruitment correlation analysis
par(mfrow=c(3, 1))
acf(soi, 48, main="Southern ocsillation index")
acf(rec, 48, main="Recruitment")
ccf(soi, rec, main="SOI vs Recruitment", ylab="CCF" )

# example 1.29 prewhitening and cross correlation analysis
set.seed(1492)
num <- 120
t <- 1:num
X <- ts(2*cos(2*pi*t/12) + rnorm(num), frequency = 12)
Y <- ts(2*cos(2*pi*(t + 5)/12) + rnorm(num), frequency = 12)
Yw <- resid(lm(Y ~ cos(2*pi*t/12) + sin(2*pi*t/12), na.action = NULL))
par(mfrow=c(3, 2))
plot(X)
plot(Y)
acf(X, 48, ylab='ACF(X)')
acf(Y, 48, ylab='ACF(Y)')
ccf(X, Y, 24, ylab="CCF(X,Y)")
ccf(X, Yw, 24, ylab='CCF(X, Yw)', ylim=c(-0.6, 0.6))
