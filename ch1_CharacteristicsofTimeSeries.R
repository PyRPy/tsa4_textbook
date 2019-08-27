## Chapter 1 Characteristics of TimeSeries
## simplited for template purposes

library('astsa')
source('grid.r')  # I changed the defaults so that
#  grid() gives grid(lty=1, col = gray(.9))

#####################################################
## Fig.1.1.Johnson&Johnson quarterly earnings per share,84quarters,1960-Ito1980-IV.
## page 2
par(mar=c(3,3,1,1), mgp=c(1.6,.6,0))
plot(jj, ylab="Quarterly Earnings per Share",   type='n')
grid(lty=1)
lines(jj, type="o")

############ this is in the EZ version only #########
par(mfrow=1:2, mar=c(2.5,2.5,.5,.5)+.1, mgp=c(1.5,.6,0))
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

###########
par(mar=c(3,3,1,1), mgp=c(1.6,.6,0))
plot(globtemp, ylab="Global Temperature Deviations",    type='n')   
grid(lty=1)
lines(globtemp, type='o')
dev.off()

###########
par(mar=c(3,3,1,1), mgp=c(1.6,.6,0))
plot(speech,   type='n', ylim=c(0,4100))
grid(lty=1)
lines(speech)
dev.off()

###########
## Fig.1.4.The daily returns of the DowJones Industrial Average(DJIA) from 
## April 20,2006 to April 20,2016.

library(TTR)
library(Quandl)
library(quantmod)
djia <-  getSymbols(Symbols = "DJIA", src = "yahoo", auto.assign = FALSE) 
head(djia)

write.csv(djia, "djia_2019.csv")
library(xts)
djiar <-  diff(log(djia$DJIA.Close))[-1]         
 
# par(mar=c(3,3,1,1), mgp=c(1.6,.6,0))
plot(djiar, ylab="DJIA Returns", main='', type='n')
lines(djiar)
dev.off()
# TTR::getYahooData is deprecated and will be removed in a future release.
# Please use quantmod::getSymbols instead.

#########
## Fig.1.5.Monthly SOI and Recruitment(estimated new),1950-1987.
par(mfrow = c(2,1), mar=c(2,2,1,0)+.5, mgp=c(1.6,.6,0), cex.main=1.05)
plot(soi, ylab="", xlab="", main="Southern Oscillation Index",   type='n')
grid(lty=1)
lines(soi)

plot(rec, ylab="", main="Recruitment",  type='n')
grid(lty=1)
lines(rec)
dev.off()


#########
## Example1.5 ElNiñoand Fish Population  
par(mfrow = c(2,1), mar=c(2,2,1,0)+.5, mgp=c(1.6,.6,0), cex.main=1.05)
ts.plot(fmri1[,2:5], ylab="BOLD", xlab="", main="Cortex", type='n')
grid(lty=1)
par(new=TRUE)
ts.plot(fmri1[,2:5], col=1:4, ylab="BOLD", xlab="", main="Cortex")

ts.plot(fmri1[,6:9], ylab="BOLD", xlab="", main="Thalamus & Cerebellum", type='n')
grid(lty=1)
par(new=TRUE)
ts.plot(fmri1[,6:9], col=1:4, ylab="BOLD", xlab="", main="Thalamus & Cerebellum")
mtext("Time (1 pt = 2 sec)", side=1, line=1.5)
dev.off()


#####################
## Example1.6 fMRI Imaging  
## Fig. 1.7. Arrival phases from an earthquake (top) and explosion (bottom) at 40 points per second.

par(mfrow = c(2,1), mar=c(2,2,1,0)+.5, mgp=c(1.6,.6,0), cex.main=1.05)
plot(EQ5, main="Earthquake", xlab="", type='n')
grid(lty=1)
lines(EQ5)
plot(EXP6, main="Explosion", xlab="", type='n')  
grid(lty=1)
lines(EXP6)
mtext("Time", side=1, line=1.5)
dev.off()

####################
## Example1.9 MovingAveragesandFiltering 
## Fig. 1.8. Gaussian white noise series (top) and three-point moving average of 
## the Gaussian whitenoiseseries(bottom).
par(mfrow = c(2,1), mar=c(2,2,1,0)+.5, mgp=c(1.6,.6,0), cex.main=1.05)
set.seed(1)
w = rnorm(500,0,1)                    # 500 N(0,1) variates
plot(w)

v = filter(w, sides=2, filter=rep(1/3,3))  # moving average

plot.ts(w, main="white noise", type='n')
grid(lty=1, col=gray(.9))
lines(w)

plot.ts(v, ylim=c(-3,3), main="moving average", type='n')
grid(lty=1, col=gray(.9))
lines(v)
dev.off()


####################
## Example1.10 Autoregressions
## Fig.1.9.Autoregressive series generated from model(1.2)
## page-11 
par(mar=c(2,2,1,0)+.5, mgp=c(1.6,.6,0))
w = rnorm(550,0,1)  # 50 extra to avoid startup problems

## x_t = x_t???1???.9x_t???2 + w_t (1.2)
x = filter(w, filter=c(1,-.9), method="recursive")[-(1:50)]
plot.ts(x, main="autoregression", type='n')
grid(lty=1, col=gray(.9))
lines(x)
dev.off()


#################
## Example1.11 Random Walk with Drift  
par(mar=c(2,2,1,0)+.5, mgp=c(1.6,.6,0), cex.main=1.05)
set.seed(154)                # so you can reproduce the results
w = rnorm(200,0,1)
x = cumsum(w)   
wd = w +.2 # random walk with drift
xd = cumsum(wd)

plot.ts(xd, ylim=c(-5,55), main="random walk", ylab='', type='n')
grid(lty=1)
lines(xd)
lines(x, col=4)
abline(h=0, col=4, lty=2)
abline(a=0, b=.2, lty=2)
dev.off()


####################
## Example1.12 SignalinNoise 
par(mfrow = c(3,1), mar=c(2,2.5,1,0)+.5, mgp=c(1.6,.6,0))
cs = 2*cos(2*pi*1:500/50 + .6*pi)
w = rnorm(500,0,1)

par(mfrow=c(3,1), mar=c(3,2,2,1), cex.main=1.05)

plot.ts(cs, ylab='',xlab='', main=expression(2*cos(2*pi*t/50+.6*pi)), type='n', cex.main=1.5)
grid(lty=1, col=gray(.9))
lines(cs) 

plot.ts(cs+w, ylab='',xlab='',main=expression(2*cos(2*pi*t/50+.6*pi) + N(0,1)), type='n', cex.main=1.5)
grid(lty=1, col=gray(.9))
lines(cs+w) 

plot.ts(cs+5*w, ylab='', main=expression(2*cos(2*pi*t/50+.6*pi) + N(0,5^2)), type='n', cex.main=1.5)
grid(lty=1, col=gray(.9))
lines(cs+5*w) 
dev.off()


###################
## page-22
## Fig.1.12.Autocorrelation function of a three-point moving average. 
ACF = c(0,0,0,1/3,2/3,1,2/3,1/3,0,0,0)
LAG = c(-5,-4,-3,-2,-1,0,1,2,3,4,5)
par(mar=c(2,2,0,0)+.5, mgp=c(1.6,.6,0))
plot(LAG, ACF, type='h', lwd=3,  panel.first=grid(lty=1))
abline(h=0)
points(LAG[-(4:8)],ACF[-(4:8)], pch=20)
dev.off()


###################
## Example1.24 Prediction Using Cross-Correlation
## Fig. 1.13. Demonstration of the results of Example 1.24 when l= 5. 
## The title shows which sideleads.

par(mar=c(2.2,2,.7,0)+.5, mgp=c(1.6,.6,0))
set.seed(2)
x = rnorm(100)
y = lag(x,-5) + rnorm(100)
ccf(y,x, ylab='CCovF', xlab="LAG", type='covariance',
    panel.first=grid(lty=1, col=gray(.9)), main="", lwd=2)
abline(v=0, lty=2)
text(11, .9, 'x leads')
text(-9, .9, 'y leads')
title(main="y & x", cex.main=1)
dev.off()


#######################
## Example1.25 Sample ACF and Scatter plots 
r = round(acf(soi, 6, plot=FALSE)$acf[-1], 3) # first 6 sample acf values
par(mfrow=c(1,2), mar=c(2.5,2.5,0,0)+.5, mgp=c(1.6,.6,0))
plot(lag(soi,-1), soi,panel.first=grid(lty=1))
legend('topleft', legend=r[1], bg='white', adj=.25, cex = 0.85)

plot(lag(soi,-6), soi,panel.first=grid(lty=1))
legend('topleft', legend=r[6], bg='white', adj=.25, cex = 0.8)
dev.off()


######################
## Example1.27 ACF of a Speech Signal 
num=250  
ACF = acf(speech, 250, plot = FALSE)$acf[-1]
LAG = 1:250
minA = min(ACF)
maxA = max(ACF)
U = 2/sqrt(num)
L = -U
minu = min(minA, L) - 0.01
maxu = min(maxA + 0.1, 1)
plot(LAG, ACF, type = "n", ylim = c(minu, maxu))
grid(lty = 1, col = gray(0.9))
abline(h = c(0, L, U), lty = c(1, 2, 2), col = c(1, 4, 4))
lines(LAG, ACF, type = "h")
#acf(speech, 250, panel.first=grid(lty=1))
dev.off()


###################
## 1.6 Vector-ValuedandMultidimensionalSeries
## Example1.30 SoilSurfaceTemperatures        
par(mar=c(0,1,0,0)+.5, mgp=c(1.6,.6,0))
persp(1:64, 1:36, soiltemp,  phi=25, theta=25, scale=FALSE, expand=4, 
      ticktype="detailed", xlab="rows", ylab="cols", zlab="temperature")
dev.off()

##

par(mar=c(3,3,1,1), mgp=c(1.6,.6,0))
plot.ts(rowMeans(soiltemp), xlab="row", ylab="Average Temperature" , type='n')
grid(lty=1); lines(rowMeans(soiltemp))
dev.off()


#######################
## Example1.31 Sample ACF of the Soil Temperature Series
par(mar=c(1,1,0,0)+.5)
fs = Mod(fft(soiltemp-mean(soiltemp)))^2/(64*36)
cs = Re(fft(fs, inverse=TRUE)/sqrt(64*36)) # ACovF
rs = cs/cs[1,1] # ACF
rs2 = cbind(rs[1:41,21:2], rs[1:41,1:21])
rs3 = rbind(rs2[41:2,], rs2)
persp(-40:40, -20:20, rs3, phi=30, theta=30, expand=30, scale="FALSE",
ticktype="detailed", xlab="row lags", ylab="column lags", zlab="ACF")
dev.off()


########
## Example1.28 SOI and Recruitment Correlation Analysis 
par(mfrow=c(3,1), mar=c(2,2.5,1,0)+.5, mgp=c(1.6,.6,0))
acf(soi, 48, xlab=' ', main="", panel.first=grid(lty=1))
mtext(side=3, 'Southern Oscillation Index', font=2)

acf(rec, 48, xlab='',main="", panel.first=grid(lty=1))
mtext(side=3, 'Recruitment', font=2)

ccf(soi, rec, 48, xlab='LAG', main="", ylab="CCF",panel.first=grid(lty=1))
mtext(side=3, 'SOI vs Recruitment', font=2)
dev.off()


########
## Example1.29 Prewhitening and Cross Correlation Analysis
set.seed(1492)
num=120; t=1:num
X = ts(2*cos(2*pi*t/12) + rnorm(num), freq=12)
Y = ts(2*cos(2*pi*(t+5)/12) + rnorm(num), freq=12)
Yw = resid( lm(Y~ cos(2*pi*t/12) + sin(2*pi*t/12), na.action=NULL) )

par(mfrow=c(3,2), mgp=c(1.6,.6,0), mar=c(3,3,1,1) )
plot(X, type='n'); grid(lty=1, col=gray(.9)); lines(X)
plot(Y, type='n'); grid(lty=1, col=gray(.9)); lines(Y)
acf(X,48, ylab='ACF(X)', panel.first=grid(lty=1, col=gray(.9)))
acf(Y,48, ylab='ACF(Y)', panel.first=grid(lty=1, col=gray(.9)))
ccf(X,Y,24, ylab='CCF(X,Y)', panel.first=grid(lty=1, col=gray(.9)))
ccf(X,Yw,24, ylab='CCF(X,Yw)', ylim=c(-.6,.6), panel.first=grid(lty=1, col=gray(.9)))
dev.off()
