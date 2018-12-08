## ARIMA Models
library('astsa')
source('grid.r')

##########
## page-80
## Example3.2 TheSamplePathofanAR(1)Process 
set.seed(101010)
par(mfrow = c(2,1), mar=c(1.5,2,1,0)+.5, mgp=c(1.6,.6,0), cex.main=1.05)
x<-arima.sim(list(order=c(1,0,0), ar=.9), n=100)
plot(x, ylab="x", xlab="", main=(expression(AR(1)~~~phi==+.9)), type='n')
grid(lty=1)
lines(x)

x<-arima.sim(list(order=c(1,0,0), ar=-.9), n=100)
plot(x, ylab="x",  xlab="",  main=(expression(AR(1)~~~phi==-.9)), type='n')
grid(lty=1)
lines(x)
mtext('Time', side=1, line=1)
dev.off()


#####################
## Example3.5 The MA(1) Process 
par(mfrow = c(2,1), mar=c(1.5,2,1,0)+.5, mgp=c(1.6,.6,0), cex.main=1.05)
set.seed(101010)
plot(x<-arima.sim(list(order=c(0,0,1), ma=.9), n=100), ylab="x", xlab="", 
     main=(expression(MA(1)~~~theta==+.9)), type='n')
grid(lty=1)
lines(x)

plot(x<-arima.sim(list(order=c(0,0,1), ma=-.9), n=100), ylab="x", xlab='', 
     main=(expression(MA(1)~~~theta==-.9)), type='n')
grid(lty=1)
lines(x)
mtext('Time', side=1, line=1)
dev.off()

################
## Example3.7 Parameter Redundancy
set.seed(8675309) # Jenny, I got your number 
x = rnorm(150, mean=5) # generate iid N(5,1)s 
arima(x, order=c(1,0,1)) # estimation 
## (1+ .96B)xt = (1+ .95B)wt 


################
## Example3.8 ParameterRedundancy,Causality,Invertibility 
ARMAtoMA(ar = .9, ma = .5, 10) # first 10 psi-weights 
ARMAtoMA(ar = -.5, ma = -.9, 10) # first 10 pi-weights
plot(ARMAtoMA(ar=.9, ma=.5, 50)) # for a graph

################
## Example3.9 Causal Conditions for an AR(2) Process
par(mar=c(3,3,2,1), mgp=c(1.6,.6,0))
phi1p=seq(0, 2, by=0.1)
phi1m=seq(-2,0, by=.1)
phi1=seq(-2, 2, by=0.1)
phi2=seq(-1, 1, by=0.1)
name1=expression(phi[1])
name2=expression(phi[2])
plot(phi1p, (1-phi1p), typ="l", ylim=c(-1,1), xlim=c(-2,2), ylab=name2, xlab=name1)
lines(phi1m, (1+phi1m), typ="l", ylim=c(-1,1), xlim=c(-2,2)) 
abline(h=0, v=0, lty=2, col='#cccccc')
lines(phi1, -(phi1^2 /4), ylim=c(-1,1))
lines( x=c(-2,2), y=c(-1,-1), typ="l", ylim=c(-1,1))
text(0,.35,'real roots')
text(0,-.5, 'complex roots')
mtext('Causal Region of an AR(2)', side=3, line=.5)
dev.off()


####################
# E3.11
z = c(1,-1.5,.75) # coefficients of the polynomial 
(a = polyroot(z)[1]) # print one root = 1 + i/sqrt(3) 
arg = Arg(a)/(2*pi) # arg in cycles/pt 
1/arg # the pseudo period 

set.seed(8675309)

par(mar=c(2,2,1,0)+.5, mgp=c(1.6,.6,0))
ar2 = arima.sim(list(order=c(2,0,0), ar=c(1.5,-.75)), n = 144)
plot(ar2, axes=FALSE, xlab="Time", type='n') 
axis(2)
axis(1, at=seq(0,144,by=12))
box()
abline(v=seq(0,144,by=12), lty=2)
abline(h=c(-5,0,5), lty=1, col=gray(.9))
lines(ar2)
plot(ar2) # or simply
dev.off()
 

################
## Example3.16 The PACF of an AR(p) 
ACF = ARMAacf(ar=c(1.5,-.75), ma=0, 24)[-1]
PACF = ARMAacf(ar=c(1.5,-.75), ma=0, 24, pacf=TRUE) 
par(mfrow=c(1,2), mar=c(2.5,2.5,.5,0)+.5, mgp=c(1.6,.6,0))

plot(ACF, type="h", xlab="lag", ylim=c(-.8,1), panel.first=grid(lty=1)); abline(h=0)
plot(PACF, type="h", xlab="lag", ylim=c(-.8,1), panel.first=grid(lty=1)); abline(h=0)
dev.off()

#######################
## Example3.18 Preliminary Analysis of the Recruitment Series
acf2(rec, 48) # will produce values and a graphic 
(regr = ar.ols(rec, order=2, demean=FALSE, intercept=TRUE)) 
regr$asy.se.coef # standard errors of the estimates
## The estimates and standard errors (in parentheses) are 
## ??0 = 6.74(1.11), ??1 = 1.35(.04), ^ ??2 = ???.46(.04),and ^ ??2 w = 89.72.
## xt = ??0 + ??1xt???1 + ??2xt???2 + wt 

#######################
u = acf2(rec, 48); dev.off(); ACF=u[,1]; PACF=u[,2]
LAG = 1:48/frequency(rec)
num = length(rec)
minA = min(ACF)
maxA = max(ACF)
minP = min(PACF)
maxP = max(PACF)
U = 2/sqrt(num)
L = -U
minu = min(minA, minP, L) - 0.01
maxu = min(max(maxA + 0.1, maxP + 0.1), 1)
pdf(file="recacf.pdf",width=7,height=4) 
par(mfrow=c(2,1), mar=c(2,2,0,0)+.5, mgp=c(1.5,.6,0))
plot(LAG, ACF, type="h", xlab="LAG", ylim = c(minu, maxu), panel.first=grid(lty=1)); abline(h=0)
abline(h = c(L, U), col=4, lty=2)  
plot(LAG, PACF, type="h", xlab="LAG",  ylim = c(minu, maxu) , panel.first=grid(lty=1)); abline(h=0)
abline(h = c(L, U), col=4, lty=2)  
dev.off()


############
## Example3.25 Forecasting the Recruitment Series
par(mar=c(2.5,2.5,0,0)+.5, mgp=c(1.6,.6,0))
regr = ar.ols(rec, order=2, demean=FALSE, intercept=TRUE)
fore = predict(regr, n.ahead=24)
ts.plot(rec, fore$pred, col=1:2, xlim=c(1980,1990), ylab="Recruitment", type='n')
grid(lty=1)
par(new=TRUE)
ts.plot(rec, fore$pred, col=1:2, xlim=c(1980,1990), ylab="Recruitment")
 U = fore$pred+fore$se
 L = fore$pred-fore$se	
 xx = c(time(U), rev(time(U)))
 yy = c(L, rev(U))
polygon(xx, yy, border = 8, col = gray(0.6, alpha = 0.2))
lines(fore$pred, type="p", col=2)
dev.off()
## plot together ??
ts.plot(rec, fore$pred, col=1:2) # quick plot

############
## Example3.26 Backcasting an ARMA(1,1) 
set.seed(90210)
x = arima.sim(list(order = c(1,0,1), ar =.9, ma=.5), n = 100)               
xr = rev(x)                                   # xr is the reversed data
pxr = predict(arima(xr, order=c(1,0,1)), 10)  # predict the reversed data
pxrp = rev(pxr$pred)              # reorder the predictors (for plotting)
pxrse = rev(pxr$se)               # reorder the SEs
nx = ts(c(pxrp, x), start=-9)     # attach the backcasts to the data
#
par(mar=c(3,3,1.5,1), mgp=c(1.6,.6,0), cex.main=1.1)
plot(nx, ylab=expression(X[~t]), main='Backcasting', type='n')
grid(lty=1)
lines(nx)
 U=  nx[1:10] + pxrse
 L = nx[1:10] - pxrse	
 xx = c(-9:0, 0:-9)
 yy = c(L, rev(U))
 polygon(xx, yy, border = 8, col = gray(0.6, alpha = 0.2))
 lines(-9:0, nx[1:10], col=2, type='o') 
dev.off()

#######################
## Example3.28 Yule-WalkerEstimationoftheRecruitmentSeries
rec.yw = ar.yw(rec, order=2) 
rec.yw$x.mean # = 62.26 (mean estimate) 
rec.yw$ar # = 1.33, -.44 (coefficient estimates) 
sqrt(diag(rec.yw$asy.var.coef)) # = .04, .04 (standard errors) 
rec.yw$var.pred # = 94.80 (error variance estimate) 

rec.pr = predict(rec.yw, n.ahead=24) 
ts.plot(rec, rec.pr$pred, col=1:2) 
lines(rec.pr$pred + rec.pr$se, col=4, lty=2) 
lines(rec.pr$pred - rec.pr$se, col=4, lty=2)

#######################
u = acf2(diff(log(varve)), 36)
dev.off()
ACF=u[,1]
PACF=u[,2]
num = length(varve[-1])
LAG = 1:36/frequency(varve)
minA = min(ACF)
maxA = max(ACF)
minP = min(PACF)
maxP = max(PACF)
U = 2/sqrt(num)
L = -U
minu = min(minA, minP, L) - 0.01
maxu = min(max(maxA + 0.1, maxP + 0.1), 1)

par(mfrow=c(2,1), mar=c(2,2,0,0)+.5, mgp=c(1.5,.6,0))
plot(LAG, ACF, type="h", xlab="LAG", ylim = c(minu, maxu), panel.first=grid(lty=1)); abline(h=0)
abline(h = c(L, U), col=4, lty=2)  
plot(LAG, PACF, type="h", xlab="LAG",  ylim = c(minu, maxu) , panel.first=grid(lty=1)); abline(h=0)
abline(h = c(L, U), col=4, lty=2)  
dev.off()

############################
## Example3.29 Method of Moments Estimation for an MA(1) 
set.seed(2) 
ma1 = arima.sim(list(order = c(0,0,1), ma = 0.9), n = 50) 
acf(ma1, plot=FALSE)[1] # = .507 (lag 1 sample ACF) 
dev.off()

par(mfrow=c(2,1))
plot(ma1)
acf(ma1)

###########################
## Example3.31 MLE for the Recruitment Series
rec.mle = ar.mle(rec, order=2) 
rec.mle$x.mean # 62.26 
rec.mle$ar # 1.35, -.46 
sqrt(diag(rec.mle$asy.var.coef)) # .04, .04 
rec.mle$var.pred # 89.34



###############
## Example3.32 Gauss-NewtonforanMA(1) 
## Example3.33 FittingtheGlacialVarveSeries
x=diff(log(varve))
r=acf(x, lag=1, plot=FALSE)$acf[-1]
rstart = (1-sqrt(1-4*(r^2)))/(2*r)    #example 3.29 (e2.27)
c(0) -> w 
c() -> Sc 
num = length(x)
th = seq(-.3,-.94,-.01)
for (p in 1:length(th)){
    for (i in 2:num){w[i]=x[i]-th[p]*w[i-1]}
	Sc[p] = sum(w^2)
	}		
par(mar=c(2,2.5,0,0)+.5, mgp=c(1.6,.6,0))	
plot(th, Sc, type="l",ylab=expression(S[c](theta)), xlab=expression(theta),lwd=2, panel.first=grid(NA, NULL,lty=1)) 
# estimation
c(0) -> w -> z
c() -> Sc -> Sz -> Szw
para = c()
niter = 15
para[1]=rstart
  for (p in 1:niter){
    for (i in 2:num){w[i]=x[i]-para[p]*w[i-1]
                   z[i]=w[i-1]-para[p]*z[i-1]
                   }
  Sc[p] = sum(w^2)				   
  Sz[p]=sum(z^2)
  Szw[p]=sum(z*w)
  para[p+1] = para[p] + Szw[p]/Sz[p]
  }  
#round(cbind(iteration=0:(niter-1), thetahat=para[1:niter] , Sc , Sz ), 3)
abline(v=para[1:12], lty=2)
points(para[1:12], Sc[1:12], pch=16)
dev.off()
###########

#############
## Example3.36 Bootstrapping an AR(1) 
par(mar=c(3,3,1,1), mgp=c(1.6,.6,0))
set.seed(101010)
e = rexp(150, rate=.5)
u = runif(150,-1,1)
de = e*sign(u)
dex = 50 + arima.sim(n=100,list(ar=.95), innov=de, n.start=50)
plot.ts(dex, type='n', ylab=expression(X[~t]))
grid(lty=1, col=gray(.9))
lines(dex, type='o')
dev.off()
##
##
fit = ar.yw(dex, order=1) 
round(cbind(fit$x.mean, fit$ar, fit$var.pred), 2) 

# simulate 'true' distn
set.seed(111)
phi.yw = rep(NA, 1000)

for (i in 1:1000){
  e = rexp(150, rate=.5); u = runif(150,-1,1); de = e*sign(u)
  x = 50 + arima.sim(n=100,list(ar=.95), innov=de, n.start=50)
  phi.yw[i] = ar.yw(x, order=1)$ar }

# fit to dex (generated above)
fit = ar.yw(dex, order=1)

# bootstrap
set.seed(666)
m = fit$x.mean
phi = fit$ar  # estimate of phi
nboot = 500   # number of bootstrap replicates
resids = fit$resid[-1]  # the first resid is NA
x.star = dex    # initialize x*
phi.star.yw = rep(NA, nboot)
for (i in 1:nboot) {
  resid.star = sample(resids, replace=TRUE)
  for (t in 1:99){ x.star[t+1] = m + phi*(x.star[t]-m) + resid.star[t] }
  phi.star.yw[i] = ar.yw(x.star, order=1)$ar } 

# plot everything
culer = rgb(.5,.7,1,.5)
par(mar=c(3,3,1,1), mgp=c(1.6,.6,0))
hist(phi.star.yw, 15, main="", prob=TRUE, xlim=c(.65,1.05), ylim=c(0,14), col=culer, xlab=expression(hat(phi)))
lines(density(phi.yw, bw=.02), lwd=2) 
u = seq(.75, 1.1, by=.001)
lines(u, dnorm(u, mean=.96, sd=.03), lty="dashed", lwd=2) 
legend(.65,14, legend=c('true distribution', 'bootstrap distribution', 'normal approximation'),
       bty='n', col=1, lty=c(1,0,2), lwd=c(2,0,2),
       pch=c(NA,22,NA), pt.bg=c(NA,culer,NA), pt.cex=2.5)
dev.off()
#################################################

#################################################
## Example3.38 IMA(1,1) and EWMA
## xt = xt???1 + wt ?????wt???1, 
set.seed(666) 
x = arima.sim(list(order = c(0,1,1), ma = -0.8), n = 100) 
(x.ima = HoltWinters(x, beta=FALSE, gamma=FALSE)) 
# ?? below is 1????? Smoothing parameter: alpha: 0.1663072 
plot(x.ima)


########################
## Example3.39 Analysis of GNP Data
layout(matrix(c(1, 1, 2), ncol = 1))
par(mar=c(2.75,2.5,.5,.5), mgp=c(1.6,.6,0), cex.lab=1.1) 
plot(gnp, ylab="Billions of Dollars",  type='n')
grid(lty=1, col=gray(.9))

lines(gnp)
# acf  
acf(gnp, 48, panel.first=grid(lty=1))
dev.off()


#############
## Fig.3.14.U.S.GNP quarterly growth rate.The horizontal line displays the average growth of the process,
## which is close to 1%.
par(mar=c(2.75,2.5,.5,.5), mgp=c(1.6,.6,0)) 
plot(diff(log(gnp)), ylab="GNP Growth Rate", type='n')
grid(lty=1, col=gray(.9)); lines(diff(log(gnp)))
abline(h=mean(diff(log(gnp))), col=4)
dev.off()


############ 
## Fig.3.15.Sample ACF and PACF of the GNP quarterly growth rate.Lag is in terms of years
ACF = acf(diff(log(gnp)), 24, plot=FALSE)$acf[-1]
PACF = pacf(diff(log(gnp)), 24, plot=FALSE)$acf
num = length(gnp)-1
minA = min(ACF)
maxA = max(ACF)
minP = min(PACF)
maxP = max(PACF)
U = 2/sqrt(num)
L = -U
LAG = 1:24/4
minu = min(minA, minP, L) - 0.01
maxu = min(maxA + 0.2, maxP + 0.2, 1)

par(mfrow=c(2,1), mar=c(2,2.5,0,0)+.5, mgp=c(1.4,.6,0))
plot(LAG, ACF, type="h", xlab="LAG", ylim = c(minu, maxu), panel.first=grid(lty=1)); abline(h=0)
abline(h = c(L, U), col=4, lty=2)  
plot(LAG, PACF, type="h", xlab="LAG",  ylim = c(minu, maxu) , panel.first=grid(lty=1)); abline(h=0)
abline(h = c(L, U), col=4, lty=2)  
dev.off()

#############
## Fig.3.16.Diagnostics of the residuals from MA(2) ???t on GNP growth rate.
sarima(diff(log(gnp)), 0, 0, 2) # MA(2)
dev.off()



#############
## Example3.41 DiagnosticsfortheGlacialVarveSeries
## Fig.3.17.Q-statisticp-valuesfortheARIMA(0,1,1)???t(top)andtheARIMA(1,1,1)???t(bottom) totheloggedvarvedata.
par(mfrow=c(2,1), mar=c(2,2.5,.5,0)+.5, mgp=c(1.4,.6,0))
rs = resid(arima(log(varve), order=c(0,1,1)))
pval=c()
nlag=20
ppq=1
for (i in (ppq+1):nlag) {
  u <- stats::Box.test(rs, i, type = "Ljung-Box")$statistic
  pval[i] <- stats::pchisq(u, i - ppq, lower.tail = FALSE)
}
plot((ppq + 1):nlag, pval[(ppq + 1):nlag], xlab = "lag", cex.main=1, font.main=1,
     ylab = "p value", ylim = c(0, 1), main = "p values for Ljung-Box statistic")
abline(h = 0.05, lty = 2, col = "blue")
#
rs = resid(arima(log(varve), order=c(1,1,1)))
pval=c()
nlag=20
ppq=2
for (i in (ppq+1):nlag) {
  u <- stats::Box.test(rs, i, type = "Ljung-Box")$statistic
  pval[i] <- stats::pchisq(u, i - ppq, lower.tail = FALSE)
}
plot((ppq + 1):nlag, pval[(ppq + 1):nlag], xlab = "lag", 
     ylab = "p value", ylim = c(0, 1), main = "")
abline(h = 0.05, lty = 2, col = "blue")
dev.off()

## page-143
sarima(log(varve), 0, 1, 1, no.constant=TRUE) # ARIMA(0,1,1) 
sarima(log(varve), 1, 1, 1, no.constant=TRUE) # ARIMA(1,1,1)

## Example3.43 Model Choice for the U.S. GNP Series
gnpgr <- diff(log(gnp))
sarima(gnpgr, 1, 0, 0) # AR(1) 
sarima(gnpgr, 0, 0, 2) # MA(2) 

## The AIC and AICc both prefer the MA(2) ???t, whereas the BIC prefers the simplerAR(1)model.
## ItisoftenthecasethattheBICwillselectamodelofsmaller orderthantheAICorAICc.Ineithercase,
## itisnotunreasonabletoretaintheAR(1) becausepureautoregressivemodelsareeasiertoworkwith.

## 3.8 Regression with Autocorrelated Errors
## Example3.44 Mortality,Temperature and Pollution
## Fig.3.19.Sample ACF and PACF of the mortality residuals in dicating an AR(2) process
## An easy way to tackle this problem was ???rst presented in Cochrane and Orcutt(1949),
trend = time(cmort)
temp = tempr - mean(tempr)
temp2 = temp^2 

summary(fit <- lm(cmort~trend + temp + temp2 + part, na.action=NULL)) 
acf2(resid(fit), 52) # implies AR2 

sarima(cmort, 2,0,0, xreg=cbind(trend,temp,temp2,part)) 

## Example3.44 Mortality,Temperature and Pollution | detailed codes
## Fig.3.19.Sample ACF and PACF of the mortality residuals in dicating an AR(2) process
par(mfrow=c(2,1), mar=c(2.5,2.5,0,0)+.5, mgp=c(1.5,.6,0))
trend = time(cmort)
temp = tempr - mean(tempr)
temp2 = temp^2

fit <- lm(cmort~trend + temp + temp2 + part, na.action=NULL)
ACF = acf(resid(fit), 52, plot=FALSE)$acf[-1]
PACF = pacf(resid(fit), 52, plot=FALSE)$acf
num = length(cmort)
minA = min(ACF)
maxA = max(ACF)
minP = min(PACF)
maxP = max(PACF)
U = 2/sqrt(num)
L = -U
LAG = 1:52/52
minu = min(minA, minP, L) - 0.01
maxu = min(maxA + 0.2, maxP + 0.2, 1)
plot(LAG, ACF, type="h", ylim = c(minu, maxu), panel.first=grid(lty=1)); abline(h=0)
abline(h = c(L, U), col=4, lty=2)  
plot(LAG, PACF, type="h", ylim = c(minu, maxu) , panel.first=grid(lty=1)); abline(h=0)
abline(h = c(L, U), col=4, lty=2)  
dev.off()

## page-147
## Example3.45 Regression with Lagged Variables(cont) 
## Rt = ??0 + ??1St???6 + ??2Dt???6 + ??3Dt???6 St???6 + wt, 
dummy = ifelse(soi<0, 0, 1) 
fish = ts.intersect(rec, soiL6=lag(soi,-6), dL6=lag(dummy,-6), dframe=TRUE)
summary(fit <- lm(rec ~soiL6*dL6, data=fish, na.action=NULL)) 
attach(fish) 
plot(resid(fit)) 
acf2(resid(fit)) # indicates AR(2) 
intract = soiL6*dL6 # interaction term 
sarima(rec,2,0,0, xreg = cbind(soiL6, dL6, intract)) 

## 3.9 Multiplicative Seasonal ARIMA Models
###########################################
## Example3.46 A Seasonal AR Series 
set.seed(666)
phi = c(rep(0,11),.9)
sAR = arima.sim(list(order=c(12,0,0), ar=phi), n=37)
sAR = ts(sAR, freq=12)
layout(matrix(c(1,1,2, 1,1,3), nc=2))
par(mar=c(2.5,2.5,2,1), mgp=c(1.6,.6,0))
plot(sAR, axes=FALSE, col='#808080', main='seasonal AR(1)', xlab="year", type='c')
abline(v=1:4, lty=2, col=gray(.6))
abline(h=seq(-4,2,2), col=gray(.9), lty=1)
Months = c("J","F","M","A","M","J","J","A","S","O","N","D")
points(sAR, pch=Months, cex=1.35, font=4, col=1:4) 
axis(1,1:4) 
axis(2)
box()

## lower part of the plot
ACF = ARMAacf(ar=phi, ma=0, 100)[-1]  # [-1] removes 0 lag
PACF = ARMAacf(ar=phi, ma=0, 100, pacf=TRUE)
plot(ACF, type="h", xlab="LAG", ylim=c(-.1,1), axes=FALSE);
segments(0,0,0,1)
axis(1, seq(0,100,by=12))
axis(2)
box()
abline(h=0)
plot(PACF, type="h", xlab="LAG", ylim=c(-.1,1), axes=FALSE);
axis(1, seq(0,100,by=12))
axis(2)
box()
abline(h=0)
dev.off()

## Example3.47 A Mixed Seasonal Model
## Fig.3.21. ACF and PACF of the mixed seasonal ARMA model xt = .8xt???12 + wt ???.5wt???1. 
phi = c(rep(0,11),.8)
ACF = ARMAacf(ar=phi, ma=-.5, 50)[-1]     # [-1] removes 0 lag
PACF = ARMAacf(ar=phi, ma=-.5, 50, pacf=TRUE)
par(mfrow=c(1,2), mar=c(2.5,2.5,2,1), mgp=c(1.6,.6,0))
plot(ACF,  type="h", xlab="LAG", ylim=c(-.4,.8), axes=FALSE)  
abline(h=0)
axis(1, seq(0,50,by=12))
axis(2)
box()
plot(PACF, type="h", xlab="LAG", ylim=c(-.4,.8), axes=FALSE)  
abline(h=0)
axis(1, seq(0,50,by=12))
axis(2)
box()
dev.off()

## Example3.49 Air Passengers
## Fig. 3.22. R data set AirPassengers, which are the monthly totals of international airline passengersx,
## andthetransformeddata:lx = logxt,dlx = ???logxt,andddlx = ???12???logxt.
par(mfrow=c(4,1), mar = c(0, 3, 0, 3), oma=c(3,0,2,0), mgp=c(1.6,.6,0), cex.lab=1.5)
x = AirPassengers
lx = log(x)
dlx = diff(lx)
ddlx = diff(dlx, 12)

u = ts.union(x,lx,dlx,ddlx) # create a new time series
plot.ts(u[,1], ylab='x', xaxt="no", type='n')
grid(lty=1, col=gray(.9))
lines(u[,1])
plot.ts(u[,2], ylab='lx', xaxt="no", type='n', yaxt='no', ylim=c(4.5,6.5))
grid(lty=1, col=gray(.9))
axis(4)
lines(u[,2]) 

plot.ts(u[,3], ylab='dlx', xaxt="no", type='n')
grid(lty=1, col=gray(.9))
lines(u[,3])

plot.ts(u[,4], ylab='ddlx', yaxt='no', type='n')
grid(lty=1, col=gray(.9))
axis(4)
lines(u[,4])
title(xlab="Time", outer=TRUE)
dev.off()



sarima(log(AirPassengers), 0, 1, 1, 0, 1, 1, 12) 
dev.off()


####################
x = AirPassengers
xdata = log(x)
fore = sarima.for(xdata, 12, 0,1,1, 0,1,1,12)
dev.off()
# -- for publication

par(mar=c(2,2,0,0)+.5, mgp=c(1.4,.6,0))
n = length(xdata)
U = fore$pred + 2 * fore$se
L = fore$pred - 2 * fore$se
U1 = fore$pred + fore$se
L1 = fore$pred - fore$se
a = max(1, n - 100)
minx = min(xdata[a:n], L)
maxx = max(xdata[a:n], U)
xnew = window(xdata, start=1953)
ts.plot(xnew, fore$pred, col = 1:2, ylim = c(minx, maxx), type='n')
grid(lty=1); par(new=TRUE)
ts.plot(xnew, fore$pred, col = 1:2, type = "o", ylim = c(minx, maxx), ylab='log(AirPassengers)')
xx = c(time(U), rev(time(U)))
yy = c(L, rev(U))
polygon(xx, yy, border = 8, col = gray(0.6, alpha = 0.2))
yy1 = c(L1, rev(U1))
polygon(xx, yy1, border = 8, col = gray(0.6, alpha = 0.2))
lines(fore$pred, col = "red", type = "o")	
dev.off()


###########################
## tsa4 book version codes in R
x = AirPassengers 
lx = log(x)
dlx = diff(lx)
ddlx = diff(dlx, 12)

plot.ts(cbind(x,lx,dlx,ddlx), main="") 
# below of interest for showing seasonal RW (not shown here): 
par(mfrow=c(2,1)) 
monthplot(dlx)
monthplot(ddlx)

## Seasonsal Component: It appears that at the seasons, the ACF is cutting o??? a lag 1s (s = 12),
## whereas the PACF is tailing o??? at lags 1s,2s,3s,4s,. . .

## Non-Seasonsal Component:Inspecting the sample ACF and PACF at the lowerlags, it appears as though 
## both are tailing o???. 
acf2(ddlx,50)

## Thus,we ???rst try an ARIMA(1,1,1)×(0,1,1)12 on the logged data
sarima (lx, 1,1,1, 0,1,1,12) 

## However, the AR parameter is not signi???cant, so we should try dropping one parameter from the
## within seasons part
sarima(lx, 0,1,1, 0,1,1,12) 

sarima(lx, 1,1,0, 0,1,1,12) 

## All information criteria prefer the ARIMA(0,1,1)×(0,1,1)12 model,
## which is the model displayed in(3.164).

## Finally,weforecasttheloggeddataouttwelvemonths,andtheresultsareshown inFigure3.25. 
sarima.for(lx, 12, 0,1,1, 0,1,1,12)
sarima.for(lx, 24, 0,1,1, 0,1,1,12) # just try
