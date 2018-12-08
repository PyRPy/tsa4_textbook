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



