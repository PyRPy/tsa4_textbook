# Reading chapter 5 Additional Time Domain Topics
library(astsa)
# 5.1 Long Memory ARMA and Fractional Differencing ------------------------
acf(log(varve), 100)
acf(cumsum(rnorm(1000)), 100)

# Example 5.1 Long Memory Fitting of the Glacial Varve Series
library(fracdiff)
lvarve <- log(varve) - mean(log(varve))
varve.fd <- fracdiff(lvarve, nar=0, nma=0, M=30)
varve.fd$d
varve.fd$stderror.dpq

p <- rep(1, 31)
for (k in 1:30){p[k+1] = (k-varve.fd$d)*p[k]/(k+1)}
plot(1:30, p[-1], ylab = expression(pi(d)), xlab = "Index", type = "h")
res.fd <- diffseries(log(varve), varve.fd$d) # frac diff resid
res.arima <- resid(arima(log(varve), order=c(1,1,1))) # arima resids
par(mfrow=c(2,1))
acf(res.arima, 100, xlim=c(4,97), ylim=c(-.2,.2), main="")
acf(res.fd, 100, xlim=c(4,97), ylim=c(-.2,.2), main="")

# Example 5.2 Long Memory Spectra for the Varve Series
series = log(varve) # specify series to be analyzed
d0 = .1 # initial value of d
n.per = nextn(length(series))
m = (n.per)/2 - 1
per = Mod(fft(series-mean(series))[-1])^2 # remove 0 freq and
per = per/n.per # scale the peridogram
g = 4*(sin(pi*((1:m)/n.per))^2)

# Function to calculate -log.likelihood
whit.like = function(d){
  g.d=g^d
  sig2 = (sum(g.d*per[1:m])/m)
  log.like = m*log(sig2) - d*sum(log(g)) + m
  return(log.like) }
# Estimation (output not shown)
(est = optim(d0, whit.like, gr=NULL, method="L-BFGS-B", hessian=TRUE,
             lower=-.5, upper=.5, control=list(trace=1,REPORT=1)))

##-- Results: d.hat = .380, se(dhat) = .028, and sig2hat = .229 --##
cat("d.hat =", est$par, "se(dhat) = ",1/sqrt(est$hessian),"\n")
g.dhat = g^est$par; sig2 = sum(g.dhat*per[1:m])/m
cat("sig2hat =",sig2,"\n")

par(mfrow=c(1,1))
u = spec.ar(log(varve), plot=FALSE) # produces AR(8)
g = 4*(sin(pi*((1:500)/2000))^2)
fhat = sig2*g^{-est$par} # long memory spectral estimate
plot(1:500/2000, log(fhat), type="l", ylab="log(spectrum)", xlab="frequency")
lines(u$freq[1:250], log(u$spec[1:250]), lty="dashed")
ar.mle(log(varve)) # to get AR(8) estimates

library(fracdiff)
fdGPH(log(varve), bandw=.9) # m = n^bandw


# 5.2 Unit Root Testing ---------------------------------------------------
# Example 5.3 Testing Unit Roots in the Glacial Varve Series
library(tseries)
adf.test(log(varve), k=0) # DF test

adf.test(log(varve)) # ADF test

pp.test(log(varve)) # PP test


# 5.3 GARCH Models --------------------------------------------------------
# Example 5.4 Analysis of U.S. GNP
u <- sarima(diff(log(gnp)), 1, 0, 0)
acf2(resid(u$fit)^2, 20)

library(fGarch)
summary(garchFit(~arma(1,0)+garch(1,0), diff(log(gnp))))

# Example 5.5 ARCH Analysis of the DJIA Returns
library(xts)
djiar <- diff(log(djia$Close))[-1]
acf2(djiar) # exhibits some autocorrelation (not shown)
acf2(djiar^2) # oozes autocorrelation (not shown)
library(fGarch)
summary(djia.g <- garchFit(~arma(1,0)+garch(1,1), data=djiar,
                           cond.dist='std'))
plot(djia.g) # to see all plot options


# 5.6 Multivariate ARMAX Models -------------------------------------------
# Example 5.10 Pollution, Weather, and Mortality
library(vars)
x <- cbind(cmort, tempr, part)
summary(VAR(x, p=1, type='both')) # 'both' fits constant + trend

# Example 5.11 Pollution, Weather, and Mortality (cont)
VARselect(x, lag.max=10, type="both")
summary(fit <- VAR(x, p=2, type="both")) # partial results displayed

acf(resid(fit), 52)
serial.test(fit, lags.pt=12, type="PT.adjusted") # noise is not white
(fit.pr = predict(fit, n.ahead = 24, ci = 0.95)) # 4 weeks ahead
fanchart(fit.pr) # plot prediction + error
