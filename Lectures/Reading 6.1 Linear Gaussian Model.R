## Chapter 6 State Space Models
library(astsa)
# 6.1 Linear Gaussian Model -----------------------------------------------

# Example 6.1 A Biomedical Example
plot(blood, type="o", pch=9, xlab="day", main="")

# Example 6.2 Global Warming
ts.plot(globtemp, globtempl, col=c(6,4), ylab="Temperature Deviations")

# Example 6.3 An AR(1) Process with Observational Noise


# 6.2 Filtering, Smoothing, and Forecasting -------------------------------

# Example 6.5 Prediction, Filtering and Smoothing for the Local Level Model
# generate data
set.seed(1)
num <- 50
w <- rnorm(num + 1, 0, 1)
v <- rnorm(num, 0, 1)
mu <- cumsum(w)
y <- mu[-1] + v
plot(y, type = "l")

# filter and smooth
ks <- Ksmooth0(num, y, A=1, mu0=0, Sigma0 = 1, Phi = 1, cQ=1, cR=1)

# plot for comparison
Time <- 1:num
par(mfrow=c(3,1))

plot(Time, mu[-1], main = "Predict", ylim=c(-5,10))
lines(ks$xp)
lines(ks$xp + 2*sqrt(ks$Pp), lty=2, col=4)
lines(ks$xp - 2*sqrt(ks$Pp), lty=2, col=4)

plot(Time, mu[-1], main='Filter', ylim=c(-5,10))
lines(ks$xf)
lines(ks$xf+2*sqrt(ks$Pf), lty=2, col=4)
lines(ks$xf-2*sqrt(ks$Pf), lty=2, col=4)

plot(Time, mu[-1], main='Smooth', ylim=c(-5,10))
lines(ks$xs)
lines(ks$xs+2*sqrt(ks$Ps), lty=2, col=4)
lines(ks$xs-2*sqrt(ks$Ps), lty=2, col=4)
mu[1]
ks$x0n
sqrt(ks$P0n)

dev.off()

# 6.3 Maximum Likelihood Estimation ---------------------------------------
# Example 6.6 Newton-Raphson for Example 6.3
# generate data
set.seed(999)
num <- 100
x <- arima.sim(n=num+1, list(ar=0.8), sd=1)
y <- ts(x[-1] + rnorm(num, 0, 1))

ts.plot(x, y, col=c(1,4))

# initial estimates
u <- ts.intersect(y, lag(y, -1), lag(y, -2))
varu <- var(u)
coru <- cor(u)
phi <- coru[1,3]/coru[1,2]
q <- (1-phi^2)*varu[1,2]/phi
r <- varu[1,1] - q/(1-phi^2)
(init.par = c(phi, sqrt(q), sqrt(r)))

# to evaluate likelihood
Linn <- function(para){
  phi <- para[1]
  sigw <- para[2]
  sigv <- para[3]
  Sigma0 <- (sigw^2)/(1-phi^2)
  Sigma0[Sigma0<0] = 0
  kf <- Kfilter0(num, y, 1, mu0=0, Sigma0, phi, sigw, sigv)
  return(kf$like)
}

# run the estimation
est <- optim(init.par, Linn, gr=NULL, method = "BFGS",
             hessian = TRUE,
             control = list(trace=1, REPORT=1))

SE <- sqrt(diag(solve(est$hessian)))
cbind(estimate=c(phi=est$par[1], sigw=est$par[2], sigv=est$par[3]), SE)

# Example 6.7 Newton-Raphson for the Global Temperature Deviations

# data preparation
y <- cbind(globtemp, globtempl)
num <- nrow(y)
input <- rep(1, num)

A <- array(rep(1,2), dim=c(2,1, num))
A
mu0 <- -0.35
Sigma0 <- 1
Phi <- 1

# Function to calculate likelihood
Linn <- function(para){
  cQ = para[1] # sigma_w
  cR1 = para[2] # 11 element of chol(R)
  cR2 = para[3] # 22 element of chol(R)
  cR12 = para[4] # 12 element of chol(R)
  cR = matrix(c(cR1,0,cR12,cR2),2) # put the matrix together
  drift = para[5]
  kf = Kfilter1(num,y,A,mu0,Sigma0,Phi,drift,0,cQ,cR,input)
  return(kf$like) 
}

# Estimation
init.par <- c(.1,.1,.1,0,.05) # initial values of parameters
(est <- optim(init.par, Linn, NULL, method='BFGS', hessian=TRUE,
             control=list(trace=1,REPORT=1)))
SE <-  sqrt(diag(solve(est$hessian)))

# Display estimates
u <- cbind(estimate=est$par, SE)
rownames(u) <- c('sigw','cR11', 'cR22', 'cR12', 'drift')
u

# Smooth (first set parameters to their final estimates)
cQ = est$par[1]
cR1 = est$par[2]
cR2 = est$par[3]
cR12 = est$par[4]
cR = matrix(c(cR1,0,cR12,cR2), 2)
(R = t(cR)%*%cR) # to view the estimated R matrix
drift = est$par[5]
ks = Ksmooth1(num,y,A,mu0,Sigma0,Phi,drift,0,cQ,cR,input)

# Plot
xsm = ts(as.vector(ks$xs), start=1880)
rmse = ts(sqrt(as.vector(ks$Ps)), start=1880)
plot(xsm, ylim=c(-.6, 1), ylab='Temperature Deviations')
xx = c(time(xsm), rev(time(xsm)))
yy = c(xsm-2*rmse, rev(xsm+2*rmse))
polygon(xx, yy, border=NA, col=gray(.6, alpha=.25))
lines(globtemp, type='o', pch=2, col=4, lty=6)
lines(globtempl, type='o', pch=3, col=3, lty=6)

# Example 6.8 EM Algorithm for Example 6.3
library(nlme) # loads package nlme

# Generate data (same as Example 6.6)
set.seed(999)
num = 100
x = arima.sim(n=num+1, list(ar = .8), sd=1)
y = ts(x[-1] + rnorm(num,0,1))

# Initial Estimates (same as Example 6.6)
u = ts.intersect(y, lag(y,-1), lag(y,-2))
varu = var(u); coru = cor(u)
phi = coru[1,3]/coru[1,2]
q = (1-phi^2)*varu[1,2]/phi
r = varu[1,1] - q/(1-phi^2)

# EM procedure - output not shown
(em = EM0(num, y, A=1, mu0=0, Sigma0=2.8, Phi=phi, cQ=sqrt(q), cR=sqrt(r),
          max.iter=75, tol=.00001))

# Standard Errors (this uses nlme)
phi = em$Phi
cq = sqrt(em$Q)
cr = sqrt(em$R)
mu0 = em$mu0
Sigma0 = em$Sigma0
para = c(phi, cq, cr)

Linn = function(para){ # to evaluate likelihood at estimates
  kf = Kfilter0(num, y, 1, mu0, Sigma0, para[1], para[2], para[3])
  return(kf$like) 
  }
emhess = fdHess(para, function(para) Linn(para))
SE = sqrt(diag(solve(emhess$Hessian)))

# Display Summary of Estimation
estimate = c(para, em$mu0, em$Sigma0)
SE = c(SE, NA, NA)
u = cbind(estimate, SE)
rownames(u) = c('phi','sigw','sigv','mu0','Sigma0')
u
