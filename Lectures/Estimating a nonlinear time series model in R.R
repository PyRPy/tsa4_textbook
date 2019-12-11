# Estimating a nonlinear time series model in R
# https://robjhyndman.com/hyndsight/nlts/
# https://cran.r-project.org/web/packages/tsDyn/vignettes/tsDyn.pdf
# The model is a first order threshold autoregression:

# function to simulate time series -----------------------------------------

simnlts <- function(n, alpha, beta, r, sigma, gamma, burnin=100){
  # generate noise
  e <- rnorm(n + burnin, 0, sigma)
  # create space for y
  y <- numeric(n + burnin)
  # generate time series
  for(i in 2:(n + burnin)){
    if(y[i-1] <= r)
      y[i] <- alpha*y[i-1] + e[i]
    else
      y[i] <- beta*y[i-1] + gamma*e[i]
  }
  # throw away first burin values
  y <- ts(y[-(1:burnin)])
  # return results
  return(y)
}

# function to fit the model -----------------------------------------------

fitnlts <- function(x){
  ss <- function(par, x){
    alpha <- par[1]
    beta <- par[2]
    r <- par[3]
    n <- length(x)
    # check that each regime has at least 10% of observations
    if(sum(x<=r) < n/10 | sum(x>r) < n/10)
      return(1e20)
    e1 <- x[2:n] - alpha*x[1:(n-1)]
    e2 <- x[2:n] - beta*x[1:(n-1)]
    regime1 <- (x[1:(n-1)] <= r)
    e <- e1*(regime1) + e2*(!regime1)
    return(sum(e^2))
  }
  fit <- optim(c(0,0,mean(x)), ss, x=x, control = list(maxit=1000))
  if(fit$convergence > 0)
    return(rep(NA, 3))
  else
    return(c(alpha=fit$par[1], beta=fit$par[2], r=fit$par[3]))
}


# simulate data and run the model -----------------------------------------
set.seed(42)
y <- simnlts(100, 0.5, -1.8, -1, 1, 2)
plot(y, type="l")
fitnlts(y)
#     alpha       beta          r 
# 0.4986785 -2.1101805 -1.0003822 

# test model using tsDyn --------------------------------------------------

library(tsDyn)
tar1minus1 <- setar(y, m=1, thDelay = 0, th=-1)
summary(tar1minus1) # similar results
plot(tar1minus1) # to observe the trend and oscillations


#          Estimate  Std. Error  t value  Pr(>|t|)    
# const.L -0.223210    0.299579  -0.7451    0.4580    
# phiL.1   0.444638    0.089626   4.9610 3.029e-06 ***
# const.H -0.116553    0.212183  -0.5493    0.5841    
# phiH.1  -2.080732    0.145081 -14.3419 < 2.2e-16 ***

# without threshold given
tar1no.th <- setar(y, m=1, thDelay = 0)
summary(tar1no.th) # find the threshold value at -1.056, very good !

# Coefficient(s):
#   
#          Estimate  Std. Error  t value  Pr(>|t|)    
# const.L -0.223210    0.299579  -0.7451    0.4580    
# phiL.1   0.444638    0.089626   4.9610 3.029e-06 ***
# const.H -0.116553    0.212183  -0.5493    0.5841    
# phiH.1  -2.080732    0.145081 -14.3419 < 2.2e-16 ***
# ---
# Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
# 
# Threshold
# Variable: Z(t) = + (1) X(t) 
# 
# Value: -1.056
