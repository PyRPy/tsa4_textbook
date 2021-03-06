# 6.9 Hidden Markov Models and Switching Autoregression
# Example 6.16  Poisson HMM - Number of Major Earthquakes (cont)

#  modeled as a two-state Poisson HMM mode --------------------------------
library(astsa)
library(depmixS4)

data("EQcount")
head(EQcount)
plot(EQcount, type="l")

model <- depmix(EQcount ~ 1, nstates = 2, data = data.frame(EQcount),
                family = poisson())

set.seed(90210)
summary(fm <- fit(model))

# get parameters
u <- as.vector(getpars(fm))
if (u[7] <= u[8]){para.mle = c(u[3:6], exp(u[7]), exp(u[8]))
} else {para.mle = c(u[6:3], exp(u[8]), exp(u[7]))}

mtrans <- matrix(para.mle[1:4], byrow = TRUE, nrow = 2)
lams <- para.mle[5:6]
pi1 <- mtrans[2,1]/(2 - mtrans[1,1] - mtrans[2,2])
pi2 <- 1 - pi1

# graphics - data and states
plot(EQcount, main="", ylab="EQcount", type='h', col=gray(.7))
text(EQcount, col = 6*posterior(fm)[,1] - 2, 
     labels = posterior(fm)[,1], cex=.9)

# prob of state 2
plot(ts(posterior(fm)[,3], start = 1900), 
     ylab= expression(hat(pi)[~2]*'(t/n'))

abline(h=.5, lty=2)

# histogram
hist(EQcount, breaks = 30, probability = TRUE, main="")
xvals <- seq(1,45)
u1 <- pi1*dpois(xvals, lams[1])
u2 <- pi2*dpois(xvals, lams[2])
lines(xvals, u1, col=4)
lines(xvals, u2, col=2)

# boots trapping section is skipped for now
# function to generate data
pois.HHM.generate_sample <- function(n,m,lambda, Mtrans, StatDist=NULL){
  # n = data length
  # m = number of states
  # Mtrans = transition matrix
  # StatDist = stationary distn
  if(is.null(StatDist)) StatDist=solve(t(diag(m)-Mtrans + 1),rep(1,m))
  mvect = 1:m
  state=numeric(n)
  state[1]=sample(mvect, 1, prob = StatDist)
  for (i in 2:n) state[i] = sample(mvect, 1, prob = Mtrans[state[i-1],])
  y = rpois(n, lambda = lambda[state])
  list(y=y, state=state)
}

# start it up
set.seed(10101101)
nboot <- 100
nobs <- length(EQcount)
para.star <- matrix(NA, nrow=nboot, ncol=6)
for (j in 1:nboot){
  x.star <- pois.HHM.generate_sample(n=nobs, m=2, lambda = lams, 
                                     Mtrans = mtrans)$y
  model <- depmix(x.star ~ 1, nstates = 2, data = data.frame(x.star),
                  family = poisson())
  
  u <- as.vector(getpars(fit(model, verbose=0)))
  
  if (u[7] <= u[8]){para.star[j,] = c(u[3:6], exp(u[7]), exp(u[8]))} else
  {para.star[j,] = c(u[6:3], exp(u[8]), exp(u[7]))}
}

# bootstrapped std errors
SE <- sqrt(apply(para.star, 2, var) +
             (apply(para.star, 2, mean) - para.mle)^2)[c(1,4:6)]

names(SE) <- c('seM11/M12', 'seM21/M22', 'seLam1', 'seLam2')
SE


# Example 6.18 Switching AR - Influenza Mortality -------------------------

library(MSwM)
set.seed(90210)
plot(flu, type="l")

dflu <- diff(flu) # detrended
model <- lm(dflu ~ 1)
mod <- msmFit(model, k=2, p=2, sw=rep(TRUE, 4)) # 2 regimes, AR(2)
summary(mod)

plotProb(mod, which = 3)
