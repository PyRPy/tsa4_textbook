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

# Example 6.18 Switching AR - Influenza Mortality
library(MSwM)
set.seed(90210)
plot(flu, type="l")

dflu <- diff(flu) # detrended
model <- lm(dflu ~ 1)
mod <- msmFit(model, k=2, p=2, sw=rep(TRUE, 4)) # 2 regimes, AR(2)
summary(mod)

plotProb(mod, which = 3)
