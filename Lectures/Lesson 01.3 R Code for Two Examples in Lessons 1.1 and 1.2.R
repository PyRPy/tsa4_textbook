# 1.3 R Code for Two Examples in Lessons 1.1 and 1.2

# Quakes data set ---------------------------------------------------------

x <- scan("Data/quakes.dat") # creat a folder to store data sets
x <- ts(x) #this makes sure R knows that x is a time series
plot(x, type="b") #time series plot of x with points marked as "o"
# install.packages("astsa")

library(astsa) # See note 1 below
lag1.plot(x,1) # Plots x versus lag 1 of x.
acf(x, xlim=c(1,19)) # Plots the ACF of x for lags 1 to 19

xlag1 <- lag(x,-1) # Creates a lag 1 of x variable. See note 2
y <- cbind(x,xlag1) # See note 3 below

ar1fit <- lm(y[,1]~y[,2])#Does regression, stores results object named ar1fit
summary(ar1fit) # This lists the regression results

plot(ar1fit$fit,ar1fit$residuals) #plot of residuals versus fits

acf(ar1fit$residuals, xlim=c(1,18)) # ACF of the residuals for lags 1 to 18 


# weekly cardiovascular mortality rate ------------------------------------
mort <- scan("Data/cmort.dat")

plot(mort, type="o") # plot of mortality rate
mort <- ts(mort)

mortdiff <- diff(mort,1) # creates a variable = x(t) - x(t-1)
plot(mortdiff,type="o") # plot of first differences
acf(mortdiff,xlim=c(1,24)) # plot of first differences, for 24 lags

mortdifflag1 <- lag(mortdiff,-1)
# bind first differences and lagged first differences
y <- cbind(mortdiff,mortdifflag1) 
mortdiffar1 <- lm(y[,1]~y[,2]) # AR(1) regression for first differences
summary(mortdiffar1) # regression results
acf(mortdiffar1$residuals, xlim = c(1,24)) # ACF of residuals for 24 lags. 

