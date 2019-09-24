# 2.3 Smoothing in the Time Series Context
library(astsa)
# Example 2.11 Moving Average Smoother ------------------------------------

# SOI data
wgts <-  c(.5, rep(1,11), .5)/12
soif <- filter(soi, sides=2, filter=wgts)
plot(soi)
lines(soif, lwd=2, col=4)

# removes (filters out) the obvious annual temperaturecycle and helps 
# emphasize the El Niño cycle, although choppy


# Example 2.12 Kernel Smoothing -------------------------------------------
# smoother than ma
plot(soi)
lines(ksmooth(time(soi), soi, "normal", bandwidth = 1), lwd=2, col=4)
par(fig=c(0.65, 1, 0.65, 1), new=TRUE)
gauss <-  function(x){1/sqrt(2*pi)*exp(-(x^2)/2)}
x <- seq(from=-3, to = 3, by=0.001)
plot(x, gauss(x), type = "l", ylim = c(-0.2, 0.45), xaxt="n", yaxt="n",
     ann=FALSE)


# Example 2.13 Lowess -----------------------------------------------------

plot(soi)
# El Nino cycle
lines(lowess(soi, f=0.05), lwd=2, col=4)
# trend with default span
lines(lowess(soi), lty=2, lwd=2, col=2)


# Example 2.14 Smoothing Splines ------------------------------------------
# ?? is seen as a trade-off between linear regression(completely smooth) 
# and the data itself (no smoothness). The larger the value of ??,the 
# smoother the fit

plot(soi)
# El Nino cycle
lines(smooth.spline(time(soi), soi, spar = 0.5), lwd=2, col=4)
# trend
lines(smooth.spline(time(soi), soi, spar = 1), lty=2, col=2)


# Example 2.15 Smoothing One Series as a Function of Another --------------

plot(tempr, cmort, xlab="temperature", ylab="mortality")
lines(lowess(tempr, cmort), col="blue")
