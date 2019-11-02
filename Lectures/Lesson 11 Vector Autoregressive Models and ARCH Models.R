# Lesson 11: Vector Autoregressive Models/ ARCH Models

# 11.1 ARCH/GARCH Models --------------------------------------------------

library(fGarch)
x <- ts(scan("L11.1.g55sim.dat"))
plot(x, type="l")
# center x
y <- x - mean(x)
x.g <- garchFit(~garch(1,1), y, include.mean = F)
summary(x.g)


# 11.2 Vector Autoregressive models VAR(p) models -------------------------

# Trend-Stationary Model
library(vars)
library(astsa)
data("cmort", package = "astsa")
data("tempr")
data(part)

x <- cbind(cmort, tempr, part)
plot.ts(x, main="", xlab="")
fitvar1 <- VAR(x, p=1, type = "both")
summary(fitvar1)

# VAR2 model and residuals
summary(VAR(x, p=2, type = "both"))
fitvar2 <- VAR(x, p=2, type = "both")

acf(residuals(fitvar2)[,1])
acf(residuals(fitvar2)[,2])
acf(residuals(fitvar2)[,3])

# cross-correlation matrix
acf(residuals(fitvar2))

# Stationary Model
y1 <- scan("var2daty1.dat")
y2 <- scan("var2daty2.dat")
summary(VAR(cbind(y1,y2), p=2, type="both"))

# alternatively
fitvar2 <- ar.ols(cbind(y1, y2), order=2)
fitvar2$asy.se.coef
