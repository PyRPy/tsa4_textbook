
# 5.4 Threshold Models --------------------------------------------------

library(astsa)
# Example 5.7 Threshold Modeling of the Influenza Series
# Plot data with month initials as points
plot(flu, type="c")
Months <- c("J", "F", "M","A","M","J","J","A","S","O","N","D")
points(flu, pch=Months, cex=0.8, font=2)

# start analysis
dflu <- diff(flu)
lag1.plot(dflu, corr=FALSE) # scatterplot with lowess fit
thrsh <- 0.05
Z <- ts.intersect(dflu, lag(dflu, -1),
                        lag(dflu, -2),
                        lag(dflu, -3),
                        lag(dflu, -4))
head(Z)
ind1 <- ifelse(Z[,2] < thrsh, 1, NA) # indicator < thrsh
ind2 <- ifelse(Z[,2] < thrsh, NA, 1) # indicator >= thrsh

X1 <- Z[,1]*ind1
X2 <- Z[,1]*ind2

summary(fit1 <- lm(X1 ~ Z[, 2:5])) # case 1
summary(fit2 <- lm(X2 ~ Z[, 2:5])) # case 2

D <- cbind(rep(1, nrow(Z)), Z[, 2:5]) # design matrix
p1 <- D%*%coef(fit1) # get predictions
p2 <- D%*%coef(fit2)

prd <- ifelse(Z[,2] < thrsh, p1, p2)
plot(dflu, ylim = c(-0.5, 0.5), type="p", pch=3)
lines(prd)

prde1 <- sqrt(sum(resid(fit1)^2)/df.residual(fit1))
prde2 <- sqrt(sum(resid(fit2)^2)/df.residual(fit2))

prde <- ifelse(Z[,2] < thrsh, prde1, prde2)

tx <- time(dflu)[-(1:4)]
xx <- c(tx, rev(tx))
yy <- c(prd - 2*prde, rev(prd + 2*prde))

polygon(xx, yy, border = 8, col=gray(0.6, alpha = 0.25))
abline(h=0.05, col=4, lty=6)

# another package in R tsDyn
library(tsDyn)
# vignette("tsDyn")
(u <- setar(dflu, m=4, thDelay = 0, th=0.05))
(u <- setar(dflu, m=4, thDelay = 0)) # let program fit threshold (=.036)
BIC(u) #  m=3 works well too
AIC(u)
plot(u) # many plots
