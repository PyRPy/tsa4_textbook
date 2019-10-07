# Reading 4.8 Lagged regression models
library(astsa)
LagReg(soi, rec, L=15, M=32, threshold = 6)

# in reverse
LagReg(rec, soi, L=15, M=32, inverse = TRUE, threshold = 0.01)

# ARMAX model
fish <- ts.intersect(R=rec, RL1=lag(rec, -1), SL5=lag(soi, -5))
(u <- lm(fish[,1] ~ fish[, 2:3], na.action = NULL))
acf2(residuals(u)) # AR(1) 
sarima(fish[,1], 1, 0, 0, xreg = fish[,2:3]) # armax model
