# Lesson 12: Spectral Analysis

# 12.1: Estimating the Spectral Density -----------------------------------
library(astsa)

## Smoothing Method (Nonparametric Estimation of the Spectral Density)
kernel("daniell", 2)
kernel("modified.daniell", 2)

## Bandwidth
## soi
x <- soi
plot(soi, type="l")
k <- kernel("daniell", 4)
mvspec(x, k, log="no")

# x.bw <- (2*4 + 1)/453 # not corrected
# x.bw

mvspec(x, spans = 9, log="no")
mvspec(x, spans = c(9,9), log="no")


## recruitment
y <- rec
plot(y, type="l")
k2 <- kernel("daniell", c(4,4))
mvspec(y, k2, log="no")
specvalues <- mvspec(y, k2, log="no")

specvalues$freq[which.max(specvalues$spec)]
str(specvalues)
abline(v = 0.275, lty="dotted")
abline(v = 1.0, lty="dotted")

# somehow the spectrum is in different scale/data different

mvspec(y, spans = 73, log="no") # too much smoothing


# Parametric Estimation of the Spectral Density ---------------------------
spec.ar(y, log="no")
specvalues <- spec.ar(y, log="no")
specvalues$method
# "AR (13) spectrum "