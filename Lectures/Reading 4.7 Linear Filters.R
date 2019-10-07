# Reading 4.7 Linear Filters
library(astsa)
par(mfrow = c(3,1))
plot(soi)
plot(diff(soi))

k <- kernel("modified.daniell", 6)
plot(soif <- kernapply(soi, k))

dev.new()

# spectra analysis
spectrum(soif, spans=9, log="no")
abline(v=12/52, lty="dashed")
dev.new()

# frequency responses
par(mfrow=c(2,1))
w <- seq(0, 0.5, 0.01)
FRdiff <- abs(1 - exp(2i*pi*w))^2
plot(w, FRdiff, type = "l", xlab = "frequency")

u <- cos(2*pi*w) + cos(4*pi*w) + cos(6*pi*w) + cos(8*pi*w) + cos(10*pi*w)
FRma <- ((1 + cos(12*pi*w) + 2*u)/12)^2
plot(w, FRma, type = "l", xlab = "frequency")
