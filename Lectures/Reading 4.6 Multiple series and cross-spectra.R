# 4.6 Multiple series and cross-spectra
library(astsa)
sr = mvspec(cbind(soi, rec), kernel("daniell", 9), plot=FALSE)
sr$df
f = qf(0.999, 2, sr$df-2)
C = f/(18 + f)
plot(sr, plot.type="coh", ci.lty=2)
abline(h = C)