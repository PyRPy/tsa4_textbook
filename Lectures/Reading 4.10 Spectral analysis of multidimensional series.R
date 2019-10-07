# 4.10 Spectral analysis of multidimensional series
# soil surface temperature
library(astsa)
per <- Mod(fft(soiltemp - mean(soiltemp))/sqrt(64*36))^2
per2 <- cbind(per[1:32, 18:2], per[1:32, 1:18])
per3 <- rbind(per2[32:2, ], per2)
persp(-31:31/64, -17:17/36, per3, phi=30, theta=30, expand = .6,
      ticktype = "detailed", xlab = "cyles/row", ylab = "cycles/column",
      zlab = "periodogram ordinate")
