# Reading 4.2 Spectral density
library(astsa)
# Example 4.7 A Second-Order Autoregressive Series ------------------------
# Fig. 4.4. Theoretical spectra of white noise (top), a first-order moving 
# average (middle), and a second-order autoregressive process (bottom). 
par(mfrow=c(3,1))
arma.spec(log="no", main="White Noise")
arma.spec(ma=.5, log="no", main="Moving Average")
arma.spec(ar=c(1,-.9), log="no", main="Autoregression")
dev.off()

