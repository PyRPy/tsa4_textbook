# 4.4 Nonparametric spectral estimation
library(astsa)
# Fig. 4.6. A small section (near the peak) of the AR(2) 
# spectrum shown in Figure 4.4 .
u <- arma.spec(ar=c(1,-.9), 
               xlim=c(.15,.151), 
               ylim=c(10, 100), 
               n.freq=100000, 
               col='white')
grid(lty=1, equilogs = FALSE )
lines(u$freq,u$spec, lwd=2)
dev.off()


# Example 4.14 Averaged Periodogram for SOI and Recruitment  --------------
par(mfrow=c(2,1))
k <- kernel("daniell", 4)
soi.ave <- mvspec(soi, k, log="no", type='n')
grid(lty=1, col=gray(.9))
par(new=TRUE)
mvspec(soi, k, log="no")
abline(v=.25, lty=2, col=8)
mtext('1/4',side=1, line=0, at=.25, cex=.75)

rec.ave <- mvspec(rec, k, log="no", type='n')
grid(lty=1, col=gray(.9))
par(new=TRUE)
mvspec(rec, k, log="no")
abline(v=.25, lty=2, col=8)
mtext('1/4',side=1, line=0, at=.25, cex=.75)
dev.off()

# Fig. 4.8. Figure 4.7 with the average periodogram ordinates plotted on 
# a log10 scale.
par(mfrow=c(2,1))
k <- kernel("daniell", 4)
soi.ave <- mvspec(soi, k, type='n')
grid(lty=1)
par(new=TRUE)
mvspec(soi, k)
abline(v=.25, lty=2, col=8)
mtext('1/4',side=1, line=0, at=.25, cex=.75)

rec.ave <- mvspec(rec, k, type='n')
grid(lty=1)
par(new=TRUE)
mvspec(rec, k)
abline(v=.25, lty=2, col=8)
mtext('1/4',side=1, line=0, at=.25, cex=.75)
dev.off()

## website as reference
# https://www.stat.pitt.edu/stoffer/tsa4/Rexamples.htm
soi.ave <- mvspec(soi, kernel('daniell',4), log='no')
abline(v = c(.25,1,2,3), lty=2)
soi.ave$bandwidth      # = 0.225
df  <-  soi.ave$df       # df = 16.9875  
U   <-  qchisq(.025, df) # U = 7.555916
L   <-  qchisq(.975, df) # L = 30.17425
soi.ave$spec[10]       # 0.0495202
soi.ave$spec[40]       # 0.1190800
# intervals
df*soi.ave$spec[10]/L  # 0.0278789
df*soi.ave$spec[10]/U  # 0.1113333
df*soi.ave$spec[40]/L  # 0.0670396
df*soi.ave$spec[40]/U  # 0.2677201

# Repeat above commands with soi replaced by rec, for example:
rec.ave <- mvspec(rec, k, log="no")
abline(v=c(.25,1,2,3), lty=2)


# Example 4.15 Harmonics --------------------------------------------------
t <- seq(0, 1, by=1/200)  
# WARNING: using t is bad pRactice because it's reserved- but let's be bad
amps <- c(1, .5, .4, .3, .2, .1)
x <- matrix(0, 201, 6)
x
for (j in 1:6) x[,j] = amps[j]*sin(2*pi*t*2*j)
x <- ts(cbind(x, rowSums(x)), start=0, deltat=1/200)               
ts.plot(x, lty=c(1:6, 1), lwd=c(rep(1,6), 2), ylab="Sinusoids")
names = c("Fundamental","2nd Harmonic","3rd Harmonic","4th Harmonic",
          "5th Harmonic", "6th Harmonic","Formed Signal")
legend("topright", names, lty=c(1:6, 1), lwd=c(rep(1,6), 2))
rm(t)                    # Redemption
dev.off()


# Example 4.16 Smoothed Periodogram for SOI and Recruitment ---------------
kernel("modified.daniell", c(3,3))
kernel("modified.daniell", c(1,1))
k <- kernel("modified.daniell", c(3,3))
soi.smo <- mvspec(soi, kernel = k, taper = 0.1, log="no")
abline(v = c(0.25, 1), lty=2)

# for rec data
rec.smo <- mvspec(rec, kernel = k, taper = 0.1, log="no")
abline(v = c(0.25, 1, 2, 3), lty=2)

df <- soi.smo$df
soi.smo$bandwidth


# Example 4.17 The Effect of Tapering the SOI Series ----------------------
# page-203
# The solid line shows the result with full tapering. Notice that the 
# tapered spectrum does a better job in separating the yearly cycle and the 
# El Niño cycle (1/4).
s0  <- mvspec(soi, spans=c(7,7), plot=FALSE)            # no taper
s50 <- mvspec(soi, spans=c(7,7), taper=.5, plot=FALSE)  # full taper
plot(s50$freq, s50$spec, log="y", type="l", 
     ylab="spectrum", xlab="frequency") 
lines(s0$freq, s0$spec, lty=2, col="blue") 
