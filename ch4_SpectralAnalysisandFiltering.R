library('astsa')
source('grid.r')

##############
## Example 4.1 A Periodic Series 
x1 = 2*cos(2*pi*1:100*6/100) + 3*sin(2*pi*1:100*6/100)
x2 = 4*cos(2*pi*1:100*10/100) + 5*sin(2*pi*1:100*10/100)
x3 = 6*cos(2*pi*1:100*40/100) + 7*sin(2*pi*1:100*40/100)
x = x1 + x2 + x3

par(mfrow = c(2,2), mar=c(2,2,1,0)+.5, mgp=c(1.6,.6,0))
tsplot(x1, ylim=c(-10,10), main=expression(omega==6/100~~~A^2==13) )
tsplot(x2, ylim=c(-10,10), main=expression(omega==10/100~~~A^2==41) )
tsplot(x3, ylim=c(-10,10), main=expression(omega==40/100~~~A^2==85) )
tsplot(x,  ylim=c(-16,16), main="sum", font.main=1)
dev.off()

##############
## Example 4.2 Estimation and the Periodogram
par(mar=c(2,2.5,1,0)+.5, mgp=c(1.6,.6,0))
P = Mod(2*fft(x)/100)^2
Fr = 0:99/100
plot(Fr, P, type="o", xlab="frequency", ylab="scaled periodogram", 
     panel.first=grid(lty=1), ylim=c(0,90) )
abline(v=.5, lty=2, col=4)
abline(v=c(.1,.3,.7,.9), lty=1, col='lightgray')
dev.off()


######################
## Example 4.3 Star Magnitude
## Fig. 4.3. Star magnitudes and part of the corresponding periodogram.
n = length(star)
layout(matrix(c(1,2), ncol = 1), height=c(1.25,1))
par( mar=c(2,2,0,0)+.5, mgp=c(1.6,.6,0))
plot(star, ylab="star magnitude", xlab="day", type='n' )
grid(lty=1)
lines(star)
Per = Mod(fft(star-mean(star)))^2/n
Freq = (1:n -1)/n
plot(Freq[1:50], Per[1:50], type='n', ylab="Periodogram", xlab="Frequency")
grid()
lines(Freq[1:50], Per[1:50], type='h', lwd=3)
#u = which.max(Per[1:50])         # 22  freq=21/600=.035 cycles/day
#uu = which.max(Per[1:50][-u])    # 25  freq=25/600=.041 cycles/day 
#1/Freq[22]; 1/Freq[26]           # period = days/cycle
text(.05, 7000, "24 day cycle"); text(.027, 9000, "29 day cycle")
dev.off()

## page-178
###########################
## Example 4.7 A Second-Order Autoregressive Series
## Fig. 4.4. Theoretical spectra of white noise (top), a first-order moving average (middle), 
## and a second-order autoregressive process (bottom). 
par(mfrow=c(3,1), mar=c(3,3,1.5,1), mgp=c(1.6,.6,0), cex.main=1.1)
arma.spec(log="no", main="White Noise")
arma.spec(ma=.5, log="no", main="Moving Average")
arma.spec(ar=c(1,-.9), log="no", main="Autoregression")
dev.off()


###########################
### spectral plots
## Example 4.13 Periodogram of SOI and Recruitment Series
par(mfrow=c(2,1), mar=c(3.5,3,2.5,1),  mgp=c(1.5,.6,0), oma=rep(0,4), font.main=1)

soi.per = mvspec(soi, log="no", type='n')
grid(lty=1)
par(new=TRUE)
mvspec(soi, log="no") 
abline(v=1/4, lty=2, col=8)
mtext('1/4',side=1, line=0, at=.25, cex=.75)

rec.per = mvspec(rec, log="no", type='n')
grid(lty=1)
par(new=TRUE)
mvspec(rec, log="no") 
abline(v=1/4, lty=2, col=8)
mtext('1/4',side=1, line=0, at=.25, cex=.75)
dev.off()

###########################
## Fig. 4.6. A small section (near the peak) of the AR(2) spectrum shown in Figure 4.4 .
par(mar=c(2,2,0,0)+.5, mgp=c(1.5,.6,0))
u=arma.spec(ar=c(1,-.9), xlim=c(.15,.151), ylim=c(10, 100), n.freq=100000, col='white')
grid(lty=1, equilogs = FALSE )
lines(u$freq,u$spec, lwd=2)
dev.off()


###########################
## Example 4.14 Averaged Periodogram for SOI and Recruitment 
par(mfrow=c(2,1), mar=c(3.5,3,2.5,1),  mgp=c(1.5,.6,0), oma=rep(0,4), font.main=1)
k = kernel("daniell", 4)
soi.ave = mvspec(soi, k, log="no", type='n')
grid(lty=1, col=gray(.9))
par(new=TRUE)
mvspec(soi, k, log="no")
abline(v=.25, lty=2, col=8)
mtext('1/4',side=1, line=0, at=.25, cex=.75)

rec.ave = mvspec(rec, k, log="no", type='n')
grid(lty=1, col=gray(.9))
par(new=TRUE)
mvspec(rec, k, log="no")
abline(v=.25, lty=2, col=8)
mtext('1/4',side=1, line=0, at=.25, cex=.75)
dev.off()

##########
## Fig. 4.8. Figure 4.7 with the average periodogram ordinates plotted on a log10 scale.
par(mfrow=c(2,1), mar=c(3.5,3,2.5,1),  mgp=c(1.5,.6,0), oma=rep(0,4), font.main=1)
k = kernel("daniell", 4)
soi.ave = mvspec(soi, k, type='n')
grid(lty=1)
par(new=TRUE)
mvspec(soi, k)
abline(v=.25, lty=2, col=8)
mtext('1/4',side=1, line=0, at=.25, cex=.75)

rec.ave = mvspec(rec, k, type='n')
grid(lty=1)
par(new=TRUE)
mvspec(rec, k)
abline(v=.25, lty=2, col=8)
mtext('1/4',side=1, line=0, at=.25, cex=.75)
dev.off()

## website as reference
# https://www.stat.pitt.edu/stoffer/tsa4/Rexamples.htm
soi.ave = mvspec(soi, kernel('daniell',4), log='no')
abline(v = c(.25,1,2,3), lty=2)
soi.ave$bandwidth      # = 0.225
df  = soi.ave$df       # df = 16.9875  
U   = qchisq(.025, df) # U = 7.555916
L   = qchisq(.975, df) # L = 30.17425
soi.ave$spec[10]       # 0.0495202
soi.ave$spec[40]       # 0.1190800
# intervals
df*soi.ave$spec[10]/L  # 0.0278789
df*soi.ave$spec[10]/U  # 0.1113333
df*soi.ave$spec[40]/L  # 0.0670396
df*soi.ave$spec[40]/U  # 0.2677201

# Repeat above commands with soi replaced by rec, for example:
rec.ave = mvspec(rec, k, log="no")
abline(v=c(.25,1,2,3), lty=2)

# Example 4.15 Harmonics
t = seq(0, 1, by=1/200)  # WARNING: using t is bad pRactice because it's reserved- but let's be bad
amps = c(1, .5, .4, .3, .2, .1)
x = matrix(0, 201, 6)
for (j in 1:6) x[,j] = amps[j]*sin(2*pi*t*2*j)
x = ts(cbind(x, rowSums(x)), start=0, deltat=1/200)               
ts.plot(x, lty=c(1:6, 1), lwd=c(rep(1,6), 2), ylab="Sinusoids")
names = c("Fundamental","2nd Harmonic","3rd Harmonic","4th Harmonic","5th Harmonic", 
          "6th Harmonic","Formed Signal")
legend("topright", names, lty=c(1:6, 1), lwd=c(rep(1,6), 2))
rm(t)                    # Redemption
dev.off()
# code in the github, a clearer version than that in the website
par(mar=c(2.5,2.5,0,0)+.5, mgp=c(1.6,.6,0))
t=seq(0,1,by=1/200)
x1 =  ts(sin(2*pi*2*t),start=0,deltat=1/200)
x2 = ts(.5*sin(2*pi*t*4),start=0,deltat=1/200)
x3 = ts(.4*sin(2*pi*t*6),start=0,deltat=1/200)
x4 = ts(.3*sin(2*pi*t*8),start=0,deltat=1/200)
x5 = ts(.2*sin(2*pi*t*10),start=0,deltat=1/200)
x6 = ts(.1*sin(2*pi*t*12),start=0,deltat=1/200)
xsum = x1+x2+x3+x4+x5+x6
dcyan = rgb(0,.6,.6)
sgreen =  rgb(0, .7, 0)
ts.plot(x1,x2,x3,x4,x5,x6,xsum, lty=c(1,5,2,5,2,5,1), lwd=c(rep(1,6),2), col=c(4,sgreen,2,4,dcyan,6,1), ylab="Sinusoids")
names=c("Fundamental","2nd Harmonic","3rd Harmonic","4th Harmonic", "5th Harmonic", "6th Harmonic", "Formed Signal")
legend("topright", names, lty=c(1,5,2,5,2,5,1),  col=c(4,sgreen,2,4,dcyan,6,1), lwd=c(rep(1,6),2), cex=.8)
dev.off()

# Example 4.17 The Effect of Tapering the SOI Series
# page-203
# The solid line shows the result with full tapering. Notice that the tapered spectrum 
# does a better job in separating the yearly cycle (?? = 1) and the El Niño cycle (?? = 1/4).
s0  = mvspec(soi, spans=c(7,7), plot=FALSE)            # no taper
s50 = mvspec(soi, spans=c(7,7), taper=.5, plot=FALSE)  # full taper
plot(s50$freq, s50$spec, log="y", type="l", ylab="spectrum", xlab="frequency") 
lines(s0$freq, s0$spec, lty=2) 
# github version
par(mfrow=c(2,1), mar=c(3.5,3,2.5,1),  mgp=c(1.5,.6,0), oma=rep(0,4), font.main=1)
k = kernel("modified.daniell", c(3,3))
soi.ave = mvspec(soi, k, log="no", taper=.1,  type='n')
grid(lty=1); par(new=TRUE)
mvspec(soi, k, taper=.1, log="no")
abline(v=.25, lty=2, col=8)
mtext('1/4',side=1, line=0, at=.25, cex=.75)
rec.ave = mvspec(rec, k, log="no", taper=.1,  type='n')
grid(lty=1); par(new=TRUE)
mvspec(rec, k, taper=.1, log="no")
abline(v=.25, lty=2, col=8)
mtext('1/4',side=1, line=0, at=.25, cex=.75)
dev.off()

# Example 4.18 Autoregressive Spectral Estimator for SOI
spaic = spec.ar(soi, log="no", ylim=c(0,.3))             # min AIC spec, order = 15
text(frequency(soi)*1/52, .07, substitute(omega==1/52))  # El Nino Cycle
text(frequency(soi)*1/12, .27, substitute(omega==1/12))  # Yearly Cycle
(soi.ar = ar(soi, order.max=30))                         # estimates and AICs
dev.new() 
plot(1:30, soi.ar$aic[-1], type="o")                     # plot AICs


# Better comparison of pseudo-ICs 
n = length(soi)
c() -> AIC -> AICc -> BIC
for (k in 1:30){
  fit = ar(soi, order=k, aic=FALSE) 
  sigma2  = fit$var.pred               
  BIC[k]  = log(sigma2) + (k*log(n)/n)
  AICc[k] = log(sigma2) + ((n+k)/(n-k-2))
  AIC[k]  = log(sigma2) + ((n+2*k)/n) 
}

dev.new()
IC = cbind(AIC, BIC+1)
ts.plot(IC, type="o", xlab="p", ylab="AIC / BIC")
grid()
text(15.2, -1.48, "AIC")
text(15,   -1.35, "BIC")

# github version
par(mar=c(2.75,2.5,1,1), mgp=c(1.5,.6,0))
n = length(soi)
AIC = rep(0, 30) -> AICc -> BIC
for (k in 1:30){
  fit = ar(soi, order=k, aic=FALSE)
  sigma2 = fit$var.pred
  BIC[k] = log(sigma2) + (k*log(n)/n)
  AICc[k] = log(sigma2) + ((n+k)/(n-k-2))
  AIC[k] = log(sigma2) + ((n+2*k)/n)
}
IC = cbind(AIC, BIC+1)
ts.plot(IC,  type='n',  xlab="p", ylab="AIC / BIC")
grid(lty=1); par(new=TRUE)
ts.plot(IC, type="o", xlab='', ylab='')
text(15.2, -1.48, "AIC")
text(15, -1.35, "BIC")
dev.off()

# Example 4.19 Three-Point Moving Average
# Example 4.21 Coherence Between SOI and Recruitment
sr = mvspec(cbind(soi,rec), kernel("daniell",9), plot.type="coh", plot=FALSE)
sr$df                     # df = 35.8625
f = qf(.999, 2, sr$df-2)  # f = 8.529792
C = f/(18+f)              # C = 0.3188779
abline(h = C)

# github version
par(mar=c(2.75,2.5,1,1), mgp=c(1.5,.6,0), font.main=1, cex.main=1.1)
sr = mvspec(cbind(soi,rec), kernel("daniell",9), plot=FALSE) 
f = qf(.999, 2, sr$df-2)  
C = f/(18+f)  
plot(sr, plot.type = "coh", ci.lty = 2, panel.first=grid(lty=1), main='SOI & Recruitment')
abline(h = C)
dev.off()

# Example 4.22 First Difference and Moving Average Filters
par(mfrow=c(3,1))
tsplot(soi)                         # plot data
tsplot(diff(soi))                   # plot first difference
k = kernel("modified.daniell", 6)   # filter weights
tsplot(soif <- kernapply(soi, k))   # plot 12 month filter
dev.new()
spectrum(soif, spans=9, log="no") # spectral analysis (not shown)
abline(v=12/52, lty="dashed")
dev.new()
##-- frequency responses --##
par(mfrow=c(2,1), mar=c(3,3,1,1), mgp=c(1.6,.6,0))
w = seq(0, .5, by=.01)
FRdiff = abs(1-exp(2i*pi*w))^2
plot(w, FRdiff, type='l', xlab='frequency')
u = cos(2*pi*w)+cos(4*pi*w)+cos(6*pi*w)+cos(8*pi*w)+cos(10*pi*w)
FRma = ((1 + cos(12*pi*w) + 2*u)/12)^2
plot(w, FRma, type='l', xlab='frequency')

# github version
par(mfrow=c(3,1), mar=c(3,2,1.5,1), mgp=c(1.6,.6,0))
plot(soi, type='n', ylab='') # plot data
grid(lty=1)
lines(soi)
mtext(side=3, 'SOI')
plot(diff(soi), type='n', ylab='') # plot first difference
grid(lty=1)
lines(diff(soi))
mtext(side=3, 'First Difference')
k = kernel("modified.daniell", 6) # filter weights
plot(soif <- kernapply(soi, k), type='n', ylab='') # plot 12 month filter
grid(lty=1)
lines(soif)
mtext(side=3, 'Seasonal Moving Average')
dev.off()

# Example 4.24 Lagged Regression for SOI and Recruitment 
LagReg(soi, rec, L=15, M=32, threshold=6)
LagReg(rec, soi, L=15, M=32, inverse=TRUE, threshold=.01)
# armax model
fish = ts.intersect(R=rec, RL1=lag(rec,-1), SL5=lag(soi,-5))
(u = lm(fish[,1]~fish[,2:3], na.action=NULL))
acf2(resid(u))       # suggests ar1
sarima(fish[,1], 1, 0, 0, xreg=fish[,2:3]) 

# lagreg1 and lagreg2
u1 = LagReg(soi, rec, L=15, M=32, threshold=6)
dev.off()
u2 = LagReg(rec, soi, L=15, M=32, inverse=TRUE, threshold=.01)
dev.off()

par(mfrow=c(2,1), mar=c(2.5,2.5,1,.5), mgp=c(1.4,.4,0), cex=.9)
plot(u1$betas, type = "h", xlab = "s", ylab = "beta(s)", panel.first=grid(lty=1))
abline(h=0)
mtext(side=3,'Input: SOI' )
plot(u2$betas, type = "h", xlab = "s", ylab = "beta(s)", panel.first=grid(lty=1))
abline(h=0)
mtext(side=3,'Input: Recruitment' )
dev.off()

# Example 4.25 Estimating the El Niño Signal via Optimal Filters
SigExtract(soi, L=9, M=64, max.freq=.05) 

# 4.10 Spectral Analysis of Multidimensional Series
per = abs(fft(soiltemp-mean(soiltemp))/sqrt(64*36))^2       
per2 = cbind(per[1:32,18:2], per[1:32,1:18])   # this and line below is just rearranging
per3 = rbind(per2[32:2,], per2)                # results to get 0 frequency in the middle

par(mar=c(1,2.5,0,0)+.1)
persp(-31:31/64, -17:17/36, per3, phi=30, theta=30, expand=.6, ticktype="detailed", xlab="cycles/row", 
      ylab="cycles/column", zlab="Periodogram Ordinate")

## SigExtract(soi, L=9, M=64, max.freq=.05)
L = 9;  M = 64; max.freq=.05
series = ts(soi, frequency = 1)  
spectra = stats::spec.pgram(series, spans=L, plot = FALSE)
A <- function(nu) {
  qwe = ifelse((nu > .01 && nu < max.freq), 1, 0) 
  qwe  # Sets A(nu) to be qwe
}
N = 2*length(spectra$freq)  # This will be T'.
sampled.indices = (N/M)*(1:(M/2))  # These are the indices of the frequencies we want
fr.N = spectra$freq
fr.M = fr.N[sampled.indices]  # These will be the frequencies we want
spec.N = spectra$spec
spec.M = spec.N[sampled.indices] # Power at these frequencies
A.desired = vector(length = length(fr.M))
for(k in 1:length(fr.M)) A.desired[k] = A(fr.M[k])
# Invert A.desired, by discretizing the defining integral, to get the coefficients a:
delta = 1/M
Omega = seq(from = 1/M, to = .5, length = M/2)
aa = function(s) 2*delta*sum(exp(2i*pi*Omega*s)*A.desired)
S = ((-M/2+1):(M/2-1))
a = vector(length = length(S))
for(k in 1:length(S)) a[k] = aa(S[k])
a = Re(a)  # The filter coefficients 
# Apply a cosine taper
h = .5*(1+cos(2*pi*S/length(S)))
a = a*h    # Comment out this line, to see the effect of NOT tapering

# Compute the realized frequency response function, and the filtered series:
A.M = function(nu) Re(sum(exp(-2i*pi*nu*S)*a))
A.attained = vector(length = length(fr.N))
A.theoretical = vector(length = length(fr.N))
for(k in 1:length(fr.N)) {
  A.attained[k] = A.M(fr.N[k]) # The attained freq. resp.
  A.theoretical[k] = A(fr.N[k])
}
series.filt = stats::filter(series, a, sides = 2) # The filtered series

###########

par(mfrow=c(2,1), mar=c(2.5,2.5,1,.5), mgp=c(1.25,.6,0), cex.lab=.8, cex.axis=.8, font.main=1, cex.main=.9)
plot.ts(series, type='n') 
grid(lty=1)
lines(series)
mtext(side=3, "Original series")
plot.ts(series.filt, type='n') 
grid(lty=1)
lines(series.filt)
mtext(side=3, "Filtered series")
dev.off()

###########
par(mfrow=c(2,1), mar=c(2.5,2.5,1,.5), mgp=c(1.25,.6,0), cex.lab=.8, cex.axis=.8, font.main=1, cex.main=.9)
plot(S, a, xlab = "s", ylab = "a(s)", main = "Filter coefficients", panel.first=grid(lty=1))
plot(fr.N, A.theoretical, type = "l", lty = 6, col=4, xlab = "freq", ylab = "freq. response", 
     main = "Desired and attained frequency response functions", panel.first=grid(lty=1))
lines(fr.N, A.attained, lty = 1, col = 2)
dev.off()


###############################
per = abs(fft(soiltemp-mean(soiltemp))/sqrt(64*36))^2       
per2 = cbind(per[1:32,18:2], per[1:32,1:18])   
per3 = rbind(per2[32:2,],per2)
par(mar=c(1,1,0,0)+.5, cex.axis=.7)
persp(-31:31/64, -17:17/36, per3, phi=30, theta=30, expand=.6, ticktype="detailed", xlab="cycles/row",  ylab="cycles/column", zlab="Periodogram Ordinate")
dev.off()


#######################
par(mar=c(2,2,1,0)+.5, mgp=c(1.6,.6,0))
plot(sunspotz, type='n')
grid(lty=1)
lines(sunspotz)
dev.off()
