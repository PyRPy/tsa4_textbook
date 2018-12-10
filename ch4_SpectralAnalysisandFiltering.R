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