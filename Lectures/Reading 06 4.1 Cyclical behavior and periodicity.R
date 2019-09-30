# 4.1 Cyclical behavior and periodicity
library('astsa')
# Example 4.1 periodic series ---------------------------------------------
x1 <- 2*cos(2*pi*1:100*6/100) + 3*sin(2*pi*1:100*6/100)
x2 <- 4*cos(2*pi*1:100*10/100) + 5*sin(2*pi*1:100*10/100)
x3 <- 6*cos(2*pi*1:100*40/100) + 7*sin(2*pi*1:100*40/100)
x <- x1 + x2 + x3

par(mfrow = c(2,2))
tsplot(x1, ylim=c(-10,10), main=expression(omega==6/100~~~A^2==13) )
tsplot(x2, ylim=c(-10,10), main=expression(omega==10/100~~~A^2==41) )
tsplot(x3, ylim=c(-10,10), main=expression(omega==40/100~~~A^2==85) )
tsplot(x,  ylim=c(-16,16), main="sum", font.main=1)
dev.off()


# Example 4.2 Estimation and the Periodogram ------------------------------
P <- Mod(2*fft(x)/100)^2
Fr <- 0:99/100
plot(Fr, P, type="o", xlab="frequency", ylab="scaled periodogram", 
     panel.first=grid(lty=1), ylim=c(0,90) )
abline(v=.5, lty=2, col=4)
abline(v=c(.1,.3,.7,.9), lty=1, col='lightgray')


# Example 4.3 Star Magnitude ----------------------------------------------
## Fig. 4.3. Star magnitudes and part of the corresponding periodogram.
n <- length(star)
plot(star, ylab="star magnitude", xlab="day", type='n' )
grid(lty=1)
lines(star)

Per <- Mod(fft(star-mean(star)))^2/n
Freq <- (1:n -1)/n
plot(Freq[1:50], Per[1:50], type='n', ylab="Periodogram", xlab="Frequency")
grid()
lines(Freq[1:50], Per[1:50], type='h', lwd=3)
u <- which.max(Per[1:50])         # 22  freq=21/600=.035 cycles/day
uu <- which.max(Per[1:50][-u])    # 25  freq=25/600=.041 cycles/day 
1/Freq[22]; 1/Freq[26]           # period = days/cycle
text(.05, 7000, "24 day cycle"); text(.027, 9000, "29 day cycle")
dev.off()
