# Chapter 3.9 Multiplicative Seasonal ARIMA Models

# 3.9 Multiplicative Seasonal ARIMA Models --------------------------------
## Example3.46 A Seasonal AR Series 
set.seed(666)
phi <- c(rep(0,11),.9)
sAR <-  arima.sim(list(order=c(12,0,0), ar=phi), n=37)
sAR <-  ts(sAR, freq=12)
layout(matrix(c(1,1,2, 1,1,3), nc=2))
plot(sAR, axes=FALSE, col='#808080', main='seasonal AR(1)', xlab="year", type='c')
abline(v=1:4, lty=2, col=gray(.6))
abline(h=seq(-4,2,2), col=gray(.9), lty=1)
Months <- c("J","F","M","A","M","J","J","A","S","O","N","D")
points(sAR, pch=Months, cex=1.35, font=4, col=1:4) 
axis(1,1:4) 
axis(2)
box()

## lower part of the plot
ACF <- ARMAacf(ar=phi, ma=0, 100)[-1]  # [-1] removes 0 lag
PACF <- ARMAacf(ar=phi, ma=0, 100, pacf=TRUE)
plot(ACF, type="h", xlab="LAG", ylim=c(-.1,1), axes=FALSE);
segments(0,0,0,1)
axis(1, seq(0,100,by=12))
axis(2)
box()
abline(h=0)
plot(PACF, type="h", xlab="LAG", ylim=c(-.1,1), axes=FALSE);
axis(1, seq(0,100,by=12))
axis(2)
box()
abline(h=0)
dev.off()
