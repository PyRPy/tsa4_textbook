# 4.3 Periodogram and Discrete Fourier Transform
library(astsa)
# Example 4.13 Periodogram of SOI and Recruitment Series ------------------
par(mfrow=c(2,1))
soi.per <- mvspec(soi, log="no", type='n')
grid(lty=1)
par(new=TRUE)
mvspec(soi, log="no") 
abline(v=1/4, lty=2, col=8)
mtext('1/4',side=1, line=0, at=.25, cex=.75)

rec.per <- mvspec(rec, log="no", type='n')
grid(lty=1)
par(new=TRUE)
mvspec(rec, log="no") 
abline(v=1/4, lty=2, col=8)
mtext('1/4',side=1, line=0, at=.25, cex=.75)
dev.off()

# univariate example
plot(co2)   # co2 is an R data set
mvspec(co2, spans=c(5,5), taper=.5)

# multivariate example
ts.plot(mdeaths, fdeaths, col=1:2)   # an R data set, male/female monthly deaths ...
dog = mvspec(cbind(mdeaths,fdeaths), spans=c(3,3), taper=.1)
dog$fxx        # look a spectral matrix estimates
dog$bandwidth  # bandwidth with time unit = year
dog$bandwidth/frequency(mdeaths)  # ... with time unit = month
plot(dog, plot.type="coherency")  # plot of squared coherency
