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
