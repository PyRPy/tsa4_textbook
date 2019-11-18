# 5.1 Long Memory ARMA and Fractional Differencing ------------------------
library(astsa)
acf(log(varve, 100))
acf(cumsum(rnorm(1000)), 100) # random walk as an analogy

# Example 5.1 Long Memory Fitting of the Glacial Varve Series
library(fracdiff)
lvarve <- log(varve) - mean(log(varve))
varve.fd <- fracdiff(lvarve, nar=0, nma=0, M=30)
varve.fd$d
varve.fd$stderror.dpq
p <- rep(1, 31)
for (k in 1:30){ p[k+1] = (k-varve.fd$d)*p[k]/(k+1)}
plot(1:30, p[-1], ylab = expression(pi(d)), xlab = "Index", type = "h")
res.df <- diffseries(log(varve), varve.fd$d)
res.arima <- resid(arima(log(varve), order=c(1,1,1)))
par(mfrow=c(2,1))
acf(res.arima, 100, xlim=c(4,97), ylim=c(-0.2, 0.2), main="")
acf(res.df, 100, xlim=c(4, 97), ylim=c(-0.2, 0.2), main="")

# Example 5.2 Long Memory Spectra for the Varve Series
series = log(varve)# specify series to be analyzed
d0 = .1# initial value of d
n.per = nextn(length(series))
n.per
m = (n.per)/2  - 1
per = Mod(fft(series-mean(series))[-1])^2# remove 0 freq and
per = per/n.per# scale the peridogram
g = 4*(sin(pi*((1:m)/n.per))^2)

