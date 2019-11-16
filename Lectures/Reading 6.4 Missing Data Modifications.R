
# 6.4 Missing Data Modifications ------------------------------------------
library(astsa)
# Example 6.9 Longitudinal Biomedical Data

y = cbind(WBC, PLT, HCT)
plot(y) # can plot three ts

num = nrow(y)

# make array of obs matrices
A = array(0, dim=c(3,3,num))
A
for(k in 1:num) { if (y[k,1] > 0) A[,,k]= diag(1,3) }
A

# Initial values
mu0 = matrix(0, 3, 1)
mu0
Sigma0 = diag(c(.1, .1, 1), 3)
Sigma0

Phi = diag(1, 3)
Phi
cQ = diag(c(.1, .1, 1), 3)
cR = diag(c(.1, .1, 1), 3)

# EM procedure - some output previously shown
(em = EM1(num, y, A, mu0, Sigma0, Phi, cQ, cR, 100, .001))

# Graph smoother
ks = Ksmooth1(num, y, A, em$mu0, em$Sigma0, em$Phi, 0, 0, chol(em$Q),
              chol(em$R), 0)

y1s = ks$xs[1,,]
y2s = ks$xs[2,,]
y3s = ks$xs[3,,]

p1 = 2*sqrt(ks$Ps[1,1,])
p2 = 2*sqrt(ks$Ps[2,2,])
p3 = 2*sqrt(ks$Ps[3,3,])

par(mfrow=c(3,1))
plot(WBC, type='p', pch=19, ylim=c(1,5), xlab='day')
lines(y1s)
lines(y1s+p1, lty=2, col=4)
lines(y1s-p1, lty=2, col=4)

plot(PLT, type='p', ylim=c(3,6), pch=19, xlab='day')
lines(y2s)
lines(y2s+p2, lty=2, col=4)
lines(y2s-p2, lty=2, col=4)

plot(HCT, type='p', pch=19, ylim=c(20,40), xlab='day')
lines(y3s)
lines(y3s+p3, lty=2, col=4)
lines(y3s-p3, lty=2, col=4)
