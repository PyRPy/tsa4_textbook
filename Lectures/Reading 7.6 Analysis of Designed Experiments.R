# 7.6 Analysis of Designed Experiments
library(astsa)
# Example 7.6 Means Test for the fMRI Data --------------------------------

n <- 128 # length of series
n.freq <- 1 + n/2 # number of frequences
Fr <- (0:(n.freq - 1))/n
N <- c(5,4,5,3,5, 4) # number of series for reach cell
n.subject <- sum(N)
n.trt <- 6 # number of treatment
L <- 3 # for smoothing
num.df <- 2*L*(n.trt - 1) # df for F test
den.df <- 2*L*(n.subject - n.trt)

# Design maxtrix (Z)
Z1 <- outer(rep(1, N[1]), c(1,1,0,0,0,0))
Z2 <- outer(rep(1, N[2]), c(1,0,1,0,0,0))
Z3 <- outer(rep(1, N[3]), c(1,0,0,1,0,0))
Z4 <- outer(rep(1, N[4]), c(1,0,0,0,1,0))
Z5 <- outer(rep(1, N[5]), c(1,0,0,0,0,1))
Z6 <- outer(rep(1, N[6]), c(1,-1,-1,-1,-1,-1))
Z <- rbind(Z1, Z2, Z3, Z4, Z5, Z6)
ZZ <- t(Z)%*%Z
SSEF <- rep(NA, n) -> SSER
HatF <- Z%*%solve(ZZ, t(Z))
HatR <- Z[,1]%*%t(Z[,1])/ZZ[1,1]

loc.name <- c("Cortex 1","Cortex 2","Cortex 3","Cortex 4","Caudate",
            "Thalamus 1","Thalamus 2","Cerebellum 1","Cerebellum 2")
par(mfrow=c(3,3))

for(Loc in 1:9){
  i <- n.trt*(Loc - 1)
  Y <- cbind(fmri[[i+1]], fmri[[i+2]], fmri[[i+3]], fmri[[i+4]],
             fmri[[i+5]], fmri[[i+6]])
  Y <- mvfft(spec.taper(Y, p=0.5))/sqrt(n)
  Y <- t(Y) # Y is now 26 x 128 FFTs


# calculate error spectra
for (k in 1:n){
  SSY <- Re(Conj(t(Y[,k]))%*%Y[,k])
  SSReg <- Re(Conj(t(Y[,k]))%*%HatF%*%Y[,k])
  SSEF[k] <- SSY - SSReg
  SSReg <- Re(Conj(t(Y[,k]))%*%HatR%*%Y[,k])
  SSER[k] <- SSY-SSReg
}

# smooth
sSSEF <- filter(SSEF, rep(1/L, L), circular = TRUE)
sSSER <- filter(SSER, rep(1/L, L), circular = TRUE)
eF <- (den.df/num.df)*(sSSER - sSSEF)/sSSEF

plot(Fr, eF[1:n.freq], type = "l", xlab = "Frequency", ylab="F Statistic",
     ylim = c(0,7))
abline(h=qf(0.999, num.df, den.df), lty=2)
text(0.25, 6.5, loc.name[Loc], cex=1.2)
}
