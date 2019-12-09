# Statistical methods in the frequency domain
library(astsa)

# 7.1 Introduction --------------------------------------------------------

attach(eqexp)
P <- 1:1024
S <- P + 1024
x <- cbind(EQ5[P], EQ6[P], EX5[P], EX6[P], NZ[P], EQ5[S], EQ6[S], EX5[S],
           EX6[S], NZ[S])

x.name <- c("EQ5", "EQ6", "EX5", "EX6", "NZ")

colnames(x) <- c(x.name, x.name)
plot.ts(x, main="")
mtext("P waves", side=3, line=1.2, adj=0.05, cex=1.2)
mtext("S waves", side=3, line=1.2, adj=0.85, cex=1.2)

