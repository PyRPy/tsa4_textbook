# Example 7.12 Cluster Analysis for Earthquakes and Explosions
library(astsa)
library(cluster)
P = 1:1024
S = P + 1024
p.dim = 2
n = 1024
eq = as.ts(eqexp[, 1:8])
ex = as.ts(eqexp[, 9:16])
nz = as.ts(eqexp[, 17])
f = array(dim = c(17, 2, 2, 512))
L = c(15, 15)
for (i in 1:8){
  f[i,,,] = mvspec(cbind(eq[P,i], eq[S,i]), spans=L, taper=.5)$fxx
  f[i+8,,,] = mvspec(cbind(ex[P,i], ex[S,i]), spans = L, taper = .5)$fxx
}

f[17,,,] = mvspec(cbind(nz[P], nz[S]), spans = L, taper = .5)$fxx
JD = matrix(0, 17, 17)

# calculate symetric information criteria
for (i in 1:16){
  for (j in (i+1):17){
    for (k in 1:256){
      tr1 = Re(sum(diag(solve(f[i,,,k], f[j,,,k]))))
      tr2 = Re(sum(diag(solve(f[j,,,k], f[i,,,k]))))
      JD[i,j] = JD[i,j] + (tr1 + tr2 - 2*p.dim)
    }
  }
}
JD = (JD + t(JD))/n

colnames(JD) = c(colnames(eq), colnames(ex), "NZ")
rownames(JD) = colnames(JD)

cluster.2 = pam(JD, k=2, diss=TRUE)
summary(cluster.2)

clusplot(JD, cluster.2$cluster, col.clus = 1, labels = 3, lines = 0,
         col.p = 1, main="Cluster results for Explosion and Earthquakes")

text(2, -1, "Group I", cex=1.1, font = 2)
text(1, 5, "Group II", cex = 1.1, font = 2)












