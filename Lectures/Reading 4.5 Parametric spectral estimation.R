# 4.5 Parametric spectral estimation

# Example 4.18 Autoregressive Spectral Estimator for SOI ------------------
spaic <- spec.ar(soi, log="no", ylim=c(0,.3)) # min AIC spec, order = 15
text(frequency(soi)*1/52, .07, substitute(omega==1/52))  # El Nino Cycle
text(frequency(soi)*1/12, .27, substitute(omega==1/12))  # Yearly Cycle
(soi.ar = ar(soi, order.max=30))                         # estimates and AICs
dev.new() 
plot(1:30, soi.ar$aic[-1], type="o")                     # plot AICs


# Better comparison of pseudo-ICs 
n <- length(soi)
c() -> AIC -> AICc -> BIC
for (k in 1:30){
  fit = ar(soi, order=k, aic=FALSE) 
  sigma2  = fit$var.pred               
  BIC[k]  = log(sigma2) + (k*log(n)/n)
  AICc[k] = log(sigma2) + ((n+k)/(n-k-2))
  AIC[k]  = log(sigma2) + ((n+2*k)/n) 
}

IC <- cbind(AIC, BIC+1)
ts.plot(IC, type="o", xlab="p", ylab="AIC / BIC")
grid()
text(15.2, -1.48, "AIC")
text(15,   -1.35, "BIC")
