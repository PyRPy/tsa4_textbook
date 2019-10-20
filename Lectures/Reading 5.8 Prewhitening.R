
# Example 5.8 Relating prewhitened SOI to transformed recruitment  --------
library(astsa)
soi.d <- resid(lm(soi ~ time(soi), na.action = NULL))
acf2(soi.d)
fit <- arima(soi.d, order=c(1,0,0))
ar1 <- as.numeric(coef(fit)[1])
ar1
soi.pw <- resid(fit)
rec.fil <- filter(rec, filter = c(1, -ar1), sides = 1)
ccf(soi.pw, rec.fil, ylab="CCF", na.action = na.omit, panel.first=grid())
