# Repeated Measures Analysis with R - Part 2
# https://stats.idre.ucla.edu/r/seminars/repeated-measures-analysis-with-r/

# Unequally Spaced Time Points --------------------------------------------
# Modeling Time as a Linear Predictor of Pulse

study2 <- read.csv("https://stats.idre.ucla.edu/stat/data/study2.csv")
study2 <- within(study2, {
  id <- factor(id)
  exertype <- factor(exertype)
  diet <- factor(diet)
})
study2[1:20, ]

## Load
library(lattice)

##
par(cex = .6)
xyplot(pulse ~ time, data = study2, groups = id,
       type = "o", panel = panel.superpose)

xyplot(pulse ~ time | exertype, data = study2, groups = id,
       type = "o", panel = panel.superpose)

xyplot(pulse ~ time | diet, data = study2, groups = id,
       type = "o", panel = panel.superpose)

time.linear <- lme(pulse ~ exertype * time,
                   random = list(id = pdDiag(~ time)), data = study2)
summary(time.linear)
anova(time.linear)

fitted <- fitted(time.linear, level=0)

with(study2, plot(time[exertype==3], fitted[exertype==3], ylim = c(50, 150),
              xlab = "time", ylab = "predicted", type = "b", col = "green"))
with(study2, points(time[exertype==2], fitted[exertype==2],
                    pch = 4, type = "b", col = "red"))
with(study2, points(time[exertype==1], fitted[exertype==1],
                    pch = 16, type = "b", col = "blue"))

xyplot(pulse[exertype==1] ~ time[exertype==1], data = study2, groups = id,
       type = "o", ylim = c(50, 150), xlim = c(0, 800),
       panel = panel.superpose, col = "blue")
with(study2, lines(time[exertype==1], fitted[exertype==1],
                   ylim = c(50, 150),  xlim = c(0, 800),
                   type = "b", col = "dark blue", lwd = 4))

xyplot(pulse[exertype==2] ~ time[exertype==2], data = study2, groups=id,
       type = "o", ylim = c(50, 150), xlim = c(0, 800),
       panel = panel.superpose, col = "red")
with(study2, lines(time[exertype==2], fitted[exertype==2],
                   ylim = c(50, 150),  xlim = c(0, 800),
                   type = "b", col = "dark red", lwd = 4))

xyplot(pulse[exertype==3] ~ time[exertype==3], data = study2, groups = id,
       type = "o", ylim = c(50, 150), xlim = c(0, 800),
       panel = panel.superpose, col = "green")
with(study2, lines(time[exertype==3], fitted[exertype==3],
                   ylim = c(50, 150), xlim = c(0, 800),
                   type = "b", col = "dark green", lwd = 4))

# Modeling Time as a Quadratic Predictor of Pulse
# to model the quadratic effect of time, we add time*time
study2$time2 <- study2$time^2
time.quad <- lme(pulse ~ exertype * time + time2,
                 random = list(id = pdDiag(~ time)), study2)
summary(time.quad)
anova(time.quad)

fitted2 <- fitted(time.quad, level = 0)
a <- with(study2,
          data.frame(time, fitted2, exertype)[order(exertype, time), ])

with(a, {
  plot(time[exertype==3], fitted2[exertype==3], ylim = c(50, 150),
       xlab = "time", ylab = "predicted", col = "green", type = "b")
  points(time[exertype==2], fitted2[exertype==2],
         pch = 4, col = "red", type = "b")
  points(time[exertype==1], fitted2[exertype==1],
         pch = 16, col = "blue", type = "b")
  title("Time Quadratic Effect")})

xyplot(pulse[exertype==1] ~ time[exertype==1], groups = id, data = study2,
       ylim = c(50, 150), xlim = c(0, 800), type = "o",
       panel=panel.superpose, col="blue")
with(a, lines(time[exertype==1], fitted2[exertype==1],
              ylim = c(50, 150), xlim = c(0, 800),
              type = "b", col = "dark blue", lwd = 4))

xyplot(pulse[exertype==2] ~ time[exertype==2], groups = id, data = study2,
       ylim=c(50, 150), xlim=c(0, 800), type="o",
       panel=panel.superpose, col="red")
with(a, lines(time[exertype==2], fitted2[exertype==2],
              ylim = c(50, 150), xlim = c(0, 800),
              type = "b", col = "dark red", lwd = 4))

xyplot(pulse[exertype==3] ~time[exertype==3], groups = id, data = study2,
       ylim = c(50, 150), xlim = c(0, 800), type = "o",
       panel = panel.superpose, col = "green")
with(a, lines(time[exertype==3], fitted2[exertype==3],
              ylim = c(50, 150), xlim = c(0, 800),
              type = "b", col = "dark green", lwd = 4))


# Modeling Time as a Quadratic Predictor of Pulse, Interacting by  --------

time.quad2 <- lme(pulse ~ exertype * time + exertype * time2,
                  random = list(id = pdDiag(~ time)), data = study2)
summary(time.quad2)

fitted3 <- fitted(time.quad2, level = 0)
a <- with(study2,
          data.frame(time, fitted3, exertype)[order(exertype, time), ])

with(a, {
  plot(time[exertype==3], fitted3[exertype==3], ylim = c(50, 150),
       xlab = "time", ylab = "predicted", col = "green", type = "b")
  points(time[exertype==2], fitted3[exertype==2],
         pch = 4, col = "red", type = "b")
  points(time[exertype==1], fitted3[exertype==1],
         pch = 16,  col = "blue", type = "b")
  title("Time Quadratic Effect")})


# optional ----------------------------------------------------------------

xyplot(pulse[exertype==1] ~ time[exertype==1], groups = id, data = study2,
       ylim = c(50, 150), xlim = c(0, 800), type = "o",
       panel = panel.superpose, col = "blue")
with(a, lines(time[exertype==1], fitted3[exertype==1],
              ylim = c(50, 150), xlim = c(0, 800),
              type = "b", col = "dark blue", lwd = 4))

xyplot(pulse[exertype==2] ~ time[exertype==2], groups = id, data = study2,
       ylim = c(50, 150), xlim = c(0, 800), type = "o",
       panel = panel.superpose, col = "red")
with(a, lines(time[exertype==2], fitted3[exertype==2],
              ylim = c(50, 150), xlim = c(0, 800),
              type = "b", col = "dark red", lwd = 4))

xyplot(pulse[exertype==3] ~ time[exertype==3], groups = id,  data = study2,
       ylim = c(50, 150), xlim = c(0, 800), type = "o",
       panel = panel.superpose, col = "green")
with(a, lines(time[exertype==3], fitted3[exertype==3],
              ylim = c(50, 150), xlim = c(0, 800),
              type = "b", col = "dark green", lwd = 4))
