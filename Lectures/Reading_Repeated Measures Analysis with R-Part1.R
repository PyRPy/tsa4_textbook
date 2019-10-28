# Repeated Measures Analysis with R - Part 1
# https://stats.idre.ucla.edu/r/seminars/repeated-measures-analysis-with-r/
# Demo Analysis #1 --------------------------------------------------------

demo1  <- read.csv("https://stats.idre.ucla.edu/stat/data/demo1.csv")

## Convert variables to factor
demo1 <- within(demo1, {
  group <- factor(group)
  time <- factor(time)
  id <- factor(id)
})
head(demo1)

par(cex = .6)

with(demo1, interaction.plot(time, group, pulse,
      ylim = c(5, 20),
      lty= c(1, 12), lwd = 3,
      ylab = "mean of pulse", 
      xlab = "time", 
      trace.label = "group"))

demo1.aov <- aov(pulse ~ group * time + Error(id), 
                 data = demo1)
summary(demo1.aov)

# between groups test indicates that the variable group is significant
# within subject test indicate that there is not a significant time effect
# since the lines are parallel, we are not surprised that the interaction 
# between time and group is not significant


# Demo Analysis #2 --------------------------------------------------------

demo2 <- read.csv("https://stats.idre.ucla.edu/stat/data/demo2.csv")
## Convert variables to factor
demo2 <- within(demo2, {
  group <- factor(group)
  time <- factor(time)
  id <- factor(id)
})
par(cex = .6)

with(demo2, interaction.plot(time, group, pulse,
             ylim = c(10, 40), lty = c(1, 12), lwd = 3,
             ylab = "mean of pulse", xlab = "time", trace.label = "group"))

# id in Error to reflect 'random' effects
demo2.aov <- aov(pulse ~ group * time + Error(id), data = demo2)
summary(demo2.aov)

# between groups test indicates that the variable group is not significant
# within subject test indicate that there is a significant time effect
# the lines are parallel consistent with the finding that the interaction 
# is not significant.


# Demo Analysis #3 --------------------------------------------------------

demo3 <- read.csv("https://stats.idre.ucla.edu/stat/data/demo3.csv")
## Convert variables to factor
demo3 <- within(demo3, {
  group <- factor(group)
  time <- factor(time)
  id <- factor(id)
})

par(cex = .6)

with(demo3, interaction.plot(time, group, pulse,
     ylim = c(10, 60), lty = c(1, 12), lwd = 3,
     ylab = "mean of pulse", xlab = "time", trace.label = "group"))

demo3.aov <- aov(pulse ~ group * time + Error(id), data = demo3)
summary(demo3.aov)

# between groups test indicates that the variable group is significant
# within subject test indicate that there is a significant time effect
#  interaction of time and group is significant 


# Demo Analysis #4 --------------------------------------------------------

demo4 <- read.csv("https://stats.idre.ucla.edu/stat/data/demo4.csv")
## Convert variables to factor
demo4 <- within(demo4, {
  group <- factor(group)
  time <- factor(time)
  id <- factor(id)
})
par(cex = .6)

with(demo4, interaction.plot(time, group, pulse,
     ylim = c(10, 60), lty = c(1, 12), lwd = 3,
     ylab = "mean of pulse", xlab = "time", trace.label = "group"))

demo4.aov <- aov(pulse ~ group * time + Error(id), data = demo4)
summary(demo4.aov)

# within subject test indicate that the interaction of time and group 
# is significant
# main effect of time is not significant
#  between groups test indicates that there the variable group is 
# significant


# Exercise data -----------------------------------------------------------
exer <- read.csv("https://stats.idre.ucla.edu/stat/data/exer.csv")
## Convert variables to factor
exer <- within(exer, {
  diet <- factor(diet)
  exertype <- factor(exertype)
  time <- factor(time)
  id <- factor(id)
})
print(exer)

# Exercise example, model 1 (time and diet)
par(cex=.6)

with(exer, interaction.plot(time, diet, pulse,
      ylim = c(90, 110), lty = c(1, 12), lwd = 3,
      ylab = "mean of pulse", xlab = "time", trace.label = "group"))

diet.aov <- aov(pulse ~ diet * time + Error(id),
                data = exer)
summary(diet.aov)


# Exercise example, model 2 (time and exercise type)

with(exer, interaction.plot(time, exertype, pulse,
      ylim = c(80, 130), lty = c(1, 2, 4), lwd = 2,
      ylab = "mean of pulse", xlab = "time"))

exertype.aov <- aov(pulse ~ exertype * time + Error(id), data = exer)
summary(exertype.aov)

# between subject test of the effect of exertype is also significant


# Variance-Covariance Structures ------------------------------------------

mat <- with(exer, matrix(c(pulse[time==1], pulse[time==2], pulse[time==3]), 
                         ncol = 3))
var(mat)
cor(mat)
# Exercise example, model 2 using the gls function
# Compound Symmetry
library(nlme)
longg <- groupedData(pulse ~ as.numeric(exertype) * as.numeric(time) | id, data = exer)
fit.cs <- gls(pulse ~ exertype * time, data = longg,
              corr = corCompSymm(, form= ~ 1 | id) )
summary(fit.cs)

# Unstructured
fit.un <- gls(pulse ~ exertype * time, data = longg,
              corr=corSymm(form = ~ 1 | id),
              weights = varIdent(form = ~ 1 | time))
# specifies that the variance at each time point can be different
summary(fit.un)
anova(fit.un)

# Autoregressive
fit.ar1 <- gls(pulse ~ exertype * time, data = longg,
               corr = corAR1(, form= ~ 1 | id))
summary(fit.ar1)
anova(fit.ar1)

# Autoregressive with heterogeneous variances
fit.arh1 <- gls(pulse ~ exertype * time, data = longg,
                corr = corAR1(, form = ~ 1 | id), 
                weight = varIdent(form = ~ 1 | time))
summary(fit.arh1)
anova(fit.arh1)


# Model comparison (using the anova function) -----------------------------
anova(fit.cs, fit.un)
anova(fit.cs, fit.ar1)
anova(fit.cs, fit.arh1)

# Autoregressive Heterogeneous Variances and Unstructured since these two 
# models have the smallest AIC values


# Exercise example, model 3 (time, diet and exertype)-using the ao --------

par(cex = .6)
with(exer, interaction.plot(time[diet==1], exertype[diet==1], pulse[diet==1],
    ylim = c(80, 150), lty = c(1, 12, 8),
    trace.label = "exertype", ylab = "mean of pulse", xlab = "time"))
title("Diet = 1")

with(exer, interaction.plot(time[diet==2], exertype[diet==2], pulse[diet==2],
    ylim = c(80, 150), lty = c(1, 12, 8),
    trace.label = "exertype", ylab = "mean of pulse", xlab = "time"))
title("Diet = 2")

both.aov <- aov(pulse ~ exertype * diet * time + Error(id), data = exer)
summary(both.aov)


# model 3 (time, diet and exertype)-using the gls fuction -----------------


longa <- groupedData(pulse ~ as.numeric(exertype) * as.numeric(diet) * as.numeric(time) | id,
                     data = exer)
both.arh1 <- gls(pulse ~ exertype * diet * time, data = longa,
                 corr = corAR1(, form = ~ 1 | id), 
                 weight = varIdent(form = ~ 1 | time))
summary(both.arh1)
anova(both.arh1)


# Contrasts and interaction contrasts for model 3 -------------------------
longa[, c("ef", "df", "tf")] <- longa[, c("exertype", "diet", "time")]
m <- matrix( c( c(-1/2, 1/2, 0), c(-1/3, -1/3, 2/3) ), ncol=2)
contrasts(longa$ef) <- m
(contrasts(longa$tf) <- m)
(contrasts(longa$df)  <- c(-1/2, 1/2))

model.cs <- gls(pulse ~ ef * df * tf, data = longa,
                corr = corCompSymm(, form = ~ 1 | id) )

summary(model.cs)

longa$e1d12 <- (-1/2*(longa$exertype==1 & longa$diet==1))
longa$e1d12[longa$exertype==1 & longa$diet==2] <- 1/2

longa$e2d12 <- (-1/2*(longa$exertype==1))
longa$e2d12[longa$exertype==2 & longa$diet==2] <- 1/2

longa$e3d12 <- (-1/2*(longa$exertype==3 & longa$diet==1))
longa$e3d12[longa$exertype==3 & longa$diet==2] <- 1/2

modela.cs <- gls(pulse ~ ef + e1d12 + e2d12 + e3d12 , data = longa,
                 corr = corCompSymm(, form = ~ 1 | id) )
summary(modela.cs)
