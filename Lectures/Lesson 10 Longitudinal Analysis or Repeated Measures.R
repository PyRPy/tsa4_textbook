# Lesson 10: Longitudinal Analysis or Repeated Measures

# read data ---------------------------------------------------------------

phlebitisdata <- read.table("phlebitis.csv", header = T, sep = ",")
head(phlebitisdata)

attach(phlebitisdata)


# anova -------------------------------------------------------------------

aov.p <- aov(Y ~ factor(Treatment)*factor(Time) + Error(factor(Animal)),
             data = phlebitisdata)
summary(aov.p)


# interaction plot --------------------------------------------------------

library(nlme)
interaction.plot(Time, factor(Treatment), Y, lty = c(1:3), lwd=2,
                 ylab = "mean of Y", 
                 xlab = "Time", 
                 trace.label = "Treatment")


# correlation model -------------------------------------------------------

nestinginfo <- groupedData(Y ~ Treatment | Animal, data = phlebitisdata)
fit.compsym <- gls(Y ~ factor(Treatment)*factor(Time), data = nestinginfo,
                   corr=corCompSymm(, form = ~ 1|Animal))

fit.nostruct <- gls(Y ~ factor(Treatment)*factor(Time), data = nestinginfo, 
      corr=corSymm(, form= ~1|Animal), weights = varIdent(form =  ~1|Time))

fit.ar1 <- gls(Y ~ factor(Treatment)*factor(Time), 
               data=nestinginfo, 
               corr=corAR1(, form=~ 1 | Animal))
fit.ar1het <- gls(Y ~ factor(Treatment)*factor(Time), data=nestinginfo, 
   corr=corAR1(,form= ~ 1 | Animal), weights=varIdent(form = ~ 1 | Time))


# compare models ----------------------------------------------------------

anova(fit.compsym, fit.nostruct, fit.ar1, fit.ar1het)

fit.ar1polytime <- gls(Y ~ factor(Treatment)*poly(Time, degree = 3), 
                       data=nestinginfo,
                       corr=corAR1(, form= ~ 1 | Animal))
summary(fit.ar1polytime)

anova(fit.compsym)
anova(fit.ar1)
anova(fit.ar1polytime)
anova(fit.ar1polytime, fit.ar1)
detach(phlebitisdata)

