# Lesson 13: Fractional Differencing and Threshold Models

# 13.1 Long Memory Models and Fractional Differences ----------------------
varve <- ts(scan("varve.dat"))
library(astsa)
library(arfima)

# center the logs
y <- log(varve) - mean(log(varve))

acf2(y)

# estimate d
varvefd <- arfima(y)
summary(varvefd)
d <- summary(varvefd)$coef[[1]][1]
d

se.d <- summary(varvefd)$coef[[1]][1,2]  
se.d

# check residuals
innov <- resid(varvefd)
plot.ts(innov[[1]])
acf(innov[[1]])


# 13.2 Threshold Models ---------------------------------------------------
library(astsa)
flu = scan("flu.dat")
flu = ts(flu)
plot(flu,type="b")
y = diff(flu,1)
plot(y,type="b")

model = ts.intersect(y, lag1y=lag(y,-1), lag2y=lag(y, -2), lag3y=lag(y,-3), 
                     lag4y=lag(y, -4))
x = model[,1]
P = model[,2:5]
c = .05 ## Threshold value

##Regression for values below the threshold
less = (P[,1]<c)
less
x1 = x[less]
P1 = P[less,]

out1 = lm(x1~P1[,1]+P1[,2]+P1[,3]+P1[,4])
summary(out1)

##Regression for values above the threshold
greater = (P[,1]>=c)
x2 = x[greater]
P2 = P[greater,]
out2 = lm(x2~P2[,1]+P2[,2]+P2[,3]+P2[,4])
summary(out2)

##Residuals
res1 = residuals(out1)
res2 = residuals(out2)
less[less==1]= res1
greater[greater==1] = res2
resid = less + greater
acf2(resid)

# a <- c(1,2,3)
# b <- c(4,5,6)
# c <- a + b
# c

##Predicted values
less = (P[,1]<c)
greater = (P[,1]>=c)

fit1 = predict(out1)
fit2 = predict(out2)
less[less==1]= fit1
greater[greater==1] = fit2
fit = less + greater
plot(y, type="o")
lines(fit, col = "red", lty="dashed")

# install.packages("tsDyn") #if not yet installed
library(tsDyn)
dflu=diff(flu,1) # as given in the text
flu.tar4.05=setar(dflu, m=4, thDelay=0, th=.036) 
summary(flu.tar4.05) #this shows the final model above and below .05
plot(flu.tar4.05) #cycles through fit and diagnostic plots 
