# Lesson 3: Identifying and Estimating ARIMAmodels


# 3.1 Non-seasonal ARIMA Models -------------------------------------------

# Example 1: The Lake Erie data
# The series is n = 40 consecutiveannual measurements of the level of Lake 
# Erie in October

library(astsa)
# read data
xerie <- scan("eriedata.dat")
# transform to a ts object
xerie <- ts(xerie)
plot(xerie, type = "b")
acf2(xerie)

# fit a AR(1) model
sarima(xerie, 1, 0, 0)

# fit a ARMA(1, 1) model - over-parameterized
sarima(xerie, 1, 0, 1)
# p.value for ma1 = 0.6472, not significant

# forecasting based on AR(1) for next 4 points
sarima.for(xerie, 4, 1, 0, 0)


# 3.2 Diagnostics ---------------------------------------------------------


