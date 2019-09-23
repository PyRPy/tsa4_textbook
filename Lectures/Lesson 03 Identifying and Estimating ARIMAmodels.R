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


# 3.3 Forecasting with ARIMA Models ----------------------------------------
# Psi-weight representation of an ARIMA model
# Example: Suppose that an AR(1) model is xt = 40 + 0.6xt-1 + wt
ARMAtoMA(ar = 0.6, ma = 0, 12) # gives first 12 psi-weights, psi-0 = 1

# Psi-Weights for the Estimated AR(2) for the Stride Length Data
ARMAtoMA(ar = list(1.148, -0.3359), ma = 0, 5)

# need to find treadmill data stride length

