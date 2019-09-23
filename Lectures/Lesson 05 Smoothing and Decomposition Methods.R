# Lesson 5: Smoothing and Decomposition Methods

# 5.1 Decomposition Models ------------------------------------------------

# Example 5-1 Continued: Additive Decomposition for Beer Production
beerprod <- scan("beerprod.dat")
beerprod <- ts(beerprod, frequency = 4)
plot(beerprod)

# apply decompose 
decomp_beer <- decompose(beerprod, type = "additive")
plot(decomp_beer)
decomp_beer
decomp_beer$seasonal
str(decomp_beer)

# Example 5-1 Continued: Multiplicative Decomposition for Beer Production
decomp_beer_mult <- decompose(beerprod, type = "multiplicative")
decomp_beer_mult$figure


# Lowess Seasonal and Trend Decomposition ---------------------------------
stl(beerprod, "periodic")
plot(stl(beerprod, "periodic"))


# 5.2 Smoothing Time Series -----------------------------------------------
# Example 5-3: Quarterly Beer Production in Australia Section
beerprod <- scan("beerprod.dat")

# two-sided filter - assign weights
trend_pattern <- filter(beerprod, 
                        filter = c(1/8, 1/4, 1/4, 1/4, 1/8), 
                        sides = 2)

# plot the trend
plot(beerprod, type = "b", main = "moving average annual trend")
lines(trend_pattern, col="blue")

# subtract the trend pattern from the data values to get a better look 
# at seasonality
seasonals <- beerprod - trend_pattern
plot(seasonals, type="b", main="Seasonal pattern for beer production")

# one-sided filter
trend_pattern2 <-  filter(beerprod, filter = c(1/4, 1/4, 1/4, 1/4), sides=1)
plot(beerprod, type = "b", main = "MA - one sided")
lines(trend_pattern2, col="blue")

# Example 5-4: U.S. Monthly Unemployment
unemploy <- scan("unemp.dat")
trendunemploy <- filter(unemploy, 
                 filter = c(1/24, rep(1/12, 10), 1/24, sides = 2))
trendunemploy <- ts(trendunemploy, start = c(1948,1), freq = 12)
plot (trendunemploy, 
      main="Trend in U.S. Unemployment, 1948-1978", 
      xlab = "Year")

#  lowess smoother
unemploy <- ts(unemploy, start = c(1948,1), freq=12)
plot(lowess(unemploy, f = 2/3), 
     main ="Lowess smoothing of U.S. Unemployment Trend")

# Single Exponential Smoothing --------------------------------------------

oilindex <- ts(scan("oildata.dat"))
plot (oilindex, type = "b", main = "Log of Oil Index Series")

# ARIMA(0,1,1)
expsmoothfit <- arima (oilindex, order = c(0,1,1))

# to see the arima results
expsmoothfit 

# predicted values
predicteds <- oilindex - expsmoothfit$residuals 
plot (oilindex, type="b", main = "Exponential Smoothing of Log of Oil Index")
lines (predicteds, col="blue")

# forecast for time 101
1.3877*oilindex[100]-0.3877*predicteds[100] 
