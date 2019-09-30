# Lesson 6: The Periodogram

# 6.1 The Periodogram -----------------------------------------------------

# Example 6-1 Section - brain cortex activity
x <- scan("cortex.dat")
FF <- abs(fft(x)/sqrt(length(x)))^2
FF
# # Only need the first (n/2)+1 values of the FFT result.
P <- (4/length(x))*FF[1:65]

# this creates harmonic frequencies from 0 to .5 in steps of 1/128.
f <- (0:64)/128

# # This plots the periodogram; type = "l" creates a line plot. Note: l is 
# lowercase L, not number 1.
plot(f, P, type = "l")

# Example 6-2 sunspot - semi-annual sunspot activity
sunspots <- scan("sunspots.dat")
plot(sunspots, type = "b")
x <- diff(sunspots)
I <- abs(fft(x)/sqrt(length(x)))^2
P <- (4/length(x))*I(1:230)
freq <- (0:229)/length(x)
plot(freq, P, type = "l")

sunspots <- scan("sunspots.dat")
plot(sunspots, type = "b")
x <- diff(sunspots)
I <- abs(fft(x)/sqrt(458))^2
P <- (4/458)*I(1:230)
freq <- (0:229)/458
plot(freq, P, type = "l")
