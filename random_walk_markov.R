# random walk
# https://www.analyticsvidhya.com/blog/2021/02/markov-chain-mathematical-formulation-intuitive-explanation-applications/

plot(c(0,1000),c(-100,100), 
     xlab = "Time Steps", 
     ylab = "Cumulative Number of Step" , 
     main = " Random Walk Simulation")

for (i in 1:30) {
  x <- as.integer(rnorm(1000))
  lines(cumsum(x), type = "l", col=sample(1:10,1)) 
}

# check other methods to produce a 'random walk' trend 