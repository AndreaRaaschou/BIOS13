library(deSolve)
rm(list = ls())
setwd('/Users/andrearaaschou/courses/BIOS13/github/dynamic_systems')
source('functions.R')

# 1) Plot growth function against population size 
y <- growth_function(seq(1,120, by=0.1), P)
plot(y, type='l')

# 2) Plot population size as a function of time 
# Vector of time-points for the output
timevec <- seq(0, 20, by=0.1)

# List of parameters
P <- list(r0=1, K=100)
n0 <- 1 # initial population size

# call the ode function
out <- ode(y = n0, func = logisticGrowth,
           times = timevec, parms = P)

plot(out , main='Logistic growth')

# 3) Numerically test the solution
appr_values <- numeric_deriv(2,out)
plot(appr_values, type = 'l', col='red')
lines(out, col='black')


