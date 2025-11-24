library(deSolve)
rm(list = ls())
setwd('/Users/andrearaaschou/courses/BIOS13/github/dynamic_systems')
source('functions1.R')

# List of parameters
P <- list(r0=1, K=100)
n0 <- 1 # initial population size
timevec <- seq(0, 20, by=0.01) # Vector of time-points for the output

# 1) Plot growth function against population size 
y <- growth_function(n=seq(1,120, by=0.1), P)
plot(y, type='l')

# 2) Plot population size as a function of time 
out_log <- ode(y = n0, func = logisticGrowth,
           times = timevec, parms = P)

plot(out_log , main='Logistic growth')

# 3) Numerically test the solution
appr_values <- numeric_deriv(2,out_log)
plot(appr_values, type = 'l', col='red')
lines(out_log, col='black')

# 5) Testing my own ode_solver
# Testing dy/dx = y^2
test_sq <- ode_solver(sq, 1, 0.01, x_max = 1.1)

out_sq <- ode(y = 1, func = square, times = seq(0, 1, by=0.01), 
           parms = NULL)

par(mfrow = c(1,2))

# Plot test
plot(test_sq[,1], test_sq[,2], type = "l", 
     col = "blue", main = "My ODE Solver", 
     xlab = "x", ylab = "y", ylim = c(0, 100))

# Plot out
plot(out_sq[,1], out_sq[,2], type = "l", 
     col = "red", main = "deSolve Output", 
     xlab = "x", ylab = "y", ylim = c(0, 100))

# Testing the logistic function
test_log <- ode_solver(growth_function, parms = P)

plot(test_log[,1], test_log[,2], type = "l", 
     col = "blue", main = "My ODE Solver", 
     xlab = "x", ylab = "y", ylim = c(0, 110))
plot(out_log[,1], out_log[,2], type = "l", 
     col = "red", main = "deSolve Output", 
     xlab = "x", ylab = "y", ylim = c(0, 110))

# Testing cosine
test_cos <- ode_solver(cosine)
out_cos <- ode(y = 1, func = cosine_deSolve, times = timevec, parms = NULL)

plot(test_cos[,1], test_cos[,2], type = "l", 
     col = "blue", main = "My ODE Solver", 
     xlab = "x", ylab = "y", ylim = c(0, 2.1))
plot(out_cos[,1], out_cos[,2], type = "l", 
     col = "red", main = "deSolve Output", 
     xlab = "x", ylab = "y", ylim = c(0, 2.1))


# QUESTIONS (5):
# Am I supposed to choose an arbitrary x0 as starting point?
# Could try to use the starting field described in video?
# Can I make one function that will work with both the deSolve function and my own function?

# Other stuff to try:
# implement dynamic step size - makes the solver more efficient while keeping accuracy
# (search for runge-kutta-fehlberg (RK45) method)



