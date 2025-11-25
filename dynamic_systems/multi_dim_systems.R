library(deSolve)
rm(list = ls())
setwd('/Users/andrearaaschou/courses/BIOS13/github/dynamic_systems')
source('functions2.R')

# 1a) Test isocline function
plot_isoclines(1, 1, 2)

# 1b) Simulate predator prey system using Lotka-Volterras predator-prey equations

# list of parameters in order: (r, a, mu)
P = c(1,1,2)
# starting points of prey (n) and predator (p)
np0 = c(1, 2)

out <- ode(y = np0, func = LV_sys, times = seq(1, 100, by = 0.1), parms = P)

plot(1, type = "n",
     xlim = c(0, 20),
     ylim = c(min(out[,2], out[,3]), max(out[,2], out[,3])),
     xlab = "time",
     ylab = "population",
     main = "Simulation of predator vs prey")
lines(out[,1], out[,2], col='blue')
lines(out[,1], out[,3], col='red') 
legend("topright",
       legend = c("Prey", "Predator"),
       col = c("blue", "red"),
       lty = 1,
       bty = "n") 

# Draw a phase-plane plot
plot(out[,2], out[,3], type = 'l',
     xlab = "n",
     ylab = "p",
     main = "Phase-plane plot of predator vs prey")

# 1c) Calculate the jacobian matrix of the Lotka-Volterra predator-prey equations
jacobian(P)
cycle_per(P)

# 2c and 2d)
source('functions2.R')

# list of parameters in order: (r, a, mu, K)
P = c(2, 1, 3, 10)
# initial population sizes of prey (n) and predator (p)
np0 = c(2, 2)

out <- ode(y = np0, func = LV_sys2, times = seq(1, 100, by = 0.01), parms = P)

plot_isoclines_lg(P[1], P[2], P[3], P[4], out)
plot_simulation(out)



















