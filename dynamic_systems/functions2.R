# This scripts contains functions for multidimensional dynamic systems

#' Plot the isoclines of the normal Lotka-Volterra equations
#' @param r growth rate of prey
#' @param a attack rate
#' @param mu death rate of predator (to stop exponential growth of the predator)
plot_isoclines <- function(r, a, mu){
  par(mfrow = c(1,1))
  # Plot the lines by using the first and last points of them (intercept and )
  # line 1: (n = 0, p = r/a), (n = 2*mu/a, p = r/a)
  # line 2: (n = mu/a, p = 0), (n = mu/a, p = 2*r/a)
  plot(1, type = "n",
       xlim = c(0, 2*mu/a),
       ylim = c(0, 2*r/a),
       xlab = "n, prey",
       ylab = "p, predator",
       main = "Isocline plot, predator vs prey")
  
  abline(h = r/a)
  abline(v = mu/a)
}

#' @param t, time
#' @param np vector, (n, p)
#' @param parms vector, (r, a, mu)
LV_sys <- function(t, np, parms) { 
  # extract vector content:
  n <- np[1]
  p <- np[2]
  
  # calculate the two growth rates:
  dndt <- parms[1]*n - parms[2]*n*p
  dpdt <- parms[2]*n*p - parms[3]*p
  
  # return the result as a vector in a list
  return(list(c(dndt, dpdt ))) 
}

LV_sys2 <- function(t, np, parms) { 
  # extract vector content:
  n <- np[1]
  p <- np[2]
  
  # calculate the two growth rates:
  dndt <- parms[1]*n*(1 - n/parms[4]) - parms[2]*n*p
  dpdt <- parms[2]*n*p - parms[3]*p
  
  # return the result as a vector in a list
  return(list(c(dndt, dpdt ))) 
}

#' Calculate the Jacobian matrix of the Lotka-Volterra predator-prey equations
#' Use simple formulas from lecture
#' @param parms (r, a, mu)
#' @return jacobian matrix
jacobian <- function(parms){
  r = parms[1]
  mu = parms[3]
  jacobian <- matrix(c(0, -mu, r, 0), 2, 2)
  return(jacobian)
}

#' Calculate the cycle period associated with the Lotka-Volterra predator-prey equations
#' @param parms (r, a, mu)
cycle_per <- function(parms){
  r = parms[1]
  mu = parms[3]
  
  # Calculate the period of cycles using the imaginary part of eigenvalues
  T = 2*pi / abs(sqrt(mu*r))
  return(T)
}

#' Plot the isoclines of the Lotka-Volterra equations with logistic growth added
#' @param r growth rate of prey
#' @param a attack rate
#' @param mu death rate of predator (to stop exponential growth of the predator)
#' @param K carrying capacity for prey population
plot_isoclines_lg <- function(r, a, mu, K, out){
  par(mfrow = c(1,1)) 
  
  n = seq(0, 2*mu/a, by = 0.01)
  y_prey <- (r/a) * (1 - n/K)
  
  # compute axis limits to include both isoclines and trajectory
  x_min <- 0
  x_max <- max(2*mu/a, max(out[,2]))
  y_min <- 0
  y_max <- max(max(y_prey), max(out[,3]))
  
  # Isocline for predator equilibrium density
  plot(n, y_prey, type = "l", col = 'blue', 
       xlab = "n, prey",
       ylab = "p, predator",
       main = "Isocline plot, predator vs prey",
       xlim = c(x_min, x_max),
       ylim = c(y_min, y_max))
  
  # Isocline for prey equilibrium density
  abline(v = mu/a, col = 'red')
  
  legend("topright",
         legend = c("Prey", "Predator", "Phase plane"),
         col = c("blue", "red", "black"),
         lty = 1,
         bty = "n") 
  
  # plot phase plane
  lines(out[,2], out[,3])
}

#' @param out deSolve object from ode function
plot_simulation <- function(out){
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
}

#' Calculate the Jacobian matrix of the Lotka-Volterra predator-prey equations
#' With added complexity (logistic equation for prey population growth)
#' Use simple formulas from lecture
#' @param parms (r, a, mu, K)
#' @return jacobian matrix
jacobian2 <- function(parms){
  # add partial derivative functions here ...
  jacobian <- matrix(c(,,,), 2, 2)
  return(jacobian)
}












