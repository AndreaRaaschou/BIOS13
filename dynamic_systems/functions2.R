# This scripts contains functions for multidimensional dynamic systems

#' Plot the isoclines of a system
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


















