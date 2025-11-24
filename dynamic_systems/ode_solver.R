# 5) Write a general ode-solver, base it on the code for (4)
library(deSolve)
rm(list = ls())

#' General ordinary differential equations solver
#' Implements the Euler solution with a given start value and constant step size.
#' @param f ODE to solve
#' @param y_0 numerical, starting value f(0) = y_0
#' @param h numerical, step size
ode_solver <- function(f, y_0, h){
  # Matrix to hold x and y-values
  num_sol <- matrix(c(0, y_0), 1, 2)
  # Placeholder
  i <- 1
  
  # Repeat loop while the x-value is less than 20
  while (num_sol[i, 1] < 20){
    # Update x and y
    x <- num_sol[i, 1] + h
    y <- num_sol[i, 2] + h * f(num_sol[i, 1])
    
    # Add as new row to numerical solution matrix
    num_sol <- rbind(num_sol, c(x, y))
    i <- i + 1
  }
  # Return the finished numerical solution
  return(num_sol)
}

sq <- function(x) 2*x

# Declare some functions to test
square <- function(t, x, parms) {
  list(2*x)  
}


# compare accuracy and speed with R’s ODE solver:
test <- ode_solver(sq, 0, 1)

out <- ode(y = 1, func = square, times = seq(1, 20, by=0.1), 
           parms = 2)

plot(out)
plot(test)


# QUESTIONS:
# Am I supposed to choose an arbitrary x0 as starting point?
# Could try to use the starting field descriped in video?

# Other stuff to try:
# implement dynamic step size - makes the solver more efficient while keeping accuracy
# (search for runge-kutta-fehlberg (RK45) method)
