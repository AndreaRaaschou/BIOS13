logisticGrowth <- function(t, n, P) {
  dndt <- P$r0 * n * ( 1 - n / P$K )
  return(list(dndt))
}

#' Normal logistic equation for population growth
#' @param n numerical value, population size
#' @param P list with r(per capita growth rate) and K (carrying capacity) values
growth_function <- function(t = 0, n, P){
  dndt <- P$r0 * n * ( 1 - n / P$K )
  return(dndt)
}

#' @param f matrix, the function to analyze
numeric_deriv <- function(delta_t, f){
  # make matrix with starting t and n-values
  apprfun <- matrix(c(f[1,1], f[1,2]), 1, 2)
  t <- 0
  # Start filling matrix with append with more t and n-values using delta_t to size the steps
  while(t < 20){
    # take one step on t-axis
    t <- t + delta_t
    # find the index of the closest t-value
    nind <- which.min(abs(f[,1] - t))
    # add the new values to the apprfun matrix as a new row
    apprfun <- rbind(apprfun, c(t, f[nind, 2]))
  }
  return(apprfun)
}

#' General ordinary differential equations solver
#' Implements the Euler solution with a given start value and constant step size.
#' @param f ODE to solve
#' @param y_0 numerical, starting value f(0) = y_0
#' @param h numerical, step size
#' @param x_max numerical, when to stop the loop/numerical solution
ode_solver <- function(f, y_0 = 1, h = 0.01, x_max = 20, parms = NULL){
  # Matrix to hold x and y-values
  num_sol <- matrix(c(0, y_0), 1, 2)
  # Placeholder
  i <- 1
  
  # Repeat loop while the x-value is less than 20
  while (num_sol[i, 1] < x_max){
    # Update x and y
    x_next <- num_sol[i, 1] + h
    y_next <- num_sol[i, 2] + h * f(num_sol[i, 1], num_sol[i, 2], parms)
    
    # Add as new row to numerical solution matrix
    num_sol <- rbind(num_sol, c(x_next, y_next))
    i <- i + 1
  }
  # Return the finished numerical solution
  return(num_sol)
}

#' Not finished
#' General ordinary differential equations solver
#' Implements the Euler solution with a given start value and dynamic step size.
#' @param f ODE to solve
#' @param y_0 numerical, starting value f(0) = y_0
#' @param h numerical, step size
#' @param x_max numerical, when to stop the loop/numerical solution
ode_adaptive <- function(f, y_0 = 1, h = 0.01, x_max = 20, parms = NULL){
  # Matrix to hold x and y-values
  num_sol <- matrix(c(0, y_0), 1, 2)
  # Placeholder
  i <- 1
  
  # Repeat loop while the x-value is less than 20
  while (num_sol[i, 1] < x_max){
    # Update x and y
    x_next <- num_sol[i, 1] + h
    y_next <- num_sol[i, 2] + h * f(num_sol[i, 1], num_sol[i, 2], parms)
    
    # Add as new row to numerical solution matrix
    num_sol <- rbind(num_sol, c(x_next, y_next))
    i <- i + 1
  }
  # Return the finished numerical solution
  return(num_sol)
}



sq <- function(x, y, parms = NULL) {
  y^2
}

square <- function(t, y, parms) {
  dydx <- y^2
  return(list(dydx))  
}

cosine <- function(x, y, parms = NULL){
  cos(x)
}

cosine_deSolve <- function(x, y, parms){
  list(cos(x))
}
