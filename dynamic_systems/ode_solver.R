# 5) Write a general ode-solver, base it on the code for (4)
# Not finished, lots of bugs
rm(list = ls())

#' General ordinary differential equations solver
#' @param f function
ode_solver <- function(f){
  # Implement the Euler solution
  num_sol <- matrix(c(0, 1), 1, 2)
  delta_t <- 0.01 
  i <- 1 # Placeholder
  
  while (num_sol[i, 1] < 20){
    # Calculate f(t,x), which is the tangent to the curve i am trying to solve at the current point
    h <- f(num_sol[i,2]) # ERROR - should take two arguments f(t,n)
    
    # Calculate delta_n = f(t,n)*delta_t
    delta_n <- h * delta_t
    
    # Update n and t
    t <- num_sol[i, 1] + delta_t
    n <- num_sol[i, 2] + delta_n
    
    # Add as new row to numerical solution matrix
    num_sol <- rbind(num_sol, c(t, n))
    i <- i + 1
  }
  # implement dynamic step size - makes the solver more efficient while keeping accuracy
  # (search for runge-kutta-fehlberg (RK45) method)
  
  # Return the finished numerical solution
  return(num_sol)
}

# Declare some functions to test
square <- function(t, n, parms) {
  list(2*n)  
}

# compare accuracy and speed with R’s ODE solver:
test <- ode_solver(square)
#test_sin <- ode_solver(sin)

out <- ode(y = 1, func = square,
           times = seq(1, 100), parms = 2)
plot(test_sin)
plot(out)
