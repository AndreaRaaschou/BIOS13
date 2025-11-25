# 4) Solve the logistic equation manually
rm(list = ls())
setwd('/Users/andrearaaschou/courses/BIOS13/github/dynamic_systems')
source('functions1.R')

# Initiate variables
num_sol <- matrix(c(0, 1), 1, 2) # num_sol has structure (t0, n0)
delta_t <- 0.01 
P <- list(r0=1, K=100) # Use the previous values for growth rate and carrying capacity
i <- 1 # Placeholder

# Repeat until reaching final t
while (num_sol[i, 1] < 20){
  # Calculate f(t,x), which is the tangent to the curve i am trying to solve at the current point
  h <- growth_function(n=num_sol[i,2], P=P)
  
  # Calculate delta_n = f(t,n)*delta_t
  delta_n <- h * delta_t
  
  # Update n and t
  t <- num_sol[i, 1] + delta_t
  n <- num_sol[i, 2] + delta_n
  
  # Add as new row to numerical solution matrix
  num_sol <- rbind(num_sol, c(t, n))
  i <- i + 1
}

plot(num_sol, type = 'l')

