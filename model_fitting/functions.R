#' Function that takes the parameter values and current population size to estimate n_plus1
#' Use Ricker model and rnorm for stochasticity
#' @param nt current population size
#' @param p parameter estimates
#' @return following population size
calc_ntplus1 <- function(nt, p){
  r0 = p[1]
  K = p[2]
  sd = p[3]
  
  eps = rnorm(1, mean = 0, sd = sd)
  
  nt_plus1=nt*exp(r0*(1-nt/K) + eps)
  return(nt_plus1)
}

#' Simulates population size 100 years in the future using the last 
#' population size as start value.
#' @param n0 starting population size
#' @param p parameter estimates
one_sim <- function(n0, p){
  # Vector with initial value n0 and length 100
  s <- c(n0, rep(0, 99))
  
  for (i in 1:99){
    # Add next population size to m
    s = c(s, calc_ntplus1(s[i], p))
  }
  return(s)
}

#' Do 1000 simulations
#' Call plot function to plot the first 20 years
#' if (i == 20){
# Call plot function with the current array
mult_sim <- function(n0, p){
  # First simulation, create matrix where each simulation is one column
  m = matrix(one_sim(n0, p), ncol = 1)
  
  # Repeat and add more simulations 
  for (i in 2:1000){
    m = cbind(m, one_sim(n0, p))
  }
  
  plot_first20(m[, 1:20])
}

#' Plot the first 20 simulations in one single plot
plot_first20 <- function(m){
  plot(1, type = "n",      
       xlim = c(0, 100),   
       ylim = c(0, max(m)),
       xlab = "Years",
       ylab = "Population size",
       main = "20 simulations for 100 years of Ringlets population")
  
  for (i in 1:20){
    # loop through m to add lines for the different simulations
    lines(m[,i])
  }
}












