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
  
  nt_plus1 = nt * exp (r0 * (1 - nt/K) + eps)
  
  return(nt_plus1)
}

#' Simulates population size 100 years in the future using the last 
#' population size as start value.
#' @param n0 starting population size
#' @param p parameter estimates
one_sim <- function(n0, p){
  # Vector with initial value n0 and length 100
  s <- c(n0, rep(0, 99))
  
  for (i in 2:100){
    # Add next population size to m
    s[i] = calc_ntplus1(s[i-1], p)
    # Do not continue with the simulation if the population goes extinct
    if (s[i] < 2) break
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
  return(m)
}

#' Plot the first 20 simulations in one single plot
plot_first20 <- function(m){
  plot(1, type = "n",      
       xlim = c(0, 100),   
       ylim = c(0, max(m)),
       xlab = "Years",
       ylab = "Population size",
       main = "20 simulations for 100 years of Ringlets population")
  # Add a line for the extinction limit of 2
  abline(h = 2, col = 'red')
  
  for (i in 1:20){
    # loop through m to add lines for the different simulations
    lines(m[,i])
  }
}

#' Calculate extinction risk for the given matrix with simulations
extinction_risk <- function(m){
  count <- 0
  
  # Loop through all columns in m
  for (i in 1:ncol(m)){
    # If the population is below zero in the current simulation, add 1 to count
    if (any(m[,i] < 2)){
      count = count + 1
    }
  }
  # Return the proportion of simulations that went extinct (risk of extinction)
  return(count / ncol(m))
}

#' Plot extinction risk for different parameter values
plot_extinction_risk <- function(n0, p){
  # Change r0
  r0 = seq(1, 2, length.out = 100)
  r0_ext = rep(0, 100)
  for (i in 1:100){
    p[1] = r0[i]
    m = mult_sim(n0, p)
    r0_ext[i] = extinction_risk(m)
  }
  
  # Change K, change the starting population size to match K
  K = seq(10, 400, length.out = 100)
  K_ext = rep(0, 100)
  for (i in 1:100){
    p[2] = K[i]
    m = mult_sim(K[i], p)
    K_ext[i] = extinction_risk(m)
  }
  
  # Change sd
  sd = seq(0.2, 0.8, length.out = 100)
  sd_ext = rep(0, 100)
  for (i in 1:100){
    p[3] = sd[i]
    m = mult_sim(n0, p)
    sd_ext[i] = extinction_risk(m)
  }
  
  
  par(mfrow = c(1, 3))
  plot(r0, r0_ext, las = 0,
       xlab = "Intrinsic growth rate (r0)", 
       ylab = "Extinction risk")
  plot(K, K_ext, las = 0, 
       xlab = "Carrying capacity (K)", 
       ylab = "Extinction risk")
  plot(sd, sd_ext, las = 0, 
       xlab = "Standard deviation", 
       ylab = "Extinction risk")
}










