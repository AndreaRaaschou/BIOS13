logisticGrowth <- function(t, n, P) {
  dndt <- P$r0 * n * ( 1 - n / P$K )
  return(list(dndt))
}

#' Normal logistic equation for population growth
#' @param n numerical value, population size
#' @param P list with r(per capita growth rate) and K (carrying capacity) values
growth_function <- function(n, P){
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



