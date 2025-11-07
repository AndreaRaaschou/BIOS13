plotmax <- function(n){
  #Generate a random vector of integers
  v <- round(runif(n, min=1, max=20))

  max_val <- max(v)
  min_val <- min(v)
  max_ind <- which(v == max_val)
  min_ind <- which(v == min_val)
  max_list = list(max_ind, max_val)
  
  plot(1:n, v, type='l', col='blue')
  points(x = max_ind, 
         y = max_val,
         pch = 4, 
         col = "red") 
  
  return(max_list)
}




