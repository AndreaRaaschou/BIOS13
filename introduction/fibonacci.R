#3 Plot the fibonacci sequence
fibonacci <- function(n){
  v <- c(1,1)
  for (i in 3:n){
    element <- v[i-2]+v[i-1]
    v <- append(v, element)
  } 
  print(v)
  print(n)
  print(length(v))
  plot(1:n, v)
}