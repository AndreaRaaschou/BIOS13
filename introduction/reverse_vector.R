reverse.vector <- function(v){
  v_rev <- c()
  for (i in 1:length(v)){
    v_rev <- append(v_rev, v[length(v)+1-i])
  }
  print(v_rev)
}


#reverse.vector <- function(v){
#  n <- length(v)
#  #in a loop, take the last element and put it in 
#  #the first place, then second place and so on
#  for (i in 1:n){
#    v <- append(v, v[n], after=i-1)
#  }
#  v <- v[1:n]
#  print(v)
#  return(v)
#}