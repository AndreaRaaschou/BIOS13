sort_vectors <- function(v1, v2){
  #Merge the two input vectors 
  v <- c(v1,v2)
  n <- length(v)
  unsorted = TRUE
  #use bubblesort
  #In a loop: compare the last two elements, then the second to last two elements and so on
  #Repeat until no elements change places - then break the loop
  #Keep track of this using a boolean flag
  
  while (unsorted){
    unsorted = FALSE
    for (i in 1:(n-1)){
      if (v[n-i]>v[n-i+1]){
        print(i)
        temp <- v[n-i]
        v[n-i] <- v[n-i+1]
        v[n-i+1] <- temp
        unsorted = TRUE
      }
    }
  }
  return(v)
}