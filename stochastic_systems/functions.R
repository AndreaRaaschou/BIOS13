#' Model of a standard queue
#' Every minute, a new customer is added to the queue with 
#' probability Pin. Also every minute, a customer is finished 
#' with probability Pout and leaves the queue
run_q <- function(pin, pout, time){
  queue = rep(0, time)
  for (i in 1:(time-1)){
    # Vector with two random numbers drawn between 0 and 1
    r = runif(2, min = 0, max = 1)
    # below pin = person enters, below pout: person leaves
    # if one person enters and no person leaves the queue: add one
    if (r[1] < pin && r[2] > pout){ 
      # add a person to the queue
      queue[i+1] = queue[i] + 1
    }
    # if no person enters and one person leaves the queue: subtract one
    else if (r[1] > pin && r[2] < pout && queue[i] > 0){
      queue[i+1] = queue[i] - 1
    }
    # if one person enters and one person leaves the queue: no change
    # if no person enters and no person leaves the queue: no change
    else {
      queue[i+1] = queue[i]
    }
  }
  # Plot the length of the queue over time
  plot(queue, las = 1, type = 'l',
       ylab = "people in the queue", 
       xlab = "time (min)")
}

#' Simulate the Moran process
#' Start with a single copy of A and returns the final number of A's
#' @param tmax numeric, the amount of stochastic steps
#' @param s numeric, the added probability of choosing A
run_moran <- function(tmax, s){
  # Start with a single copy of A in a vector with 6 letters
  vec = c('A', 'B', 'B', 'B', 'B', 'B')
  
  for (i in 1:tmax){
    # Sample one random position
    remove = sample(1:6, 1)
    # Get all elements except the element in position remove
    vec = vec[-remove]
    
    # Set weights for the current population vec
    weights = rep(1, length(vec))
    for (ind in 1:length(vec)){
      if (vec[ind] == 'A'){
        weights[ind] = 1+1
      }
    }
    
    # Take one random letter in the vector and replace the previous one with the new one
    replace = sample(1:5, 1, prob = weights) 
    vec = append(vec, vec[replace], after = remove-1) 
  }
  # Return the final number of A's
  return(sum(grepl('A', vec)))
}

#' Uses the run_moran function to iterate the Moran process
#' repeats times and returns the probability of fixation of A
#' @param repeats numeric, the amount of times the run_moran() will be tested
#' @param tmax numeric, the amount of stochastic steps
#' @param s numeric, the added probability of choosing A
p_fixation <- function(repeats, tmax, s){
  vec = rep(0, repeats)
  for (i in 1:repeats){
    num_a = run_moran(tmax, s)
    # If a is fixed, change the flag in vec to 1
    if (num_a == 6){
      vec[i] = 1
    }
  }
  # fixed / repeats = probability of fixation
  return(sum(grepl('1', vec)) / repeats)
}






