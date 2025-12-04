# Functions that I need:
# Create population, with chromosomes with different orders of cities
# Calculate fitness using the distance matrix - remember to always start and finish in Lund


#' Creates a random population of solutions
create_pop <- function(num_solutions, num_stops) {   
  solutions <- matrix(nrow = num_solutions, ncol = num_stops)
  
  for(ind in 1:num_solutions) {              # For each solution    
      solutions[ind,] = sample.int(5, 5)     # sample random order (1-5)
  }
  fitness <- rep(NA,num_solutions)
  pop <- list(size = num_solutions, 
              L = num_stops, 
              solutions = solutions, 
              fitness = fitness)
  return(pop) 
}

