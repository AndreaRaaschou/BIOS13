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

#' Let individuals with the highest fitness reproduce
#' The population reproduces in pairs, in fitness order
#' First individuals 1 and 2 mate, next individuals 3 and 4, and so on
#' Each pair produces 2 offspring, corresponding to two chromosomes after crossover
#' Offspring replace the parent population from the bottom of the list.
reproduce <- function(pop,num_breeed) {
  offspring_pos <- pop$size # where to put the offspring, starting from bottom
  
  for(parent1 in seq(1,num_breeed,by=2)) {
    parent2 <- parent1 + 1
    
    for (offspring in 1:2) { # create 2 offspring per pair of parents
    # crossover locus:
    crossover <- sample.int(pop$L,1) # Sample one random number between 1 and 5
    
    # Use crossover to choose what part of the solution to save from parent 1
    sol_p1 = pop$solutions[parent1, 1:crossover]          # Save the first part of parent1
    sol_p2 = setdiff(pop$solutions[parent2, ], sol_p1)    # Save cities that remain from parent2
    pop$solutions[offspring_pos, ] = c(sol_p1, sol_p2)    # Save offspring solution in pop
    
    offspring_pos <- offspring_pos - 1
    }
  }
  return(pop)
}

#' Function to calculate fitness for each individual
evaluate_fitness <- function(pop, distances) {
  for(ind in 1:pop$size) {
    # Use the distance matrix to calculate travel distance for each solution
    km = distances[1, pop$solutions[ind, 1] + 1]        # Lund to first city
    for (city in 1:(pop$L-1)){                          # distances between cities in solution
      km = km + distances[pop$solutions[ind, city] + 1, pop$solutions[ind, city + 1] + 1]  
    }
    km = km + distances[pop$solutions[ind, pop$L], 1]   # from last city back to Lund
    
    # Save distance as fitness
    pop$fitness[ind] <- km 
  }
  return(pop)
}

# Sort the population by fitness
sort <- function(pop) {
  # Find order of decreasing fitness:
  new_order <- order(pop$fitness, decreasing=FALSE)
  # Next, rearrange the population gene matrix and fitness vector:
  pop$solutions <- pop$solutions[new_order,]
  pop$fitness <- pop$fitness[new_order]
  return(pop)
}

# Mutate the whole population, accept the top fitness individuals
mutate <- function(pop, mutation_free, mutation_rate) {
  for(ind in (mutation_free+1):pop$size) {
    for(city_index in 1 : pop$L) {
      # Sample random number between 0 and 1 following std distribution
      # If number is below mutation_rate, mutate at the current position
      if(runif(1) <= mutation_rate) { 
        old = pop$solutions[ind, city_index]         # save old city
        new = sample.int(5, 1)                       # sample new city to replace the previous
        position = which(pop$solutions[ind,] == new) # find place of the new city to replace with the old city
        
        # Change place of the cities
        pop$solutions[ind, city_index] = new
        pop$solutions[ind, position] = old
      }
    }
  }
  return(pop)
}








