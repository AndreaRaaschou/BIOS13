# Traveling salesman problem - NOT FINISHED
# Calculate the shortest distance by car between the following 
# Swedish cities: Lund, Kalmar, Stockholm, Karlstad, Göteborg, Jönköping.
rm(list = ls())
source("ts_functions.R")

distances = matrix(c(0,   267, 602, 506, 262, 282,
                     267,   0, 412, 467, 341, 214,
                     602, 412,   0, 305, 467, 321,
                     506, 467, 305,   0, 247, 243,
                     262, 341, 467, 247,   0, 149,
                     282, 214, 321, 243, 149,   0), 
                   nrow = 6, ncol = 6, byrow = TRUE)
# Add row and column names                     
cities <- c("Lund", "Kalmar", "Stockholm", "Karlstad", "Göteborg", "Jönköping")
rownames(distances) <- cities
colnames(distances) <- cities


# Main variables:
stops = cities[-1] # remove Lund from list of cities
num_stops = length(stops) # Number of stops to make - not accounting for start and stop in Lund
num_solutions = 50


max_generations <- 5000    # number of iterations
num_breeed <- 16         

# mutation operators
mutation_rate <- 0.01    # how large percentage of the stops experience mutations
mutation_free <- 10      # number of top ranked solutions that should be saved from mutation


##############################
# Main program starts here

pop <- create_pop(num_solutions, num_stops) 
pop <- evaluate_fitness(pop)  
pop <- sort(pop) 




