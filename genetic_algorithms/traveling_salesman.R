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
stops = cities[-1]        # remove Lund from list of cities
num_stops = length(stops) # Number of stops to make - not accounting for start and stop in Lund
num_solutions = 50
max_generations <- 500    # number of iterations
num_breeed <- 20        

# mutation operators
mutation_rate <- 0.01    # how large percentage of the stops experience mutations
mutation_free <- 10      # number of top ranked solutions that should be saved from mutation


##############################
# Main program starts here

pop <- create_pop(num_solutions, num_stops) 
pop <- evaluate_fitness(pop, distances)  
pop <- sort(pop) 

# Main loop over all of our functions

meantopfitness <- c(0,max_generations) # empty vectors to store results
meanfitness <- c(0,max_generations)

for(generation in 1:max_generations) {  
  pop <- reproduce(pop,num_breeed)
  pop <- mutate(pop, mutation_free, mutation_rate)
  pop <- evaluate_fitness(pop, distances)   # give back altered population with fitness
  pop <- sort(pop)
  
  # save statistics:
  meantopfitness[generation] <- mean(pop$fitness[1:5])
  meanfitness[generation] <- mean(pop$fitness)
}

cat("generation:",max_generations,"\n")
cat("mean top fitness:",meantopfitness[max_generations],"\n")
cat("mean fitness",meanfitness[max_generations],"\n")
cat("Best solution: ", pop$solutions[1, ], "\n")
cat("Fitness of best solution: ", pop$fitness[1], "\n")

# plot results
plot(1:max_generations, meanfitness, ylim=c(0,max(meantopfitness)),type="l")
lines( 1:max_generations, meantopfitness, col="red")


