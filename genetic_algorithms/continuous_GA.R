# Continuous genetic algorithm, modified from binary_GA
rm(list = ls())
source('functions.R')

# Main variables:
pop_size <- 50            # pop size is number of chromosomes in the population
gene_number <- 2         # gene number is number of genes in each chromosome, should be one in these simple tasks
max_generations <- 5000    # number of iterations
num_breeed <- 16          # Changed from 20 to 16, out of the 50 chromosomes (= possible solutions) the 20 best are allowed to reproduce

# genetic operators
mutation_rate <- 0.01    # how large percentage of the genes experience mutations
mutation_free <- 10      # number of top ranked individuals that should be saved from mutation

##############################
# Main program starts here

pop <- Create_pop(pop_size,gene_number)  # calls function that generates random chromosomes
pop <- Evaluate_fitness(pop)   # calculate fitness for all individuals
pop <- Sort(pop) # Sort according to fitness

# empty vectors to store results
meantopfitness <- c(0,max_generations) 
meanfitness <- c(0,max_generations)
generations_needed = max_generations

# main loop over all of our functions
for(generation in 1:max_generations) {  
  pop <- Reproduce(pop,num_breeed)
  pop <- Mutate(pop, mutation_free, mutation_rate)
  pop <- Evaluate_fitness(pop)   # give back altered population with fitness
  pop <- Sort(pop)
  
  # save statistics:
  meantopfitness[generation] <- mean(pop$fitness[1:5])
  meanfitness[generation] <- mean(pop$fitness)
  
  # stop the loop if the minimum point has been found
  if (pop$fitness[1] <= -18.4){
    cat(generation, " generations were required to solve the problem. \n")
    generations_needed = generation
    break
  }
}

cat("mean top fitness:",meantopfitness[generations_needed],"\n")
cat("mean fitness",meanfitness[generations_needed],"\n")

# plot results
plot(1:generations_needed, meanfitness, ylim=c(0,max(pop$fitness[1])),type="l")
lines( 1:generations_needed, meantopfitness, col="red")

cat("Minimum value of the sine function: ", pop$fitness[1], "\n")
cat("x and y-values at this point \n")
cat("x: ", pop$genes[1, 1], "\n")
cat("y: ", pop$genes[1, 2], "\n")








