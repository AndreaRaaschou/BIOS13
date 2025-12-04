# Functions used to find the minimum of a sine function with two unknown variables

#' Sinus function
f <- function(x,y) {
  x*sin(4*x) + 1.1*y*sin(2*y)
}

#' Creates a random population of pop_size chromosomes
#' Changed: decimal number between 0 and 10, not 0 or 1
Create_pop <- function(pop_size,gene_number) {   
  genes <- matrix(nrow=pop_size, ncol=gene_number)
  for(ind in 1:pop_size) {              # For each individual
    for(gene_index in 1:gene_number) {      # For each gene in the chromosome
      genes[ind, gene_index] = runif(1, min = 0, max = 10) # fill genes with random decimal number between 0 and 10
    }
  }
  fitness <- rep(NA,pop_size)
  # pop is a list, a structured variable with information about the population:
  pop <- list(size=pop_size, L=gene_number, genes=genes, fitness=fitness)
  return(pop) 
}

#' Let individuals with the highest fitness reproduce
#' Changed: use linear blending and produce three, not two offspring
Reproduce <- function(pop,num_breeed) {
  # The population reproduces in pairs, in fitness order
  # First individuals 1 and 2 mate, next individuals 3 and 4, and so on
  # Each pair produces 3 offspring
  # Offspring replace the parent population from the bottom of the list.
  
  offspring_pos <- pop$size # where to put the offspring, starting from bottom
  
  for(parent1 in seq(1,num_breeed,by=2)) {
    parent2 <- parent1 + 1
    
    for (gene_index in 1:pop$L){
      offsp1 <- 0.5 * pop$Genes[parent1, gene_index] + 0.5 * pop$Genes[parent2, gene_index]
      offsp2 <- 1.5 * pop$Genes[parent1, gene_index] - 0.5 * pop$Genes[parent2, gene_index]
      offsp3 <- -0.5 * pop$Genes[parent1, gene_index] + 1.5 * pop$Genes[parent2, gene_index]
      
      pop$Genes[offspring_pos,gene_index] <- offsp1
      pop$Genes[offspring_pos-1, gene_index] <- offsp2
      pop$Genes[offspring_pos-2, gene_index] <- offsp3
      }

    offspring_pos <- offspring_pos - 3 # Changed from 2 to 3 to account for three offspring
  }
  return(pop)
}

#' Calculate fitness for each individual
#' Changed: Put x and y values in sine function instead of adding binary numbers
Evaluate_fitness <- function(pop) {
  for(ind in 1:pop$size) {
    pop$fitness[ind] <- f(pop$genes[ind, 1], pop$genes[ind, 2])
  }
  return(pop)
}

#' Sort the population by fitness
#' Changes: put the smallest values instead of the largest values on top 
Sort <- function(pop) {
  # Find order of decreasing fitness:
  new_order <- order(pop$fitness, decreasing=FALSE)
  # Next, rearrange the population gene matrix and fitness vector:
  pop$genes <- pop$genes[new_order,]
  pop$fitness <- pop$fitness[new_order]
  return(pop)
}

#' Mutate the whole population, accept the top fitness individuals
#' Changed: produce new random numbers between 1 and 10
Mutate <- function(pop, mutation_free, mutation_rate) {
  for(ind in (mutation_free+1):pop$size) {
    for(gene_index in 1 : pop$L) {
      if(runif(1) <= mutation_rate) { # Sample random number between 0 and 1 following std distribution
        # Sample new random number for the current gene position
        pop$genes[ind, gene_index] = runif(1, min = 0, max = 10)
      }
    }
  }
  return(pop)
}
