# Useful functions:
# runif() uniform distribution, a vector of k random numbers
# rnorm() normal distribution
# sample() samples k elements from vector v
# sample.int() samples k numbers (integers) from 1:n

rm(list = ls())
source('functions.R')

# Simulate a queue
run_q(0.5, 0.5, 60)

# The Moran process 
run_moran(5, 0)
p_fixation(100, 50, 4)

