# Simulate the Ringlets dynamics 100 years into the future
# Use last known population size as start value
# Use the parameter values of chosen model
# Run 1000 simulations
# Plot the first 20 years in one single plot
# Estimate risk of extinction (extinction = N < 2)
# Try to alter parameter values - see where the population becomes sensitive
source("functions.R")

# List of estimated parameter values 
p = c(r0 = 1.004, K = 311, sd = 0.3445)

mult_sim(300, p) # Something is very off with the simulations


# To do: 
# Check what is going wrong
# Make function that calculates extinction risk
# Also plot extinction risk

