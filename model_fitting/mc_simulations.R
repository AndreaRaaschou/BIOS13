# Simulate the Ringlets dynamics 100 years into the future
rm(list = ls())
source("functions.R")

# List of estimated parameter values (r0, K, sd)
p = c(1.004, 311, 0.3445)

# Make multiple simulations
m <- mult_sim(300, p) 

# Plot the first 20 simulations
plot_first20(m[, 1:20])

# Calculate and print extinction risk for current parameter values
ext_risk <- extinction_risk(m)
print(paste("The extinction risk is:", ext_risk))

# Plot extinction risk for different parameter values
plot_extinction_risk(300, p)

