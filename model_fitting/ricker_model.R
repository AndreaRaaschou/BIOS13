rm(list = ls())
dat <- read.csv("datasets/ringlets.csv")

# Create two vectors, one with values 1:23 and one with 2:24
nt <- dat$N[1:23]
nt_plus1 <- dat$N[2:24]

# Use the nno-linear least squares function to estimate r0 and K parameters
rickerfit = nls(nt_plus1 ~ nt*exp(r0*(1-nt/K)), 
              data = list(nt=nt),
              start = list(r0 = 1, K = 100))
summary(rickerfit)

# Plot nt against nt_plus 1
plot(nt, nt_plus1)
# Add predicted values frome stimated rickerfunction
points(nt, fitted(rickerfit), col = 'red')

# Plot residuals against population size
plot(nt, residuals(rickerfit))

# Calculate the log population growth rate (rt)
rt=log(nt_plus1/nt)
plot(nt, rt)

# Remove outlier from first year of growth 
dat = dat[-1, ]

# Recalculate everything above without outlier
nt <- dat$N[1:22]
nt_plus1 <- dat$N[2:23]

# Use the no-linear least squares function to estimate r0 and K parameters
rickerfit = nls(nt_plus1 ~ nt*exp(r0*(1-nt/K)), 
                data = list(nt=nt),
                start = list(r0 = 1, K = 100))
summary(rickerfit)

rt=log(nt_plus1/nt)
plot(nt, rt)

# use nls to predict rt
rt_fit = nls(rt ~ r0*(1-nt/K), data = list(nt=nt),
             start = list(r0 = 1, K = 100))
summary(rt_fit)

AIC(rt_fit) # AIC score: 19.5

# Plot residuals against nt (population size)
plot(nt, residuals(rt_fit))

# Plot nt against nt_plus 1
plot(nt, fitted(rt_fit))
# Add predicted values frome stimated rickerfunction
points(nt, fitted(rt_fit), col = 'red')

# Extract estimates
r0_hat <- coef(rt_fit)["r0"]
K_hat  <- coef(rt_fit)["K"]

# Predicted curve
Nt_grid <- seq(min(nt), max(nt), length.out = 200)
rt_pred <- r0_hat * (1 - Nt_grid / K_hat)

# Plot the fitted curve over the data
plot(nt, rt, pch=16, col="black",
     xlab="Nt", ylab="rt",
     main="Observed rt and fitted Ricker function")
lines(Nt_grid, rt_pred, col="red", lwd=2)
legend("topright", legend=c("Data","Fit"), 
       col=c("black","red"), pch=c(16,NA), lty=c(NA,1))




