# Using the Hassel model to predict population growth for the Ringlets data

rm(list = ls())
dat <- read.csv("datasets/ringlets.csv")

# Remove the first data point - outlier
dat = dat[-1,]

nt <- dat$N[1:22]
nt_plus1 <- dat$N[2:23]

rt=log(nt_plus1/nt)

rt_fit = nls(rt ~ log(lambda) - b*log(1 + a*nt), data = list(nt=nt),
             start = list(lambda=1.1,a=0.001,b=1))
summary(rt_fit)

AIC(rt_fit) # AIC score: 21.3

# Plot model prediction against observed values
lambda_hat <- coef(rt_fit)["lambda"]
a_hat  <- coef(rt_fit)["a"]
b_hat  <- coef(rt_fit)["b"]

Nt_grid <- seq(min(nt), max(nt), length.out = 200)
rt_pred <- log(lambda_hat) - b_hat*log(1 + a_hat*Nt_grid)

plot(nt, rt, pch=16, col="black",
     xlab="Nt", ylab="rt",
     main="Observed rt and fitted Hassel function")
lines(Nt_grid, rt_pred, col="red", lwd=2)
legend("topright", legend=c("Data","Fit"), 
       col=c("black","red"), pch=c(16,NA), lty=c(NA,1))




