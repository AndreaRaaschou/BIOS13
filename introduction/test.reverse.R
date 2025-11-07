source('reverse_vector.R')
u <- runif(n=10, min=1, max=20)
v <- reverse.vector2(u)
plot(1:10,v)

#Testing the second version of the reverse.vector fucntion
u <- c(1,2,3,4,5,6,7)
v <- reverse.vector2(u)
