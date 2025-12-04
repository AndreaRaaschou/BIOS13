# Visual representation of the function to solve
f <- function(x,y) x*sin(4*x)+1.1*y*sin(2*y)
x <- seq(0,10, length=101)
y <- seq(0,10, length=101)
z <- outer(x,y,f) # calculate f(x,y) for each value of x and y
persp(x,y,z,theta=30, phi=30, expand=0.6, ticktype='detailed') 

