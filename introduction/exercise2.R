install.packages('ggplot2')
install.packages('tidyverse')
install.packages('tidyr')
library('ggplot2')
library('tidyverse')
library('tidyr')
v <- seq(1,10, by=1)
v2 <- v*3
for (i in v2){
  print(i)
}

m <- c("a", "b", "f")

for (char in m){
  print(char)
}

#1
mymax <- function(num1, num2){
  if (num1>num2){
    print(num1)
    return(num1)
  }
  else {
    print(num2)
    return(num2)
  }
}
result <- mymax(3,1)

#2 Calculate the weighted sum of an input vector
weighted_sum <- function(v, w){
  return(sum(v*w))
}

#3
source('fibonacci.R')
fibonacci(10)


source('reverse_vector.R')
reverse.vector(c(1,2,3,4,5))

#4
source('plotmax.R')
returns <- plotmax(10)

#5
source('palindrome.R')
is_palindrome('name')

#6
source('sort_vectors.R')
v <- sort_vectors(c(1,2,3), c(12, 33, 7))

#7
x <- seq(-10,10, by=0.02)
y_sin <-  sin(x)
y_cos <-  cos(x)

data <- data.frame(x, y_sin, y_cos)

my_plot <- ggplot(data, aes(x))+
  geom_line(aes(y=y_sin), color='red')+
  geom_line(aes(y=y_cos), color='blue')
show(my_plot)

data_long <- pivot_longer(data, cols = c(y_sin, y_cos),
                          names_to = "function",
                          values_to = "value")

ggplot(data_long, aes(x, value)) +
  geom_line(aes(color = function)) #+ 
  #facet_wrap(~ function, ncol = 1) + 
  #labs(y = "Value", color = "Function") +
  #theme_minimal()










