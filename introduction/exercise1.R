v <- seq(0,5, by=0.2)
v2 <- round(v, digits = 0)
plot(v)
lines(v2)

#26
a <- c(1,2,3)
b <- c(11,12,13)

for (i in 1:length(a)){
  if(i==1) {a2 <-a[i]
  a2 <- append(a2, 0)}
  else {a2 <- append(a2, a[i])
  a2 <- append(a2, 0)}
  }
a2

for (i in 1:length(b)){
  if(i==1) {
    b2 <- 0
    b2 <-append(b2, b[i])}
  else {
    b2 <- append(b2, 0)
    b2 <- append(b2, b[i])}
  }
b2

for (i in 1:(length(a)+length(b))){
  if (i==1) d <- a2[1]
  else if ((i %% 2) == 0) d <- append(d,b2[i])
  else d <- append(d,a2[i])
}
d


#28
vowels <- c("a", "e", "i", "o", "u")

print("Write a line of text (English) \n")
line <- scan(what = character(),nmax = 1, sep = "\n")

line <- readline("Write a line of text (English)")

for (char in strsplit(line, split="")[[1]]){
  if (char %in% vowels){
    print(char)
  }
}

#29
secret_num <- sample.int(20,1)
guess <-  TRUE
count <-  0
while (guess){
  count <- count +1
  guess_num <- readline("Guess a number between 1 and 20: ")
  if (guess_num == secret_num){
    print(paste("Congratz, you have guessed the correct number! You needed", count, "guesses."))
    guess <- FALSE
  }
  else{
    print("Wrong number, try again.")
  }
}
 






