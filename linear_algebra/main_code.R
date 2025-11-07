# Initiate vectors
u <- c(3,6,7)
v <- c(12,13,14)

# Ways of calculating the norm, second option requires the vector to be in matrix format
sqrt(sum((u+v)*(u+v)))
norm(as.matrix(u+v), type = "2")

# Initiate a 3x3 matrix
B <- matrix(c(1,2,3,0,0,0,4,5,6), 3,3)

# Reshape u to be a 3x1 column vector/matrix (required for multiplication etc with B)
dim(u) <- c(3,1)

# Matrix multiplication with column vector
B %*% u        

X <- matrix(c(1,1,2,0), 2, 2)
det_X <- det(X)

# Eigenvalues and eigenvectors
E <- eigen(X)
eigenvectors <- E$vectors
eigenvalues <- E$values

print(eigenvalues)
print(class(eigenvalues))

if(eigenvalues[1]*eigenvalues[2] == det_X) print("The product of the eigenvalues is equal to the det of X.")
