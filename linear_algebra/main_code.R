setwd("/Users/andrearaaschou/courses/BIOS13/github/linear_algebra")

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

# 6: Matrix multiplication with scaling matrix
I <- diag(2)
m <- matrix(c(12,8),2,1)
0.5*I%*%m

# 7: Test if the eigenvalues of a scaling matrix are equal to the scaling constant
source("test_eigenvalues.R")
scaling_eigenvalues(I, 5)

# 8-13: Draw a tree and re-scale using scaling matrix
source("draw_a_tree.R")
draw_a_tree(0.5)
draw_transformed_tree(matrix(c(0.5,0,0,-0.9), 2,2))

# Rotation
theta <- 30/360*2*pi
A <- matrix(c(cos(theta),sin(theta),-sin(theta),cos(theta)), 2, 2)
draw_transformed_tree(A)

# Test if eigenvalues of some matrices are complex
complex_eigenvalues(A)
complex_eigenvalues(I)
?complex_eigenvalues()

# Combine scaling and rotation 
A_scaling <- matrix(c(0.5, 0, 0, 0.5), 2,2)
A_rotate <- matrix(c(cos(theta),sin(theta),-sin(theta),cos(theta)), 2, 2)
A_comb <- A_rotate %*% A_scaling
draw_tranformed_tree(A_comb)
