#' Test if the eigenvalues of a scaling matrix are equal to its scaling constant
#' @param I diagonal matrix
#' @param c numerical value, scaling constant
#' @return Boolean value, true if all eigenvalues are equal to the c
scaling_eigenvalues <- function(I, c){
  E <- eigen(c*I)
  same <- TRUE
  
  # If the eigenvalue is not equal to the scaling constant - change flag
  if(any(E$values) != c) {
    same <- FALSE
  }
  return(same)
}

#' Test if the eigenvalues of a matrix are complex
#' @param A matrix to test the eigenvalues of
#' @return Boolean value, TRUE if the eigenvalues are complex, FALSE if they are real
complex_eigenvalues <- function(A){
  E <- eigen(A)
  complex <-  FALSE
  
  # If any eigenvalue has an imaginary part that is not 0, change flag (complex) to TRUE
  if (any(Im(E$values) != 0)){
    complex <- TRUE
  }
  return(complex)
}