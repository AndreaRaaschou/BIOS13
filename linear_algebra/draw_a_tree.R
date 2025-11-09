#' @param c numeric value, scaling constant
draw_a_tree <- function(c) {
  # x-coordinates of nodes in the tree
  # (NA values creates breaks in the line, 'lifting the pen'):
  x <- c(0, 0, 0.7, 1.5, NA, 0.7, 0.8, NA, 0, -0.6, -1.2, NA, -0.6, -
           0.5)
  
  # y-coordinates:
  y <- c(0, 1, 1.3, 1.4, NA, 1.3, 1.8, NA, 1.0, 1.4, 1.7, NA, 1.4,
         2.0)
  
  # Plot a brown tree, with thick branches
  plot(x, y, type='l', col='brown', lwd=8, xlim=c(-2,2), ylim=c(-2,2)) 
  
  # If the coordinates have been re-scaled - add the re-scaled three in green to the same plot
  if (c != 1){
    x <- c*x
    y <- c*y
    # Add a green tree to the current plot
    lines(x, y, type='l', col='green', lwd=8)
  }
}

#' Re-scales the original parameters using a scaling matrix A  
#' @param A 2x2 scaling matrix
draw_transformed_tree <- function(A){
  # x-coordinates
  x <- c(0, 0, 0.7, 1.5, NA, 0.7, 0.8, NA, 0, -0.6, -1.2, NA, -0.6, -
           0.5)
  # y-coordinates
  y <- c(0, 1, 1.3, 1.4, NA, 1.3, 1.8, NA, 1.0, 1.4, 1.7, NA, 1.4,
         2.0)
  
  # Plot a brown tree, without scaling
  plot(x, y, type='l', col='brown', lwd=8, xlim=c(-2,2), ylim=c(-2,2)) 
  
  for (i in 1:length(x)) {
    # original coordinate vector:
    u <- c(x[i],y[i])
    # transformed vector:
    v <- A %*% u
    # replace the original coordinates with the transformed ones:
    x[i] <- v[1]
    y[i] <- v[2]
  }
  # Add a green tree to the current plot
  lines(x, y, type='l', col='green', lwd=8)
}