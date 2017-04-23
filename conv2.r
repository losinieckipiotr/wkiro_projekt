conv2 <- function(A, B) {
  # Returns the two-dimensional convolution of matrices A and B.

  a_size <- dim(A)
  b_size <- dim(B)
  ma <- a_size[1]
  na <- a_size[2]
  mb <- b_size[1]
  nb <- b_size[2]
  
  mc <- max(ma+mb-1, ma, mb)
  nc <- max(na+nb-1, na, nb)
  
  C <- matrix(data = 0, nrow = mc, ncol = nc)
  for (j in 1:mc) {
    for (k in 1:nc) {
      for (p in 1:ma) {
        for (q in 1:na) {
          i_x <- j-p+1
          i_y <- k-q+1
          if ((i_x > 0) & (i_y > 0) & (i_x <= mb) & (i_y <= nb)) {
            C[j,k] <- C[j,k] + A[p,q]*B[i_x,i_y]
          }
        }
      }
    }
  }
  
  return(C)
}