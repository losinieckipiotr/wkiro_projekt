conv2vec <- function(u, v, A) {
  # Returns matrix which is equvalent to:
  #   Convolution each column of A with the vector u, and then convultion each row of the result with the vector v.
  
  B =  u %*% v
  source('conv2.r')
  C <- conv2(B, A)
  
  return(C)
}