ChiTest <- function(X1,X0) {
  
  N1 = length(X1)
  N0 = length(X0)
  
  n1 = length(which(X1 == 1))
  n0 = length(which(X0 == 1))
  
  p0 = (n1 + n0) / (N1 + N0)
  
  n10 = N1 * p0
  n00 = N0 * p0
  
  observed = c(n1, N1 - n1, n0, N0 - n0)
  expected = c(n10, N1 - n10, n00, N0 - n00)
  
  Z = sum( ( observed - expected )^2 / expected )
  P_val = 1 - pchisq(Z, 1)
  
  return(P_val)
  
}