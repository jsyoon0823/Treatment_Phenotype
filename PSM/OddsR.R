
OddsR <- function(X1,X0) {
  
  n00 = length(which(X0 == 0))
  n01 = length(which(X0 == 1))
  n10 = length(which(X1 == 0))
  n11 = length(which(X1 == 1))
  
  L = log( (n11 * n00) / (n01*n10))
  SE = sqrt( (1/n00) + (1/n01) + (1/n10) + (1/n11) )
  
  OR = exp(L)
  
  CIL = exp(L-1.96*SE)
  CIR = exp(L+1.96*SE)
  
  P_val = pnorm(-abs(L)/SE)
  
  Output = c(OR, CIL, CIR, P_val)
  
  return(Output)
  
}