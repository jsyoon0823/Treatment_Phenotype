IPTW_Anal <- function(Feature, Action, Label){
  
  # 1. parameters
  No = nrow(Feature)
  Feature_No = ncol(Feature)
  thresh = 0.1
  
  ########## Model
  # 1. X, Y, Z, N Setting
  Y = Label
  X = Feature
  N = No
  Z = Action
  
  # 2. Group Number
  N1 = length(which(Z == 1))
  N0 = length(which(Z == 0))
  
  # 3. Logistic Model
  Logit_model = glm(Z ~ X, family = "binomial")
  e = as.numeric(Logit_model$fitted.values)
  
  # 4. Weight Define
  w1 = Z/e
  w0 = (1-Z)/(1-e)
  
  ## 5. Adjust weights (w = Z/e * Pr(Z=1) + (1-Z)/(1-e) * Pr(Z=0))
  #P1 = N1/N
  #P0 = N0/N
  #w1 = w1 * P1
  #w0 = w0 * P0
  
  # 6. Remove the endpoint
  idx1 = which(w1 > 1/thresh)
  idx0 = which(w0 > 1/thresh)
  
  idx = union(idx1,idx0)
  
  N = N - length(idx)
  X = X[-idx,]
  Z = Z[-idx]
  Y = Y[-idx]
  w1 = w1[-idx]
  w0 = w0[-idx]
  
  ############# Outcome Analysis
  Outcome = matrix(0,2,4)
  
  M1 = (w1) %*% Y / sum(w1)
  M0 = (w0) %*% Y / sum(w0)
  
  Outcome[1,1] = round(100 * M1, 1)
  Outcome[1,2] = round(100 * M0, 1)
  
  # P-value
  S1 = (sum(w1) / (sum(w1)^2 - w1 %*% w1)) * w1 %*% ((Y - M1)^2)
  S0 = (sum(w0) / (sum(w0)^2 - w0 %*% w0)) * w0 %*% ((Y - M0)^2)
  
  Sp = sqrt( ( (N1 - 1)*S1 + (N0 - 1)*S0 ) / (N1 + N0 - 2) )
  t = (M1 - M0) / (Sp * sqrt( (1/N0) + (1/N1) ) )
  Outcome[1,3] = round(2 * pnorm(-abs(t)),4)
  
  # Odds Ratio
  n00 = N0 * (1-M0)
  n01 = N0 * (M0)
  n10 = N1 * (1-M1)
  n11 = N1 * (M1)
  
  # Mean / Var
  L = log( (n11 * n00) / (n01*n10) )
  SE = sqrt( (1/n00) + (1/n01) + (1/n10) + (1/n11) )
  
  OR = exp(L)
  
  CIL = exp(L - 1.96 * SE)
  CIR = exp(L + 1.96 * SE)
  
  P = 2 * pnorm(-abs(L)/SE)
  
  Outcome[2,] = round(c(OR, CIL, CIR, P),4)
  
  ############# Feature Outcome
  Feature_Table = matrix(0,Feature_No,5)
  
  for (i in 1:Feature_No){
    Feature_Table[i,1] = round(w1 %*% X[,i] / sum(w1),3)
    Feature_Table[i,3] = round(w0 %*% X[,i] / sum(w0),3)
    
    Feature_Table[i,2] = ( sum(w1) / ( ( sum(w1) )^2 - w1 %*% w1 )) * w1 %*% ( (X[,i] - Feature_Table[i,1])^2 )
    Feature_Table[i,4] = ( sum(w0) / ( ( sum(w0) )^2 - w0 %*% w0 )) * w0 %*% ( (X[,i] - Feature_Table[i,3])^2 )
    
    Sp = sqrt( ( (N1 - 1)*Feature_Table[i,2] + (N0 - 1)*Feature_Table[i,4] ) / (N1 + N0 - 2) )
    t = (Feature_Table[i,1] - Feature_Table[i,3]) / (Sp * sqrt( (1/N0) + (1/N1) ) )
    
    Feature_Table[i,2] = round(sqrt(Feature_Table[i,2]),1)
    Feature_Table[i,4] = round(sqrt(Feature_Table[i,4]),1)
    
    Feature_Table[i,5] = round(2 * pnorm(-abs(t)),4)
    
  }
  
  Final = list()
  Final$N1 = N1
  Final$N0 = N0
  Final$Outcome = Outcome
  Final$Table = Feature_Table
  
  return (Final)
  
}
