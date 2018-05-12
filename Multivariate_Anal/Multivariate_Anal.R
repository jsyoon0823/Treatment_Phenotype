Multivariate_Anal <- function(Feature, Label){
  
  # 1. Parameters
  No = nrow(Feature)
  Feature_No = ncol(Feature)
  
  # 2. Logistic Model
  Logit_model = glm(Label~Feature, family = "binomial")
  Output = coef(summary(Logit_model))
  
  # 3. Model Outputs
  Coef = Output[,1]
  SE = Output[,2]
  Pval = Output[,4]
  
  # 4. Output Analysis
  Result = matrix(0,(Feature_No),4)
  
  for (i in 1:(Feature_No)){
    Result[i,1] = round( exp ( Coef[i+1]), 2)
    Result[i,2] = round( exp ( Coef[i+1] - 1.96 * SE[i+1] ),2)
    Result[i,3] = round( exp ( Coef[i+1] + 1.96 * SE[i+1] ),2)
    Result[i,4] = round( pnorm ( - abs ( Coef[i+1] /  SE[i+1] ) ), 4)
  }
  
  return (Result)
}