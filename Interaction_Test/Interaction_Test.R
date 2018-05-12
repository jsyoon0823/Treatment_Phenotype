Interaction_Test <- function(OR1, OR0) {
  
  OR_A = OR1[1]
  OR_A_CI_L = OR1[2]
  OR_A_CI_R = OR1[3]
  
  OR_B = OR0[1]
  OR_B_CI_L = OR0[2]
  OR_B_CI_R = OR0[3]
  
  ### Outputs
  Output = matrix(0,12,4)
  
  #### Two separate Analysis
  # 1. Odd Ratios
  Output[1,c(1,3)] = c(OR_A,OR_B)
  # 2. Log of Odd Ratios
  Output[2,c(1,3)] = round(c(log(OR_A),log(OR_B)),4)
  
  # 3. Odd Ratio CI
  Output[3,] = c(OR_A_CI_L, OR_A_CI_R, OR_B_CI_L, OR_B_CI_R)
  # 4. Log of Odd Ratio CI
  Output[4,] = round(log(Output[3,]),4)
  
  # 5. CI Width
  Output[5,c(1,3)] = c(Output[4,2] - Output[4,1], Output[4,4] - Output[4,3])
  # 6. CI Width / (1.96 * 2)
  Output[6,c(1,3)] = round(c(Output[5,1]/3.92, Output[5,3]/3.92),4)
  
  #### One Analysis
  # 7. Log Odd Width
  Output[7,1] = Output[2,1] - Output[2,3]
  # 8. CI Width Combine
  Output[8,1] = round(sqrt(Output[6,1]^2 + Output[6,3]^2),4)
  # 9. Odd Ratio Difference CI
  Output[9,c(1,2)] = round(c( Output[7,1] - 1.96 * Output[8,1], Output[7,1] + 1.96 * Output[8,1] ) ,4)
  
  #### P-value
  # 10. Z Value Computation & P-value Computation
  Output[10,1] = round(Output[7,1] / Output[8,1],4)
  if (Output[10,1] >= 0){
    Output[10,2] = round(1 - pnorm(Output[10,1]),4)
  } else {
    Output[10,2] = round(pnorm(Output[10,1]),4)
  }
  
  #### Exponential Outputs
  Output[11,1] = round(exp(Output[7,1]),4)
  Output[12,c(1,2)] = round(exp(Output[9,c(1,2)]),4)
  
  return (Output)
  
}

