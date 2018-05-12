rm(list = ls())

# Function call
source("/home/jinsung/Documents/Jinsung/2018_Research/Raffaele/Rcode/Interaction_Test/Interaction_Test.R")

### Input
# 1. Group A Odd Ratio
OR_A = 1.42
OR_A_CI_L = 0.60
OR_A_CI_R = 3.38

OR1 = c(OR_A,OR_A_CI_L,OR_A_CI_R)

# 2. Group B Odd Ratio
OR_B = 1.20
OR_B_CI_L = 0.60
OR_B_CI_R = 2.39

OR0 = c(OR_B,OR_B_CI_L,OR_B_CI_R)

### Analysis Results
Result = Interaction_Test(OR1, OR0)
