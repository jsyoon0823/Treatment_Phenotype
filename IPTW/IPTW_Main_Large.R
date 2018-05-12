rm(list = ls())

###### Function call
source("/home/jinsung/Documents/Jinsung/2018_Research/Raffaele/Rcode/IPTW/IPTW_Anal.R")

###### Data Preprocessing
# 1. Input data
Data = read.csv('/home/jinsung/Documents/Jinsung/2018_Research/Raffaele/Rcode/Data/IPTW_Data_8409.csv', sep = ',', header = T)
Data = as.matrix(Data)

# 2. parameters
No = nrow(Data)
Feature_No = ncol(Data)

for (time_diff in 1:9){
  
  # 3. Data Division
  Feature = Data[,1:(Feature_No-3)]
  Action = Data[,(Feature_No-2)]
  Label = Data[,(Feature_No-1):(Feature_No)]
  
  # 5. Dividing Group
  
  if (time_diff == 1){
    idx = which(Feature[,19] == 1)
  } else if (time_diff == 2){
    idx = which(Feature[,20] == 1)
  } else if (time_diff == 3){
    idx = which(Feature[,21] == 1)
  } else if (time_diff == 4){
    idx = which(Feature[,19] == 0)
  } else if (time_diff == 5){
    idx = which(Feature[,20] == 0)
  } else if (time_diff == 6){
    idx = which(Feature[,21] == 0)
  } else if (time_diff == 7){
    idx1 = which(Feature[,19] == 0)
    idx2 = which(Feature[,20] == 1)
    idx = intersect(idx1, idx2)
  } else if (time_diff == 8){
    idx1 = which(Feature[,19] == 0)
    idx2 = which(Feature[,21] == 1)
    idx = intersect(idx1, idx2)
  } else if (time_diff == 9){
    idx1 = which(Feature[,20] == 0)
    idx2 = which(Feature[,21] == 1)
    idx = intersect(idx1, idx2)
  } 
  
  Feature = Feature[idx,]
  Action = Action[idx]
  Label = Label[idx,]
  
  Label = Label[,1]
  
  # 7. Parameter Update
  No = nrow(Feature)
  
  ############## Analysis Result
  Result = IPTW_Anal(Feature, Action, Label)
  
  N0 = Result$N0
  N1 = Result$N1
  Outcome = Result$Outcome
  Table = Result$Table
  
  
  Output = matrix(0,24,5)
  Output[1,1] = N1
  Output[1,3] = N0
  Output[2:22,1:5] = Table
  Output[23,1] = Outcome[1,1]
  Output[23,3] = Outcome[1,2]
  Output[23,5] = Outcome[1,3]
  Output[24,1:3] = Outcome[2,1:3]
  Output[24,5] = Outcome[2,4]
  
  file_name = paste("/home/jinsung/Documents/Jinsung/2018_Research/Raffaele/Result/Outcome_8409_", (time_diff), ".csv", sep="")
  write.csv(Output, file_name)
}
