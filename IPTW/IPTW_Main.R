rm(list = ls())

###### Function call
source("/home/jinsung/Documents/Jinsung/2018_Research/Raffaele/Rcode/IPTW/IPTW_Anal.R")

###### Data Preprocessing
# 1. Input data
Data = read.csv('/home/jinsung/Documents/Jinsung/2018_Research/Raffaele/Rcode/Data/IPTW_Data_Entire.csv', sep = ',', header = T)

Data = as.matrix(Data)

# 2. parameters
No = nrow(Data)
Feature_No = ncol(Data)

# 3. Data Division
Feature = Data[,1:(Feature_No-3)]
Action = Data[,(Feature_No-2)]
Label = Data[,(Feature_No-1):(Feature_No)]


# 5. Dividing Group
idx1 = which(Feature[,20] == 1)
idx0 = which(Feature[,20] == 0)

# 6. Choosing one group
idx = idx0

Feature = Feature[idx,]
Action = Action[idx]
Label = Label[idx,]

Label = Label[,1]

# 7. Parameter Update
No = nrow(Feature)
Feature_No = ncol(Feature)

############## Analysis Result
Result = IPTW_Anal(Feature, Action, Label)

N0 = Result$N0
N1 = Result$N1
Outcome = Result$Outcome
Table = Result$Table


Output = matrix(0,23,5)
Output[1,1] = N1
Output[1,3] = N0
Output[2:21,1:5] = Table
Output[22,1] = Outcome[1,1]
Output[22,3] = Outcome[1,2]
Output[22,5] = Outcome[1,3]
Output[23,1:3] = Outcome[2,1:3]
Output[23,5] = Outcome[2,4]

#file_name = paste("/home/jinsung/Documents/Jinsung/2018_Research/Raffaele/Result/Outcome_2596_", (time_diff), ".csv", sep="")
#write.csv(Output, file_name)

