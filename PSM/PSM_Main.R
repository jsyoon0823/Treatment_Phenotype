rm(list = ls())

######## function call
source("/home/jinsung/Documents/Jinsung/2018_Research/Raffaele/Rcode/PSM/PSM_Anal.R")

######## Data Preprocess
# 1. Input Data
#Data = read.csv('/home/jinsung/Documents/Jinsung/2018_Research/Raffaele/Rcode/Data/IPTW_Data.csv', sep = ',', header = T)
Data = read.csv('/home/jinsung/Documents/Jinsung/2018_Research/Raffaele/Rcode/Data/IPTW_Data_Entire.csv', sep = ',', header = T)
Data = as.matrix(Data)

# 2. parameters
No = nrow(Data)
Feature_No = ncol(Data)

# 3. Data Division
Feature = Data[,1:(Feature_No-3)]
Action = Data[,(Feature_No-2)]
Label = Data[,(Feature_No-1):(Feature_No)]

# 4. Additional Datasets
Onset = read.csv('/home/jinsung/Documents/Jinsung/2018_Research/Raffaele/Rcode/Data/Onset2.csv', sep = ',', header = T)
Onset = as.matrix(Onset)

# 5. Data Combine
# Feature = cbind(Feature, Onset)

# 6. Dividing Group
idx1 = which(Feature[,10] >= 0.5)
idx0 = which(Feature[,10] < 0.5)

idx = idx0

Feature = Feature[idx,]
Action = Action[idx]
Label = Label[idx,]

# 7. Parameter Update
No = nrow(Feature)
Feature_No = ncol(Feature)

# 8. Label Define
Label = Label[,1]

########## Output
Result = PSM_Anal(Feature, Action, Label)

N = Result$N
Outcome = Result$Outcome
Table = Result$Table

