rm(list = ls())

##### Function call
source("/home/jinsung/Documents/Jinsung/2018_Research/Raffaele/Rcode/Multivariate_Anal/Multivariate_Anal.R")

######## Data Preprocess
# 1. Input Data
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
ASA = read.csv('/home/jinsung/Documents/Jinsung/2018_Research/Raffaele/Rcode/Data/ASA.csv', sep = ',', header = T)
ASA = as.matrix(ASA)

Onset = read.csv('/home/jinsung/Documents/Jinsung/2018_Research/Raffaele/Rcode/Data/Onset2.csv', sep = ',', header = T)
Onset = as.matrix(Onset)
Onset = abs(1-Onset)

TIMI = Label[,2]

# 5. Adding Features
Feature = cbind(Feature,TIMI)

# 6. Parameter Update
No = nrow(Feature)
Feature_No = ncol(Feature)

# Feature
Feature[,1] = Feature[,1]/10

# 7. Label Define
Label = Label[,1]

# 8. Combine action and feature
Feature = cbind(Action,Feature)

########## Analysis Result
Result = Multivariate_Anal(Feature, Label)
