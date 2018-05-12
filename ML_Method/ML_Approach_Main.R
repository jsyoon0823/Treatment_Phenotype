rm(list = ls())

###### Function call
source("/home/jinsung/Documents/Jinsung/2018_Research/Raffaele/Rcode/ML_Method/ML_Approach_First.R")
source("/home/jinsung/Documents/Jinsung/2018_Research/Raffaele/Rcode/ML_Method/ML_Approach_Anal.R")
source("/home/jinsung/Documents/Jinsung/2018_Research/Raffaele/Rcode/ML_Method/ML_Approach_Second.R")
source("/home/jinsung/Documents/Jinsung/2018_Research/Raffaele/Rcode/ML_Method/ML_Approach_No_Further_Divide.R")

###### Data Preprocessing
# 1. Input data
Data = read.csv('/home/jinsung/Documents/Jinsung/2018_Research/Raffaele/Rcode/Data/IPTW_Data_8409.csv', sep = ',', header = T)
Data = as.matrix(Data)

# 2. parameters
No = nrow(Data)
Feature_No = ncol(Data)

# 3. Data Division
Feature = Data[,1:(Feature_No-3)]
Action = Data[,(Feature_No-2)]
Label = Data[,(Feature_No-1):(Feature_No)]

Label = Label[,1]

##### First Round
First_Output = ML_Approach_First(Feature, Action, Label)

Output = matrix(0,24,15)
Output[1,1] = First_Output$Final0$N1
Output[1,3] = First_Output$Final0$N0
Output[2:22,1:5] = First_Output$Final0$Table
Output[23,1] = First_Output$Final0$Outcome[1,1]
Output[23,3] = First_Output$Final0$Outcome[1,2]
Output[23,5] = First_Output$Final0$Outcome[1,3]
Output[24,1:3] = First_Output$Final0$Outcome[2,1:3]
Output[24,5] = First_Output$Final0$Outcome[2,4]

Output[1,1+5] = First_Output$Final1$N1
Output[1,3+5] = First_Output$Final1$N0
Output[2:22,6:10] = First_Output$Final1$Table
Output[23,1+5] = First_Output$Final1$Outcome[1,1]
Output[23,3+5] = First_Output$Final1$Outcome[1,2]
Output[23,5+5] = First_Output$Final1$Outcome[1,3]
Output[24,6:8] = First_Output$Final1$Outcome[2,1:3]
Output[24,10] = First_Output$Final1$Outcome[2,4]

Output[1:12,11:14] = First_Output$Inter_Result

write.csv(Output,file="/home/jinsung/Documents/Jinsung/2018_Research/Raffaele/Result/ML_Output_8409.csv")

##### Second Round
f = First_Output$Output[1]
thresh = First_Output$Output[2]
OR1 = First_Output$OR1
OR0 = First_Output$OR0

idx1 = which(Feature[,f]>=thresh)
idx0 = which(Feature[,f]<thresh)

Second_Output1 = ML_Approach_Second(Feature[idx1,], Action[idx1], Label[idx1], OR0)
Second_Output0 = ML_Approach_Second(Feature[idx0,], Action[idx0], Label[idx0], OR1)

### No Further Output
No_Further_Output1 = ML_Approach_No_Further_Divide(Feature[idx1,], Action[idx1], Label[idx1], OR0)
No_Further_Output0 = ML_Approach_No_Further_Divide(Feature[idx0,], Action[idx0], Label[idx0], OR1)

write.csv(No_Further_Output1,file="/home/jinsung/Documents/Jinsung/2018_Research/Raffaele/Result/NoFurther_8409_1.csv")
write.csv(No_Further_Output0,file="/home/jinsung/Documents/Jinsung/2018_Research/Raffaele/Result/NoFurther_8409_0.csv")
