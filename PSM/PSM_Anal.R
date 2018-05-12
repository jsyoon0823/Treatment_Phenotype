PSM_Anal <- function(Feature, Action, Label) {
  
  #### Function call
  source("/home/jinsung/Documents/Jinsung/2018_Research/Raffaele/Rcode/PSM/OddsR.R")
  source("/home/jinsung/Documents/Jinsung/2018_Research/Raffaele/Rcode/PSM/ChiTest.R")
  
  # 1. parameters
  No = nrow(Feature)
  Feature_No = ncol(Feature)
  thresh = 0.3
  
  # 2. Logistic Model
  Logit_model = glm(Action ~ Feature, family = "binomial")
  Tempo = as.numeric(Logit_model$fitted.values)
  
  # 3. Action division
  act_idx1 = which(Action == 1)
  act_idx0 = which(Action == 0)
  
  Act_Mat1 = cbind(act_idx1, Tempo[act_idx1])
  Act_Mat1 = as.matrix(Act_Mat1)
  Act_Mat0 = cbind(act_idx0, Tempo[act_idx0])
  Act_Mat0 = as.matrix(Act_Mat0)
  
  ###### Greedy Approach
  Row_Len = length(act_idx1)
  
  Cor_Result = matrix(0,Row_Len,3)
  
  for (it in 1:Row_Len){
    if (Row_Len == 1){
      New = matrix(0,1,2)
      New[1,1] = Act_Mat1[1]
      New[1,2] = Act_Mat1[2]
      Act_Mat1 = New
    }
    print(it)
    
    Temp = matrix(0,Row_Len,3)
    
    for (j in 1:Row_Len){
      
      Diff = abs(Act_Mat0[,2] - Act_Mat1[j,2])
      
      cor_idx = which.min(Diff)
      
      Temp[j,1] = Act_Mat1[j,1]
      Temp[j,2] = Act_Mat0[cor_idx,1]
      
      Temp[j,3] = min(Diff)
      
    }
    
    fin_idx = which.min(Temp[,3])
    Cor_Result[it,1] = Temp[fin_idx,1]
    Cor_Result[it,2] = Temp[fin_idx,2]
    Cor_Result[it,3] = min(Temp[,3])
    
    rem_idx1 = which(Act_Mat1[,1] == Temp[fin_idx,1])
    rem_idx0 = which(Act_Mat0[,1] == Temp[fin_idx,2])
    
    Act_Mat1 = Act_Mat1[-rem_idx1,]
    Act_Mat0 = Act_Mat0[-rem_idx0,]
    
    Row_Len = Row_Len - 1
    
  }
  
  # 5. Cut down
  cut_idx = which(Cor_Result[,3] > thresh)
  
  if (length(cut_idx)>0){
    Cor_Result = Cor_Result[-cut_idx,]
  }
  
  Feature_1 = Feature[Cor_Result[,1],]
  Feature_0 = Feature[Cor_Result[,2],]
  
  Label_1 = Label[Cor_Result[,1]]
  Label_0 = Label[Cor_Result[,2]]
  
  No = nrow(Cor_Result)
  Feature_No = ncol(Feature)
  
  
  #############Output Generation
  ##### 1. Outcome Analysis
  Outcome = matrix(0,2,5)
  Outcome[1,1] = length(which(Label_1 == 1))
  Outcome[1,2] = round(100* length(which(Label_1 == 1)) / No, 1)
  Outcome[1,3] = length(which(Label_0 == 1))
  Outcome[1,4] = round(100* length(which(Label_0 == 1)) / No, 1)
  Outcome[1,5] = ChiTest(Label_1, Label_0)
  
  Outcome[2,1:4] = OddsR(Label_1, Label_0)
  Outcome[2,4] = Outcome[2,4]*2
  
  ###### 2. Feature Analysis
  Output = matrix(0,Feature_No,5)
  
  for (i in 1:Feature_No){
    if ((i %in% c(1,13,14,15)) == 1){
      Output[i,1] = round(mean(Feature_1[,i]),1)
      Output[i,2] = round(sd(Feature_1[,i]),1)
      Output[i,3] = round(mean(Feature_0[,i]),1)
      Output[i,4] = round(sd(Feature_0[,i]),1)
      Output[i,5] = t.test(Feature_1[,i],Feature_0[,i])$p.value
    }
    
    if ((i %in% c(1,13,14,15)) == 0){
      
      Output[i,1] = length(which(Feature_1[,i] == 1))
      Output[i,2] = round(100* length(which(Feature_1[,i] == 1)) / nrow(Feature_1), 1)
      Output[i,3] = length(which(Feature_0[,i] == 1))
      Output[i,4] = round(100* length(which(Feature_0[,i] == 1)) / nrow(Feature_0), 1)
      Output[i,5] = ChiTest(Feature_1[,i], Feature_0[,i])
    }
    
  }
  
  # Output
  Final = list()
  Final$N = No
  Final$Outcome = Outcome
  Final$Table = Output
  
  return(Final)
  
}

