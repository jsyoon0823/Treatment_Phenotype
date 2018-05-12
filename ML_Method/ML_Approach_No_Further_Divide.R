ML_Approach_No_Further_Divide <- function(Feature, Action, Label, Ori_OR){
  
  ###### Function call
  source("/home/jinsung/Documents/Jinsung/2018_Research/Raffaele/Rcode/IPTW/IPTW_Anal.R")
  source("/home/jinsung/Documents/Jinsung/2018_Research/Raffaele/Rcode/Interaction_Test/Interaction_Test.R")
  
  # 1. Parameter update
  No = nrow(Feature)
  Feature_No = ncol(Feature)
  
  ############### Division
  Output = matrix(0,1,11)
  
  for (f in 1:Feature_No){
    print(f)
    Temp = Feature[,f]
    if (length(intersect(Temp,Temp))<=2) {
      
      idx1 = which(Temp==1)
      idx0 = which(Temp==0)
      
      if (min(length(idx1), length(idx0)) > 10) {
        
        Final1 = IPTW_Anal(Feature[idx1,], Action[idx1], Label[idx1])
        Final0 = IPTW_Anal(Feature[idx0,], Action[idx0], Label[idx0])
        
        OR1 = Final1$Outcome[2,1:3]
        OR0 = Final0$Outcome[2,1:3]
        
        if (sum(is.na(OR1))+sum(is.na(OR0))==0){
          Inter_Result = Interaction_Test(OR1, OR0)
          Z_Val = Inter_Result[10,1]
          P_Val = Inter_Result[10,2]
          
          P_Val1 = Interaction_Test(OR1, Ori_OR)[10,2]
          P_Val0 = Interaction_Test(OR0, Ori_OR)[10,2]
          
          Tempo = c(f,0.5,OR0,OR1,P_Val, P_Val0, P_Val1)
          Output = rbind(Output,Tempo)
          
        } else if (sum(is.na(OR1))==0){
          Tempo = c(f,0.5,NaN,NaN,NaN,OR1,NaN,NaN,NaN)
          Output = rbind(Output,Tempo)
        } else if (sum(is.na(OR0))==0){
          Tempo = c(f,0.5,OR0,NaN,NaN,NaN,NaN,NaN,NaN)
          Output = rbind(Output,Tempo)
        } else{
          Tempo = c(f,0.5,NaN,NaN,NaN,NaN,NaN,NaN,NaN,NaN,NaN)
          Output = rbind(Output,Tempo)
        }
        
      }
      
    } else {
      for (it in 1:9){
        idx1 = which(Temp>=quantile(Temp,0.1*it))
        idx0 = which(Temp<quantile(Temp,0.1*it))
        
        Final1 = IPTW_Anal(Feature[idx1,], Action[idx1], Label[idx1])
        Final0 = IPTW_Anal(Feature[idx0,], Action[idx0], Label[idx0])
        
        OR1 = Final1$Outcome[2,1:3]
        OR0 = Final0$Outcome[2,1:3]
        
        if (sum(is.na(OR1))+sum(is.na(OR0))==0){
          Inter_Result = Interaction_Test(OR1, OR0)
          Z_Val = Inter_Result[10,1]
          P_Val = Inter_Result[10,2]
          
          P_Val1 = Interaction_Test(OR1, Ori_OR)[10,2]
          P_Val0 = Interaction_Test(OR0, Ori_OR)[10,2]
          
          Tempo = c(f,quantile(Temp,0.1*it),OR0,OR1,P_Val, P_Val0, P_Val1)
          Output = rbind(Output,Tempo)
          
        }  else if (sum(is.na(OR1))==0){
          Tempo = c(f,quantile(Temp,0.1*it),NaN,NaN,NaN,OR1,NaN,NaN,NaN)
          Output = rbind(Output,Tempo)
        } else if (sum(is.na(OR0))==0){
          Tempo = c(f,quantile(Temp,0.1*it),OR0,NaN,NaN,NaN,NaN,NaN,NaN)
          Output = rbind(Output,Tempo)
        } else{
          Tempo = c(f,quantile(Temp,0.1*it),NaN,NaN,NaN,NaN,NaN,NaN,NaN,NaN,NaN)
          Output = rbind(Output,Tempo)
        }
      }
    }
  }
  
  
  Output = Output[-1,]
  
  return (Output)
  
}

