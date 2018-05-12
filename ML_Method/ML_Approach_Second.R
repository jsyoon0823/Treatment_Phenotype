ML_Approach_Second <- function(Feature, Action, Label, Comp_OR){
  
  ###### Function call
  source("/home/jinsung/Documents/Jinsung/2018_Research/Raffaele/Rcode/IPTW/IPTW_Anal.R")
  source("/home/jinsung/Documents/Jinsung/2018_Research/Raffaele/Rcode/Interaction_Test/Interaction_Test.R")
  
  # 1. Parameter update
  No = nrow(Feature)
  Feature_No = ncol(Feature)
  
  ############### Division
  Output = matrix(0,1,6)
  
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
          
          if ((P_Val < 0.05) & ( log(OR1[1])*log(OR0[1]) < 0 ) ){
            
            Inter_Result1 = Interaction_Test(OR1, Comp_OR)
            Inter_Result0 = Interaction_Test(OR0, Comp_OR)
            
            if ( (Inter_Result1[10,2] < 0.05) & (Inter_Result0[10,2] < 0.05) ){
              Tempo = c(f,0.5,Z_Val,P_Val,OR1[1],OR0[1])
              Output = rbind(Output,Tempo)
            }
            
          }
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
          
          if ((P_Val < 0.05) & ( log(OR1[1])*log(OR0[1]) < 0 ) ){
            
            Inter_Result1 = Interaction_Test(OR1, Comp_OR)
            Inter_Result0 = Interaction_Test(OR0, Comp_OR)
            
            if ( (Inter_Result1[10,2] < 0.05) & (Inter_Result0[10,2] < 0.05) ){
              
              Tempo = c(f,quantile(Temp,0.1*it),Z_Val,P_Val,OR1[1],OR0[1])
              Output = rbind(Output,Tempo)
              
            }
          }
        }
      }
    }
  }
  
  if (nrow(Output)>1){
    Output = Output[-1,]
    Output = as.matrix(Output)
    new_idx = which.min(Output[,4])
    Output = Output[new_idx,]
  }
  
  ########## Output Computation
  if (Output[1]>0){
    f = Output[1]
    thresh = Output[2]
    
    Temp = Feature[,f]
    idx1 = which(Temp>=thresh)
    idx0 = which(Temp<thresh)
    
    Final1 = IPTW_Anal(Feature[idx1,], Action[idx1], Label[idx1])
    Final0 = IPTW_Anal(Feature[idx0,], Action[idx0], Label[idx0])
    
    OR1 = Final1$Outcome[2,1:3]
    OR0 = Final0$Outcome[2,1:3]
    
    Inter_Result = Interaction_Test(OR1, OR0)
    
    Final_Output = list()
    Final_Output$OR1 = OR1
    Final_Output$OR0 = OR0
    Final_Output$Output = as.numeric(Output)
    Final_Output$Final1 = Final1
    Final_Output$Final0 = Final0
    Final_Output$Inter_Result = Inter_Result
    
  } else {
    Final_Output = list()
    Final_Output$Output = Output
  }
  
  return (Final_Output)
  
}

