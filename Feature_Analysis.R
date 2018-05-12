# Jinsung Yoon (05/12/2018)
# Source code for Table 1 in PloS Medicine Submitted paper
rm(list = ls())

# Packages 
library(mice)

# Data Inclusion
data <- read.csv("/home/jinsung/Documents/Jinsung/2018_Research/Raffaele/Data/Manuscript_Data_0327.csv", sep = ",", header = T)

# Table 1
Table1 = matrix(0, nrow = 22, ncol = 5)

# Divide RMT / PCI patients
rmt_idx = (data$ppci == 0)
pci_idx = (data$ppci == 1)

# For each feature
for (i in 1:22){
  if (i == 1){Feature = data$age}
  else if (i == 2){Feature = data$female}
  else if (i == 3){Feature = data$hypercol}
  else if (i == 4){Feature = data$htn}
  else if (i == 5){Feature = data$smoking}
  else if (i == 6){Feature = data$family_cad}
  else if (i == 7){Feature = data$prior_angina}
  else if (i == 8){Feature = data$prior_mi}
  else if (i == 9){Feature = data$prior_cabg}
  else if (i == 10){Feature = data$prior_pci}
  else if (i == 11){Feature = data$pad}
  else if (i == 12){Feature = data$hf}
  else if (i == 13){Feature = data$prior_stroke}
  else if (i == 14){Feature = data$positive_biomarker}
  else if (i == 15){Feature = data$hr}
  else if (i == 16){Feature = data$sbp}
  else if (i == 17){Feature = data$ckd}
  else if (i == 18){Feature = data$asprin}
  else if (i == 19){Feature = data$clop}
  else if (i == 20){Feature = data$ufh}
  else if (i == 21){Feature = data$bb}
  else if (i == 22){Feature = data$acei}
  
  # For Continuous value
  if (max(Feature, na.rm = TRUE)>1){
    Table1[i,1] = round(mean(Feature[rmt_idx], na.rm = TRUE),1)
    Table1[i,2] = round(sd(Feature[rmt_idx], na.rm = TRUE),1)
    
    Table1[i,3] = round(mean(Feature[pci_idx], na.rm = TRUE),1)
    Table1[i,4] = round(sd(Feature[pci_idx], na.rm = TRUE),1)
    
  # For Binary value
  } else if (max(Feature, na.rm = TRUE) == 1){
    Table1[i,1] = sum(Feature[rmt_idx]==1, na.rm=TRUE)
    Table1[i,2] = round(100*sum(Feature[rmt_idx]==1, na.rm=TRUE)/sum(is.na(Feature[rmt_idx])==0),1)
    
    Table1[i,3] = sum(Feature[pci_idx]==1, na.rm=TRUE)
    Table1[i,4] = round(100*sum(Feature[pci_idx]==1, na.rm=TRUE)/sum(is.na(Feature[pci_idx])==0),1)
    
  }
  
  # P-value
  Table1[i,5] = t.test(Feature[rmt_idx], Feature[pci_idx], var.equal=FALSE, paired=FALSE)$p.value
  
}

# Output
write.csv(Table1,file="/home/jinsung/Documents/Jinsung/2018_Research/Raffaele/Rcode/Result/Table1.csv")
