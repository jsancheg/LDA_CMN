# Create dataset


#work_path <- "E:/University of Glasgow/Literature review/R Code/"
#setwd(work_path)
source("utilities.R")

SettingGen<-function(nclasses,nsvar,nvar,
                     ClassesProportion,covStructure,distance)
{
  # nclasses          : number of classes
  # nsvar             : number of separating variables
  # nvar              : total number of variables
  # ClassesProportion : Balance 50-50 or unbalance 90-10
  # covStructure      : covariance structure
  #                       # IND = Independence
  #                       # SCBSV = Strong correlation between separating variables
  #
  # distance          : distance between classes mean
  
  
  paste("S",nclasses,nsvar,nvar,covStructure,distance)
  

  
}

