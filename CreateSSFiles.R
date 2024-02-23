# 
source("Semisupervised.R")
source("ListScenarios.R")
source("GSSFile.R")
library(purrr)
library(ContaminatedMixt)

if (!requireNamespace("googledrive", quietly = TRUE)) {
  install.packages("googledrive")
}
if (!requireNamespace("readr", quietly = TRUE)) {
  install.packages("readr")
}
library(googledrive)
library(gargle)
library(readr)

# Ubunto paths
# pathScenarios <- "/home/jsancheg/Documents/Scenarios/"
# pathFiles <- "/home/jsancheg/Documents/SSFiles/"



ini <- n100.4+1
fin <- n100.5

fin-ini+1
Model <- "VVI"

ScenariosToRun <- c("S_3_3_5_3000_85_INB_SCBSV_VO_A808090_E5530_10.RDS",
                    "S_3_3_5_3000_85_INB_SCBSV_VD_A808090_E5530_10.RDS",
                    "S_3_3_5_3000_85_INB_SCBSV_MD_A808090_E5530_10.RDS",
                    "S_3_3_5_3000_85_INB_SCBSNSV_VO_A808090_E5530_10.RDS",
                    "S_3_3_5_3000_85_INB_SCBNSV_VO_A808090_E5530_10.RDS",
                    "S_3_3_5_3000_85_INB_IND_VO_A808090_E5530_10.RDS",
                    "S_3_3_5_3000_75_INB_SCBSV_VO_A808090_E5530_10.RDS",
                    "S_3_3_5_3000_75_INB_SCBSV_VD_A808090_E5530_10.RDS",
                    "S_3_3_5_3000_75_INB_SCBSV_MD_A808090_E5530_10.RDS",
                    "S_3_3_5_3000_75_INB_SCBSNSV_VO_A808090_E5530_10.RDS",
                    "S_3_3_5_3000_75_INB_SCBNSV_VO_A808090_E5530_10.RDS",
                    "S_3_3_5_3000_75_INB_IND_VO_A808090_E5530_10.RDS",
                    "S_3_2_5_3000_85_INB_SCBSV_VO_A808090_E5530_10.RDS",
                    "S_3_2_5_3000_85_INB_SCBSNSV_VO_A808090_E5530_10.RDS",
                    "S_3_2_5_3000_85_INB_SCBNSV_VO_A808090_E5530_10.RDS",
                    "S_3_2_5_3000_85_INB_IND_VO_A808090_E5530_10.RDS",
                    "S_3_2_5_3000_75_INB_SCBSV_VO_A808090_E5530_10.RDS",
                    "S_3_2_5_3000_75_INB_SCBSNSV_VO_A808090_E5530_10.RDS",
                    "S_3_2_5_3000_75_INB_SCBNSV_VO_A808090_E5530_10.RDS",
                    "S_3_2_5_3000_75_INB_IND_VO_A808090_E5530_10.RDS",
                    "S_2_3_5_3000_85_INB_SCBSV_VO_A8090_E530_10.RDS",
                    "S_2_3_5_3000_85_INB_SCBSV_VD_A8090_E530_10.RDS",
                    "S_2_3_5_3000_85_INB_SCBSV_MD_A8090_E530_10.RDS",
                    "S_2_3_5_3000_85_INB_SCBSNSV_VO_A8090_E530_10.RDS",
                    "S_2_3_5_3000_85_INB_SCBSNSV_VD_A8090_E530_10.RDS",
                    "S_2_3_5_3000_85_INB_SCBNSV_VO_A8090_E530_10.RDS",
                    "S_2_3_5_3000_85_INB_SCBNSV_VD_A8090_E530_10.RDS",
                    "S_2_3_5_3000_85_INB_IND_VO_A8090_E530_10.RDS",
                    "S_2_3_5_3000_85_INB_IND_MD_A8090_E530_10.RDS",
                    "S_2_3_5_3000_85_BAL_SCBSV_VD_A8090_E530_10.RDS",
                    "S_2_3_5_3000_85_BAL_SCBSV_MD_A8090_E530_10.RDS",
                    "S_2_3_5_3000_85_BAL_SCBSNSV_VD_A8090_E530_10.RDS",
                    "S_2_3_5_3000_75_INB_SCBSV_VO_A8090_E530_10.RDS",
                    "S_2_3_5_3000_75_INB_SCBSV_VD_A8090_E530_10.RDS",
                    "S_2_3_5_3000_75_INB_SCBSNSV_VO_A8090_E530_10.RDS",
                    "S_2_3_5_3000_75_INB_SCBSNSV_VD_A8090_E530_10.RDS",
                    "S_2_3_5_3000_75_INB_SCBNSV_VO_A8090_E530_10.RDS",
                    "S_2_3_5_3000_75_INB_SCBNSV_VD_A8090_E530_10.RDS",
                    "S_2_3_5_3000_75_INB_IND_VO_A8090_E530_10.RDS",
                    "S_2_3_5_3000_75_BAL_SCBSV_VD_A8090_E530_10.RDS",
                    "S_2_3_5_3000_75_BAL_SCBSV_MD_A8090_E530_10.RDS",
                    "S_2_2_5_3000_85_INB_SCBSV_VO_A8090_E530_10.RDS",
                    "S_2_2_5_3000_85_INB_SCBSNSV_VO_A8090_E530_10.RDS",
                    "S_2_2_5_3000_85_INB_SCBNSV_VO_A8090_E530_10.RDS",
                    "S_2_2_5_3000_75_INB_SCBSV_VO_A8090_E530_10.RDS",
                    "S_2_2_5_3000_75_INB_SCBSV_MD_A8090_E530_10.RDS",
                    "S_2_2_5_3000_75_INB_SCBSNSV_VO_A8090_E530_10.RDS",
                    "S_2_2_5_3000_75_INB_SCBNSV_VO_A8090_E530_10.RDS",
                    "S_2_2_5_3000_75_INB_IND_VO_A8090_E530_10.RDS")


status <- mclapply(ScenariosToRun, function(x){
  
  SSFilename <- str_replace(x,"S_","SSV_")
  FilesProcessed <- dir(pathSSFiles)
#  if(is_empty(intersect(FilesProcessed,SSFilename))) 
#  {
  tryCatch(
    {
      
    GenerateSSFile(x,pathScenarios,pathSSFiles,Model) 
      return(1)
    }, error = function(e){
      cat("Error fitting scenario: ",x, "\n")
      return(NULL)
    }
    )
  
 # }  else cat("\n The file ",SSFilename, " already exists in the directory. \n")
  
}, mc.cores = 1)

