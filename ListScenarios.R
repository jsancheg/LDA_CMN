
# Windows path
system_info <- Sys.info()
#OS_name <- system_info("")
pc_name <- system_info['nodename'] 

if(pc_name == "LAPTOP-ADR3M911")
{
  

# Scenarios directory -----------------------------------------------------

  
  pathScenarios <- "E:/University of Glasgow/Thesis/Scenarios/"
  pathScenarios1 <- "E:/University of Glasgow/Thesis/Scenarios1/"

  

# SFiles directory ---------------------------------------------------------

    
  pathSFiles <- "E:/University of Glasgow/Thesis/SFiles/"
  pathSFiles1 <- "E:/University of Glasgow/Thesis/SFiles/"
  
  
  # First complete run, the files in this folder do not have recorded
  # model for covariance matrix that fit best the data.
  # It is believed that all Scenarios were fitted with EII
  pathSFiles_Old <- "E:/University of Glasgow/Thesis/SFiles_Old"

  pathSFiles_HLS_VVV <- "E:/University of Glasgow/Thesis/SFiles_HLS_VVV/"
  
  pathSFiles_HLS_ALL <- "E:/University of Glasgow/Thesis/SFiles_HLS_ALL/"

  # Directory containing the fit of all models for Supervised Learning
  # Greedy search
  pathSFiles_AllModels <- "E:/University of Glasgow/Thesis/SFiles_AllModels/"
  
  pathSFiles_Hard_To_Fit <- "E:/University of Glasgow/Thesis/SFiles_Hard_To_Fit/"
  


# SSFiles directory -------------------------------------------------------

  pathSSFiles <- "E:/University of Glasgow/Thesis/SSFiles/"
  pathSSFiles1 <- "E:/University of Glasgow/Thesis/SSFiles1/"
  
    
  #  First complete run, the files in this folder do not have saved the 
  #  model for covariance matrix that fit best the data.
  #  It is believed that all Scenarios were fitted with EII
  #  A;sp the files were fitted with pnolabeled at 0.5
  pathSSFiles_Old<-"E:/University of Glasgow/Thesis/SSFiles_Old_PNolabeled50/"
  
  pathSFiles_HLS_VVV <- "E:/University of Glasgow/Thesis/SSFiles_HLS_VVV/"
  
    
  pathSSFiles_HLS_ALL <- "E:/University of Glasgow/Thesis/SSFiles_HLS_ALL/"
  

  # Directory containing the fit of all models for Supervised Learning
  # Greedy search
  pathSSFiles_AllModels <- "E:/University of Glasgow/Thesis/SSFiles_AllModels/"
  
  pathSFiles_Hard_To_Fit <- "E:/University of Glasgow/Thesis/SSFiles_Hard_To_Fit/"
  
      
}else if(pc_name == "WildFree")
{
  pathScenarios <- "/home/jsancheg/Documents/Scenarios/"
  pathSSFiles <- "/home/jsancheg/Documents/SSFiles/"
  pathSFiles <- "/home/jsancheg/Documents/SFiles/"
  
} else
{
  pathScenarios <- "M:/Scenarios/"
  pathSSFiles <- "M:/SSFiles/"
  pathSFiles <- "M:/SFiles/"
}


library("stringr")

F1 <- c("VD","MD","VO") # F1 : Mean distance
F2 <- c(2,3)            # F2 : Number of classes
F3 <- c("BAL","INB")    # F3 : class proportion matrix with the number of columns equals to F2
F4 <- c(3000)      # F4 : Number of observations
#F4 <- c(4000)      # F4 : Number of observations
F5 <- c(5,100)          # F5 : Number of variables
F6 <- c(0.75,0.85)      # F6 : Percentage of samples used as training
#F6 <- c(0.5)      # F6 : Percentage of samples used as training
F7 <- c("SCBSV","SCBNSV","SCBSNSV","IND") # F7 : Correlation structure
F8 <- c(0.8,0.9)        # F8 : Percentage of non contaminated samples (alpha)
F9 <- c(5,30)          # F9 : Variance inflation factor
F10 <- c(2,3)           # F10: number of separating variables

# posibilities
# possibilities for 2 classes
#Sets2 <- as.matrix(expand.grid(2,F10,F5,F4,F3,F1,F7,F6,F8,F8,0,F9,F9,0))
#Sets3 <- as.matrix(expand.grid(3,F10,F5,F4,F3,F1,F7,F6,F8,F8,F8,F9,F9,F9))

# New settings after meetin with Nema
Sets2 <- as.matrix(expand.grid(2,F10,F5,F4,F3,F1,F7,F6,0.8,0.9,0,5,30,0))
Sets3 <- as.matrix(expand.grid(3,F10,F5,F4,F3,F1,F7,F6,0.8,0.8,0.9,5,5,30))

n2 <- nrow(Sets2)
n3 <- nrow(Sets3)
Sets <- rbind(Sets2,Sets3)
n <- nrow(Sets)

n5 <- 0
n100 <- 0

Scenarios <- sapply(1:n,function(i) {
  # prefix file: "S_(F2)Sets[i,1]_(F10)S_Sets[i,2]_(F5)S_Sets[i,3]_(F4)S_Sets[i,4]_
  #               (F3)S_Sets[i,8]_(F1)Sets[i,5]_(F7)Sets[i,7]_(F6)Sets[i,6]_(F8.1)Sets[i,9]_
  #               (F8.2)Sets[i,10]_(F8.3)Sets[i,11]_(F9.1)Sets[i,12]_(F9.2)Sets[i,13]_
  #               (F9.3)Sets[i,14]"
  #             "(1)S_(2)NumberofClases_(3)NumberofSeparatingVariables_(4)NumberofVariables_
  #               (5)NumberofObservations_(6)ClassPorportion_(7)MeanDistance_
  #               (8)CorrelationStructure_(9)PercentageTraining_(10)Alpha1_
  #               (11)Alpha2_(12)Alpha3_(13)Eta1_(14)Eta2_(15)Eta3
  # 1) S from Scenario
  # 2) Sets[i,1](F2): Number of classes
  # 3) Sets[i,2](F10): Number of separating variables
  # 4) Sets[i,3](F5): Number of variables
  # 5) Sets[i,4](F4): Number of observations
  # 6) Sets[i,8](F3): Class proportion
  # 7) Sets[i,5](F1): Mean distance
  # 8) Sets[i,7](F7): Correlation structure
  # 9) Sets[i,6](F6): Pecerntage of samples used in training
  # 10) Sets[i,9](F8.1): alpha for the 1st group
  # 11) Sets[i,10](F8.2): alpha for the 2nd group
  # 12) Sets[i,11](F8.3): alpha for the 3rd group
  # 13) Sets[i,12](F9.1): Eta for the 1st group
  # 14) Sets[i,13](F9.2): Eta for the 2nd group
  # 15) Sets[i,14](F9.3): Eta for the 3rd group
  
  
  
  
  if( as.numeric(Sets[i,1]) == 2) # Number of classes
  {
   paste0("S_",as.numeric(Sets[i,1]),"_",as.numeric(Sets[i,2]),"_",
           as.numeric(Sets[i,3]),"_",
           as.numeric(Sets[i,4]),"_",as.numeric(Sets[i,8])*100,"_",
           Sets[i,5],"_",Sets[i,7],"_",
           Sets[i,6],"_A",as.numeric(Sets[i,9])*100, 
           as.numeric(Sets[i,10])*100,"_E",
           as.numeric(Sets[i,12]), as.numeric(Sets[i,13]),"_10.RDS")
    
  }else if (as.numeric(Sets[i,1]) == 3)
  {
    paste0("S_",as.numeric(Sets[i,1]),"_",as.numeric(Sets[i,2]),"_",
           as.numeric(Sets[i,3]),"_",
           as.numeric(Sets[i,4]),"_",as.numeric(Sets[i,8])*100,"_",
           Sets[i,5],"_",Sets[i,7],"_",
           Sets[i,6],"_A",as.numeric(Sets[i,9])*100, 
           as.numeric(Sets[i,10])*100,as.numeric(Sets[i,11])*100,
           "_E",as.numeric(Sets[i,12]), as.numeric(Sets[i,13]),
           as.numeric(Sets[i,14]),"_10.RDS")
  }
  
})

ind2.5vars  <- sapply(Scenarios , function(x)
{
  aux <- str_split_1(x,"_")
  ind2.5 <- (as.numeric(aux[2]) == 2) & (as.numeric(aux[4]) == 5)
  return(ind2.5)
})

ind2.100vars <- sapply(Scenarios , function(x)
{
  aux <- str_split_1(x,"_")
  ind2.100 <- (as.numeric(aux[2]) == 2) & (as.numeric(aux[4]) == 100)
  return(ind2.100)
})

Scenarios2.5 <- Scenarios[ind2.5vars]
Scenarios2.100 <- Scenarios[ind2.100vars]

#Scenarios2.100 <- dir(pathScenarios)
n2.5 <- length(Scenarios2.5)
n2.100 <- length(Scenarios2.100)


ind5vars <- sapply(Scenarios , function(x)
{
  aux <- str_split_1(x,"_")
  ind5 <- as.numeric(aux[4]) == 5
  return(ind5)
})

ind100vars <- !ind5vars
 n5<-  sum(ind5vars)
 n100 <- sum(ind100vars)
  

 Scenarios5 <- character(n5)
 Scenarios100 <-character(n100)
 
j5<- 1
j100<-1 
for (i in 1:n)
{
  
    if (as.numeric(Sets[i,3]) == 5)
    {
        if( as.numeric(Sets[i,1]) == 2) # Number of classes
        {
                                Scenarios5[j5] <- paste0("S_",as.numeric(Sets[i,1]),"_",as.numeric(Sets[i,2]),"_",
                                as.numeric(Sets[i,3]),"_",
                                as.numeric(Sets[i,4]),"_",as.numeric(Sets[i,8])*100,"_",
                                Sets[i,5],"_",Sets[i,7],"_",
                                Sets[i,6],"_A",as.numeric(Sets[i,9])*100, 
                                as.numeric(Sets[i,10])*100,"_E",
                                as.numeric(Sets[i,12]), as.numeric(Sets[i,13]),"_10.RDS")
        
          }else if (as.numeric(Sets[i,1]) == 3)
          {
                                Scenarios5[j5] <- paste0("S_",as.numeric(Sets[i,1]),"_",as.numeric(Sets[i,2]),"_",
                                as.numeric(Sets[i,3]),"_",
                                as.numeric(Sets[i,4]),"_",as.numeric(Sets[i,8])*100,"_",
                                Sets[i,5],"_",Sets[i,7],"_",
                                Sets[i,6],"_A",as.numeric(Sets[i,9])*100, 
                                as.numeric(Sets[i,10])*100,as.numeric(Sets[i,11])*100,
                                "_E",as.numeric(Sets[i,12]), as.numeric(Sets[i,13]),
                                as.numeric(Sets[i,14]),"_10.RDS")
          }
      
      
          j5 <- j5 + 1                
                          
      } else if(as.numeric(Sets[i,3]) == 100)
          {
            if( as.numeric(Sets[i,1]) == 2) # Number of classes
            {
                        Scenarios100[j100] <- paste0("S_",as.numeric(Sets[i,1]),"_",as.numeric(Sets[i,2]),"_",
                                  as.numeric(Sets[i,3]),"_",
                                  as.numeric(Sets[i,4]),"_",as.numeric(Sets[i,8])*100,"_",
                                  Sets[i,5],"_",Sets[i,7],"_",
                                  Sets[i,6],"_A",as.numeric(Sets[i,9])*100, 
                                  as.numeric(Sets[i,10])*100,"_E",
                                  as.numeric(Sets[i,12]), as.numeric(Sets[i,13]),"_10.RDS")
          
              }else if (as.numeric(Sets[i,1]) == 3)
              {
                        Scenarios100[j100] <- paste0("S_",as.numeric(Sets[i,1]),"_",as.numeric(Sets[i,2]),"_",
                                  as.numeric(Sets[i,3]),"_",
                                  as.numeric(Sets[i,4]),"_",as.numeric(Sets[i,8])*100,"_",
                                  Sets[i,5],"_",Sets[i,7],"_",
                                  Sets[i,6],"_A",as.numeric(Sets[i,9])*100, 
                                  as.numeric(Sets[i,10])*100,as.numeric(Sets[i,11])*100,
                                  "_E",as.numeric(Sets[i,12]), as.numeric(Sets[i,13]),
                                  as.numeric(Sets[i,14]),"_10.RDS")
              }
          
              j100<-j100+1
          }  
}
 
 
 
n5.1 <- floor(n5*10/100)
n5.2 <- floor(n5*20/100)
n5.3 <- floor(n5*30/100)
n5.4 <- floor(n5*40/100)
n5.5 <- floor(n5*50/100)
n5.6 <- floor(n5*60/100)
n5.7 <- floor(n5*70/100)
n5.8 <- floor(n5*80/100)
n5.9 <- floor(n5*90/100)

n100.1 <- floor(n100*10/100)
n100.2 <- floor(n100*20/100)
n100.3 <- floor(n100*30/100)
n100.4 <- floor(n100*40/100)
n100.5 <- floor(n100*50/100)
n100.6 <- floor(n100*60/100)
n100.7 <- floor(n100*70/100)
n100.8 <- floor(n100*80/100)
n100.9 <- floor(n100*90/100)


n2.5.1 <- floor(n2.5*10/100)
n2.5.2 <- floor(n2.5*20/100)
n2.5.3 <- floor(n2.5*30/100)
n2.5.4 <- floor(n2.5*40/100)
n2.5.5 <- floor(n2.5*50/100)
n2.5.6 <- floor(n2.5*60/100)
n2.5.7 <- floor(n2.5*70/100)
n2.5.8 <- floor(n2.5*80/100)
n2.5.9 <- floor(n2.5*90/100)


n2.100.1 <- floor(n2.100*10/100)
n2.100.2 <- floor(n2.100*20/100)
n2.100.3 <- floor(n2.100*30/100)
n2.100.4 <- floor(n2.100*40/100)
n2.100.5 <- floor(n2.100*50/100)
n2.100.6 <- floor(n2.100*60/100)
n2.100.7 <- floor(n2.100*70/100)
n2.100.8 <- floor(n2.100*80/100)
n2.100.9 <- floor(n2.100*90/100)


n2.100.p1 <- floor(n2.100*1/100)
n2.100.p2 <- floor(n2.100*2/100)
n2.100.p3 <- floor(n2.100*3/100)
n2.100.p4 <- floor(n2.100*4/100)
n2.100.p5 <- floor(n2.100*5/100)
n2.100.p6 <- floor(n2.100*6/100)
n2.100.p7 <- floor(n2.100*7/100)
n2.100.p8 <- floor(n2.100*8/100)
n2.100.p9 <- floor(n2.100*9/100)

n2.100.p10 <- floor(n2.100*10/100)
n2.100.p11 <- floor(n2.100*11/100)
n2.100.p12 <- floor(n2.100*12/100)
n2.100.p13 <- floor(n2.100*13/100)
n2.100.p14 <- floor(n2.100*14/100)
n2.100.p15 <- floor(n2.100*15/100)
n2.100.p16 <- floor(n2.100*16/100)
n2.100.p17 <- floor(n2.100*17/100)
n2.100.p18 <- floor(n2.100*18/100)
n2.100.p19 <- floor(n2.100*19/100)

n2.100.p20 <- floor(n2.100*20/100)
n2.100.p21 <- floor(n2.100*21/100)
n2.100.p22 <- floor(n2.100*22/100)
n2.100.p23 <- floor(n2.100*23/100)
n2.100.p24 <- floor(n2.100*24/100)
n2.100.p25 <- floor(n2.100*25/100)
n2.100.p26 <- floor(n2.100*26/100)
n2.100.p27 <- floor(n2.100*27/100)
n2.100.p28 <- floor(n2.100*28/100)
n2.100.p29 <- floor(n2.100*29/100)

n2.100.p30 <- floor(n2.100*30/100)
n2.100.p31 <- floor(n2.100*31/100)
n2.100.p32 <- floor(n2.100*32/100)
n2.100.p33 <- floor(n2.100*33/100)
n2.100.p34 <- floor(n2.100*34/100)
n2.100.p35 <- floor(n2.100*35/100)
n2.100.p36 <- floor(n2.100*36/100)
n2.100.p37 <- floor(n2.100*37/100)
n2.100.p38 <- floor(n2.100*38/100)
n2.100.p39 <- floor(n2.100*39/100)


n2.100.p40 <- floor(n2.100*40/100)
n2.100.p41 <- floor(n2.100*41/100)
n2.100.p42 <- floor(n2.100*42/100)
n2.100.p43 <- floor(n2.100*43/100)
n2.100.p44 <- floor(n2.100*44/100)
n2.100.p45 <- floor(n2.100*45/100)
n2.100.p46 <- floor(n2.100*46/100)
n2.100.p47 <- floor(n2.100*47/100)
n2.100.p48 <- floor(n2.100*48/100)
n2.100.p49 <- floor(n2.100*49/100)


n2.100.p50 <- floor(n2.100*50/100)
n2.100.p51 <- floor(n2.100*51/100)
n2.100.p52 <- floor(n2.100*52/100)
n2.100.p53 <- floor(n2.100*53/100)
n2.100.p54 <- floor(n2.100*54/100)
n2.100.p55 <- floor(n2.100*55/100)
n2.100.p56 <- floor(n2.100*56/100)
n2.100.p57 <- floor(n2.100*57/100)
n2.100.p58 <- floor(n2.100*58/100)
n2.100.p59 <- floor(n2.100*59/100)

n2.100.p60 <- floor(n2.100*60/100)
n2.100.p61 <- floor(n2.100*61/100)
n2.100.p62 <- floor(n2.100*62/100)
n2.100.p63 <- floor(n2.100*63/100)
n2.100.p64 <- floor(n2.100*64/100)
n2.100.p65 <- floor(n2.100*65/100)
n2.100.p66 <- floor(n2.100*66/100)
n2.100.p67 <- floor(n2.100*67/100)
n2.100.p68 <- floor(n2.100*68/100)
n2.100.p69 <- floor(n2.100*69/100)

n2.100.p70 <- floor(n2.100*70/100)
n2.100.p71 <- floor(n2.100*71/100)
n2.100.p72 <- floor(n2.100*72/100)
n2.100.p73 <- floor(n2.100*73/100)
n2.100.p74 <- floor(n2.100*74/100)
n2.100.p75 <- floor(n2.100*75/100)
n2.100.p76 <- floor(n2.100*76/100)
n2.100.p77 <- floor(n2.100*77/100)
n2.100.p78 <- floor(n2.100*78/100)
n2.100.p79 <- floor(n2.100*79/100)

n2.100.p80 <- floor(n2.100*80/100)
n2.100.p81 <- floor(n2.100*81/100)
n2.100.p82 <- floor(n2.100*82/100)
n2.100.p83 <- floor(n2.100*83/100)
n2.100.p84 <- floor(n2.100*84/100)
n2.100.p85 <- floor(n2.100*85/100)
n2.100.p86 <- floor(n2.100*86/100)
n2.100.p87 <- floor(n2.100*87/100)
n2.100.p88 <- floor(n2.100*88/100)
n2.100.p89 <- floor(n2.100*89/100)

n2.100.p90 <- floor(n2.100*90/100)
n2.100.p91 <- floor(n2.100*91/100)
n2.100.p92 <- floor(n2.100*92/100)
n2.100.p93 <- floor(n2.100*93/100)
n2.100.p94 <- floor(n2.100*94/100)
n2.100.p95 <- floor(n2.100*95/100)
n2.100.p96 <- floor(n2.100*96/100)
n2.100.p97 <- floor(n2.100*97/100)
n2.100.p98 <- floor(n2.100*98/100)
n2.100.p99 <- floor(n2.100*99/100)

