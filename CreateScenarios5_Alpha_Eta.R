#  Create additional simulations for alpha and eta where
# for 2 classes alpha1 = 0.9,   alpha2 = 0.8,   eta1 = 30,  eta2 = 5
# for 3 classes alpha1 = 0.9    alpha2 = 0.9    alpha3 = 0.8    eta1 = 30   eta2 = 5    eta3 = 5

source("CMNFunctionsV2.R")
source("SimulateScenario.R")
source("ListScenariosAlpha_Eta.R")
library(ssh)
nruns <- 10

Scenarios5<-character(n5)

n <- nrow(Sets)
aux <- vector("list",n5)

j <- 1

for(i in 1:n)
{
  if(as.numeric(Sets[i,3]) ==5 )  
    {
      aux[[j]] <- as.list(Sets[i,])
      
      if( as.numeric(Sets[i,1]) == 2) # Number of classes
      {
        Scenarios5[j] <- paste0("S_",as.numeric(Sets[i,1]),"_",as.numeric(Sets[i,2]),"_",
                                  as.numeric(Sets[i,3]),"_",
                                  as.numeric(Sets[i,4]),"_",as.numeric(Sets[i,8])*100,"_",
                                  Sets[i,5],"_",Sets[i,7],"_",
                                  Sets[i,6],"_A",as.numeric(Sets[i,9])*100, 
                                  as.numeric(Sets[i,10])*100,"_E",
                                  as.numeric(Sets[i,12]), as.numeric(Sets[i,13]),"_10.RDS")
        
      }else if (as.numeric(Sets[i,1]) == 3)
      {
        Scenarios5[j] <- paste0("S_",as.numeric(Sets[i,1]),"_",as.numeric(Sets[i,2]),"_",
                                  as.numeric(Sets[i,3]),"_",
                                  as.numeric(Sets[i,4]),"_",as.numeric(Sets[i,8])*100,"_",
                                  Sets[i,5],"_",Sets[i,7],"_",
                                  Sets[i,6],"_A",as.numeric(Sets[i,9])*100, 
                                  as.numeric(Sets[i,10])*100,as.numeric(Sets[i,11])*100,
                                  "_E",as.numeric(Sets[i,12]), as.numeric(Sets[i,13]),
                                  as.numeric(Sets[i,14]),"_10.RDS")
      }
      
      j <- j + 1
      
      
    }  
}



#Scenarios5var <- sapply(1:n,function(i) {
#  if( as.numeric(Sets[i,1]) == 2) # Number of classes
#  {
#    paste0("S_",as.numeric(Sets[i,1]),"_",as.numeric(Sets[i,2]),"_",
#           as.numeric(Sets[i,3]),"_",
#           as.numeric(Sets[i,4]),"_",as.numeric(Sets[i,8])*100,"_",
#           Sets[i,5],"_",Sets[i,7],"_",
#           Sets[i,6],"_A",as.numeric(Sets[i,9])*100, 
#           as.numeric(Sets[i,10])*100,"_E",
#           as.numeric(Sets[i,12]), as.numeric(Sets[i,13]),"_10.RDS")
    
#  }else if (as.numeric(Sets[i,1]) == 3)
#  {
#    paste0("S_",as.numeric(Sets[i,1]),"_",as.numeric(Sets[i,2]),"_",
#           as.numeric(Sets[i,3]),"_",
#           as.numeric(Sets[i,4]),"_",as.numeric(Sets[i,8])*100,"_",
#           Sets[i,5],"_",Sets[i,7],"_",
#           Sets[i,6],"_A",as.numeric(Sets[i,9])*100, 
#           as.numeric(Sets[i,10])*100,as.numeric(Sets[i,11])*100,
#           "_E",as.numeric(Sets[i,12]), as.numeric(Sets[i,13]),
#           as.numeric(Sets[i,14]),"_10.RDS")
#  }
#})



#my_ssh_session <- ssh_connect("2201449s@130.209.66.80:22")

ini <- 1 
fin <- n5

length(aux)
length(Scenarios5)

# Windows path
system_info <- Sys.info()
#OS_name <- system_info("")
pc_name <- system_info['nodename'] 

if(pc_name == "LAPTOP-ADR3M911")
{
  pathScenarios <- "E:/University of Glasgow/Thesis/Scenarios/"
  pathSSFiles <- "E:/University of Glasgow/Thesis/SSFiles/"
  pathSFiles <- "E:/University of Glasgow/Thesis/SFiles/"
  
}else if(pc_name == "WildFree")
{
  pathScenarios <- "/home/jsancheg/Documents/Scenarios/"
  pathSSFiles <- "/home/jsancheg/Documents/SSFiles/"
  pathSFiles <- "/home/jsancheg/Documents/SFiles/"
  
}

#pathScenarios <- "E:/University of Glasgow/Thesis/ScenariosNew/"

tic("Create Scenarios for 5 variables")
SimStatus <- mclapply((ini:fin),function(i)
{
#  command2 <- "ls /home/pgrad1/2201449s/R/CMN/Scenarios3"
#  FilesProcessed <- capture.output(ssh_exec_wait(my_ssh_session,command2))
  FilesProcessed <- dir(pathScenarios)
  file_name <- Scenarios5[i]
  
#  if(is_empty(intersect(FilesProcessed,file_name)))
 # {
    SimProgress <- SimScenario(aux[[i]],nruns,pathScenarios)
    #    scp_upload(my_ssh_session,paste0(pathScenarios,file_name),"/home/pgrad1/2201449s/R/CMN/SFiles/")
    filePathScenario <- paste0(pathScenarios,file_name)
  #  scp_upload(my_ssh_session,filePathScenario,"/home/pgrad1/2201449s/R/CMN/Scenarios3/")
    cat("The scenario has been created")
    
    #    if (file.exists(filePathScenario)) 
    #   {
    # Remove the file
    #      file.remove(filePathScenario)
    #     cat("File deleted successfully.\n")
    #    } else {
    #      cat("File does not exist.\n")
    #   }
    
 # }
  
}, mc.cores = 1)

toc()
