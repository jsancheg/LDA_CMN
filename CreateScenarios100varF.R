source("CMNFunctionsV2.R")
source("SimulateScenario.R")
source("ListScenarios.R")
library(ssh)
nruns <- 10

Scenarios100<-character(n100)

n <- nrow(Sets)
aux <- vector("list",n100)

j <- 1

for(i in 1:n)
{
  if(as.numeric(Sets[i,3]) ==100 )  
    {
      aux[[j]] <- as.list(Sets[i,])
      
      
      
      if( as.numeric(Sets[i,1]) == 2) # Number of classes
      {
        Scenarios100[j] <- paste0("S_",as.numeric(Sets[i,1]),"_",as.numeric(Sets[i,2]),"_",
               as.numeric(Sets[i,3]),"_",
               as.numeric(Sets[i,4]),"_",as.numeric(Sets[i,8])*100,"_",
               Sets[i,5],"_",Sets[i,7],"_",
               Sets[i,6],"_A",as.numeric(Sets[i,9])*100, 
               as.numeric(Sets[i,10])*100,"_E",
               as.numeric(Sets[i,12]), as.numeric(Sets[i,13]),"_10.RDS")
        
      }else if (as.numeric(Sets[i,1]) == 3)
      {
        Scenarios100[j] <- paste0("S_",as.numeric(Sets[i,1]),"_",as.numeric(Sets[i,2]),"_",
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





#my_ssh_session <- ssh_connect("2201449s@130.209.66.80:22")

ini <- 1 
fin <- n100.5

fin-ini + 1

length(aux)
length(Scenarios100)

#pathScenarios <- "E:/University of Glasgow/Thesis/ScenariosNew/"

SimStatus <- mclapply((ini:fin),function(i)
{
#  command2 <- "ls /home/pgrad1/2201449s/R/CMN/Scenarios3"
#  FilesProcessed <- capture.output(ssh_exec_wait(my_ssh_session,command2))
  FilesProcessed <- dir(pathScenarios)
  file_name <- Scenarios100[i]
  
#  if(is_empty(intersect(FilesProcessed,file_name)))
#  {
    SimProgress <- SimScenario(aux[[i]],nruns,pathScenarios1)
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

