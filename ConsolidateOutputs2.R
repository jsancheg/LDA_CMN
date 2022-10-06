pathPro <- "E:/University of Glasgow/Literature review/R Code/Food Analysis/LDA_CMN/LDA_CMN"
pathOutput <- paste0(pathPro,"/Output/") 

filepath <- pathOutput

process_collection1<- function(filepath)
{
  filenames <- paste0(filepath,dir(filepath))
  ncollection <- length(filenames)
  res1 <- process_file1(filenames[1])
  Accuracy_TM_cont <- res1$Accuracy_TM_cont
  Accuracy_TM_nocont <- res1$Accuracy_TM_nocont
  Accuracy_TM_cont <- res1$Accuracy_TM_cont
  Accuracy_TM_nocont <- res1$Accuracy_TM_nocont
  
  nrows_TM_cont <- nrow(Accuracy_TM_cont)
  nrows_TM_nocont <- nrow(Accuracy_TM_nocont)
  nrows_SM_cont <- nrow(Accuracy_SM_cont)
  nrows_SM_nocont <- nrow(Accuracy_SM_nocont)
  
  
  resumen <- res1$resumen
  
  nrows_resumen <- nrow(res1$resumen)
  
  nfilas <- c(nrows_resumen,nrows_TM_cont,
              nrows_TM_nocont,nrows_SM_cont,nrows_SM_nocont)
  
  if(any(nfilas == 0)) stop("Any kpi's was calculated")
  

  for (i_collection in 2:ncollection)
  {
    res<- process_file1(filenames[i_collection])
    resumen <- rbind.data.frame(resumen,res$resumen)
    Accuracy_TM_cont <- rbind.data.frame(Accuracy_TM_cont,res$Accuracy_TM_cont)
    Accuracy_TM_nocont <- rbind.data.frame(Accuracy_TM_nocont,res$Accuracy_TM_nocont)
    Accuracy_SM_cont <- rbind.data.frame(Accuracy_SM_cont,res$Accuracy_SM_cont)
    Accuracy_SM_nocont <- rbind.data.frame(Accuracy_SM_nocont,res$Accuracy_SM_nocont)
    
  }

  output <- list(resumen = resumen,
                 Accuracy_TM_contaminated = Accuracy_TM_cont,
                 Accuracy_TM_no_contaminated = Accuracy_TM_nocont,
                 Accuracy_SM_contaminated = Accuracy_SM_cont,
                 Accuracy_SM_no_contaminated = Accuracy_SM_nocont)
  
  return(output)  
}

process_file1 <- function(filename)
{
  load(file = filename)
  l <- length(sim.A5)

  resumen <- sim.A5[[1]]$resumen
  colnames(resumen)<-c("Model","AccuracySM","ModelSizeSM",
                      "Inclusion_correctness","Number_var_incorrect_included",
                      "Exclusion_correctness")
  
  Accuracy_TM_cont <- data.frame(Sim = 1,Accuracy_TM_cont5 = sim.A5[[1]]$details[5,1],
                                 Accuracy_TM_cont10 = sim.A5[[1]]$details[10,1],
                                 Accuracy_TM_cont15 = sim.A5[[1]]$details[15,1],
                                 Accuracy_TM_cont20 = sim.A5[[1]]$details[20,1])
  
  Accuracy_TM_nocont <- data.frame(Sim = 1,Accuracy_TM_nocont5 = sim.A5[[1]]$details[5,2],
                                 Accuracy_TM_nocont10 = sim.A5[[1]]$details[10,2],
                                 Accuracy_TM_nocont15 = sim.A5[[1]]$details[15,2],
                                 Accuracy_TM_nocont20 = sim.A5[[1]]$details[20,2])

  Accuracy_SM_cont <- data.frame(Sim = 1,Accuracy_SM_cont5 = sim.A5[[1]]$details[5,3],
                                 Accuracy_SM_cont10 = sim.A5[[1]]$details[10,3],
                                 Accuracy_SM_cont15 = sim.A5[[1]]$details[15,3],
                                 Accuracy_SM_cont20 = sim.A5[[1]]$details[20,3])
  
  Accuracy_SM_nocont <- data.frame(Sim = 1,Accuracy_SM_nocont5 = sim.A5[[1]]$details[5,4],
                                   Accuracy_SM_nocont10 = sim.A5[[1]]$details[10,4],
                                   Accuracy_SM_nocont15 = sim.A5[[1]]$details[15,4],
                                   Accuracy_SM_nocont20 = sim.A5[[1]]$details[20,4])
  
  for(i_run in 2:l)
  {
    cat ("\n run =", i_run, "\n")
    if(length(sim.A5[[i_run]])>1)
    {
      auxdf1<- sim.A5[[i_run]]$resumen
      colnames(auxdf1)<-c("Model","AccuracySM","ModelSizeSM",
                          "Inclusion_correctness","Number_var_incorrect_included",
                          "Exclusion_correctness")

      resumen <- rbind.data.frame(resumen,auxdf1)
      aux_TM_cont <- data.frame(Sim =i_run, 
                                Accuracy_TM_cont5 = sim.A5[[i_run]]$details[5,1],
                                Accuracy_TM_cont10 = sim.A5[[i_run]]$details[10,1],
                                Accuracy_TM_cont15 = sim.A5[[i_run]]$details[15,1],
                                Accuracy_TM_cont20 = sim.A5[[i_run]]$details[20,1])
      aux_TM_nocont <- data.frame(Sim =i_run, 
                                Accuracy_TM_nocont5 = sim.A5[[i_run]]$details[5,2],
                                Accuracy_TM_nocont10 = sim.A5[[i_run]]$details[10,2],
                                Accuracy_TM_nocont15 = sim.A5[[i_run]]$details[15,2],
                                Accuracy_TM_nocont20 = sim.A5[[i_run]]$details[20,2])
      aux_SM_cont <- data.frame(Sim =i_run, 
                                Accuracy_SM_cont5 = sim.A5[[i_run]]$details[5,3],
                                Accuracy_SM_cont10 = sim.A5[[i_run]]$details[10,3],
                                Accuracy_SM_cont15 = sim.A5[[i_run]]$details[15,3],
                                Accuracy_SM_cont20 = sim.A5[[i_run]]$details[20,3])
      aux_SM_nocont <- data.frame(Sim =i_run, 
                                  Accuracy_SM_nocont5 = sim.A5[[i_run]]$details[5,4],
                                  Accuracy_SM_nocont10 = sim.A5[[i_run]]$details[10,4],
                                  Accuracy_SM_nocont15 = sim.A5[[i_run]]$details[15,4],
                                  Accuracy_SM_nocont20 = sim.A5[[i_run]]$details[20,4])
      
      Accuracy_TM_cont <- rbind.data.frame(Accuracy_TM_cont,aux_TM_cont)
      Accuracy_TM_nocont <- rbind.data.frame(Accuracy_TM_nocont,aux_TM_nocont)
      Accuracy_SM_cont <- rbind.data.frame(Accuracy_SM_cont,aux_SM_cont)
      Accuracy_SM_nocont <- rbind.data.frame(Accuracy_SM_nocont,aux_SM_nocont)
      
    }
  }

    output <- list(resumen = resumen,
                   Accuracy_TM_cont = Accuracy_TM_cont, 
                   Accuracy_TM_nocont = Accuracy_TM_nocont,
                   Accuracy_SM_cont = Accuracy_SM_cont, 
                   Accuracy_SM_nocont = Accuracy_SM_nocont)
  return(output)
}


cdir <- c(pathOutput)
res <- process_collection1(cdir[1])
res$resumen
apply(res$resumen[,-1],2,mean)
apply(res$Accuracy_TM_contaminated[,-1],2,mean)
apply(res$Accuracy_TM_no_contaminated[,-1],2,mean)
apply(res$Accuracy_SM_contaminated[,-1],2,mean)
apply(res$Accuracy_SM_no_contaminated[,-1],2,mean)
