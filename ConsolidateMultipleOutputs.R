pathPro <- "E:/University of Glasgow/Literature review/R Code/Food Analysis/LDA_CMN/LDA_CMN"
pathEM05 <- paste0(pathPro,"/Output5/")
pathEM10 <-paste0(pathPro,"/Output10/")
pathEM15 <- paste0(pathPro,"/Output15/")

#ldir <- list()
cdir <- c(pathEM05,pathEM10,pathEM15)

process_collection<- function(filepath)
{
  filenames <- paste0(filepath,dir(filepath))
  ncollection <- length(filenames)
  res1 <- process_file(filenames[1])
  nvar <- length(res1)
  if(nvar == 0) stop("Any kpi's was calculated")
  d <- length(res1[[1]])
  
  for (i_collection in 2:ncollection)
  {
    res <- process_file(filenames[i_collection])
    res1 <- rbind(res1,res)
  }
  res1  
  
  return(res1)  
}

process_file <- function(filename)
{
  load(file = filename)
  l <- length(sim.A5)
  Accuracy <- rep(0,l)
  ModelSize <- rep(0,l)
  InclusionCorrectness <- rep(0,l)
  Number_var_incorrect_included <-rep(0,l)
  ExclusionCorrectness <- rep(0,l)
  Accuracy_TrueModel_contaminated <- rep(0,l)
  Accuracy_TrueModel_no_contaminated <- rep(0,l)
  Accuracy_contaminated <- rep(0,l)
  Accuracy_no_contaminated <- rep(0,l)

  for(i_run in 1:l)
  {
    cat ("\n run =", i_run, "\n")
    if(length(sim.A5[[i_run]])>1)
    {
      Accuracy[i_run] <- unlist(sim.A5[[i_run]]$Accuracy)
      ModelSize[i_run] <- unlist(sim.A5[[i_run]]$ModelSize)
      cat("\n Model Size = ", unlist(sim.A5[[i_run]]$ModelSize) )
      InclusionCorrectness[i_run] <- unlist(sim.A5[[i_run]]$Inclusion_correctness)
      Number_var_incorrect_included <-unlist(sim.A5[[i_run]]$Number_var_incorrect_included)
      ExclusionCorrectness[i_run] <- unlist(sim.A5[[i_run]]$Exclusion_correctness)
      Accuracy_TrueModel_contaminated <- unlist(sim.A5[[i_run]]$Accuracy_TrueModel_contaminated)
      Accuracy_TrueModel_no_contaminated <- unlist(sim.A5[[i_run]]$Accuracy_TrueModel_no_contaminated)
      Accuracy_contaminated[i_run] <- unlist(sim.A5[[i_run]]$Accuracy_contaminated)
      Accuracy_no_contaminated[i_run] <- unlist(sim.A5[[i_run]]$Accuracy_no_contaminated)
    }
  }
  output <- data.frame(Accuracy,
                       ModelSize,
                       InclusionCorrectness,
                       Number_var_incorrect_included,
                       ExclusionCorrectness,
                       Accuracy_TrueModel_contaminated,
                       Accuracy_TrueModel_no_contaminated,
                       Accuracy_contaminated,
                       Accuracy_no_contaminated)
  return(output)
}



process_collection1 <- function(filepath)
{
  filenames <- paste0(filepath,dir(filepath))
  ncollection <- length(filenames)
  res1 <- process_file(filenames[1])
  nvar <- length(res1)
  if(nvar == 0) stop("Any kpi's was calculated")
  d <- length(res1[[1]])
  
  for (i_collection in 2:ncollection)
  {
    res <- process_file(filenames[i_collection])
    res1 <- rbind(res1,res)
  }
  res1  
  
  return(res1)  
}

process_file1 <- function(filename)
{
  load(file = filename)
  l <- length(sim.A5)
  Accuracy <- rep(0,l)
  ModelSize <- rep(0,l)
  InclusionCorrectness <- rep(0,l)
  Number_var_incorrect_included <-rep(0,l)
  ExclusionCorrectness <- rep(0,l)
  Accuracy_TrueModel_contaminated <- rep(0,l)
  Accuracy_TrueModel_no_contaminated <- rep(0,l)
  Accuracy_contaminated <- rep(0,l)
  Accuracy_no_contaminated <- rep(0,l)
  
  for(i_run in 1:l)
  {
    cat ("\n run =", i_run, "\n")
    if(length(sim.A5[[i_run]])>1)
    {
      Accuracy[i_run] <- unlist(sim.A5[[i_run]]$Accuracy)
      ModelSize[i_run] <- unlist(sim.A5[[i_run]]$ModelSize)
      cat("\n Model Size = ", unlist(sim.A5[[i_run]]$ModelSize) )
      InclusionCorrectness[i_run] <- unlist(sim.A5[[i_run]]$Inclusion_correctness)
      Number_var_incorrect_included <-unlist(sim.A5[[i_run]]$Number_var_incorrect_included)
      ExclusionCorrectness[i_run] <- unlist(sim.A5[[i_run]]$Exclusion_correctness)
      Accuracy_TrueModel_contaminated <- unlist(sim.A5[[i_run]]$Accuracy_TrueModel_contaminated)
      Accuracy_TrueModel_no_contaminated <- unlist(sim.A5[[i_run]]$Accuracy_TrueModel_no_contaminated)
      Accuracy_contaminated[i_run] <- unlist(sim.A5[[i_run]]$Accuracy_contaminated)
      Accuracy_no_contaminated[i_run] <- unlist(sim.A5[[i_run]]$Accuracy_no_contaminated)
    }
  }
  output <- data.frame(Accuracy,
                       ModelSize,
                       InclusionCorrectness,
                       Number_var_incorrect_included,
                       ExclusionCorrectness,
                       Accuracy_TrueModel_contaminated,
                       Accuracy_TrueModel_no_contaminated,
                       Accuracy_contaminated,
                       Accuracy_no_contaminated)
  return(output)
}



cdir[1]
res5<-process_collection(cdir[1])
res5
apply(res5[-c(25,60,81),],2,mean)

cdir[2]
res10 <- process_collection(cdir[2])
res10
apply(res10[-11,],2,mean)

cdir[3]
res15 <- process_collection(cdir[3])
res15

apply(res15[-c(15,59,98)][1:52,],2,mean)


length(res5)

dir(pathEM05)


