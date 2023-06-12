
process_collection3<- function(filepath)
{
  filenames <- paste0(filepath,dir(filepath))
  ind_rds <- str_detect(filenames,".Rdata")
  filenames <- filenames[ind_rds]
  ncollection <- length(filenames)
  res1 <- process_file3(filenames[1])
  Accuracy_TM_cont <- res1$Accuracy_TM_cont
  Accuracy_TM_nocont <- res1$Accuracy_TM_nocont
  Accuracy_SM_cont <- res1$Accuracy_SM_cont
  Accuracy_SM_nocont <- res1$Accuracy_SM_nocont
  
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
    cat("Processing file = ",filenames[i_collection])
    res<- process_file3(filenames[i_collection])
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

process_file3 <- function(filename)
{
  load(file = filename)
  l <- length(sim.A5)
  
  resumen <- sim.A5[[1]]$resumen
  colnames(resumen)[1]<-"Model"
  colnames(resumen)
  
  Accuracy_TM_cont <- data.frame(Sim = 1,
                                 Accuracy_TM_cont1 = sim.A5[[1]]$details[1,1],
                                 Accuracy_TM_cont2 = sim.A5[[1]]$details[2,1],
                                 Accuracy_TM_cont3 = sim.A5[[1]]$details[3,1],
                                 Accuracy_TM_cont4 = sim.A5[[1]]$details[4,1],
                                 Accuracy_TM_cont5 = sim.A5[[1]]$details[5,1],
                                 Accuracy_TM_cont6 = sim.A5[[1]]$details[6,1],
                                 Accuracy_TM_cont7 = sim.A5[[1]]$details[7,1],
                                 Accuracy_TM_cont8 = sim.A5[[1]]$details[8,1],
                                 Accuracy_TM_cont9 = sim.A5[[1]]$details[9,1],
                                 Accuracy_TM_cont10 = sim.A5[[1]]$details[10,1],
                                 Accuracy_TM_cont11 = sim.A5[[1]]$details[11,1],
                                 Accuracy_TM_cont12 = sim.A5[[1]]$details[12,1],
                                 Accuracy_TM_cont13 = sim.A5[[1]]$details[13,1],
                                 Accuracy_TM_cont14 = sim.A5[[1]]$details[14,1],
                                 Accuracy_TM_cont15 = sim.A5[[1]]$details[15,1],
                                 Accuracy_TM_cont16 = sim.A5[[1]]$details[16,1],
                                 Accuracy_TM_cont17 = sim.A5[[1]]$details[17,1],
                                 Accuracy_TM_cont18 = sim.A5[[1]]$details[18,1],
                                 Accuracy_TM_cont19 = sim.A5[[1]]$details[19,1],
                                 Accuracy_TM_cont20 = sim.A5[[1]]$details[20,1])
  
  Accuracy_TM_nocont <- data.frame(Sim = 1,
                                   Accuracy_TM_nocont1 = sim.A5[[1]]$details[1,2],
                                   Accuracy_TM_nocont2 = sim.A5[[1]]$details[2,2],
                                   Accuracy_TM_nocont3 = sim.A5[[1]]$details[3,2],
                                   Accuracy_TM_nocont4 = sim.A5[[1]]$details[4,2],
                                   Accuracy_TM_nocont5 = sim.A5[[1]]$details[5,2],
                                   Accuracy_TM_nocont6 = sim.A5[[1]]$details[6,2],
                                   Accuracy_TM_nocont7 = sim.A5[[1]]$details[7,2],
                                   Accuracy_TM_nocont8 = sim.A5[[1]]$details[8,2],
                                   Accuracy_TM_nocont9 = sim.A5[[1]]$details[9,2],
                                   Accuracy_TM_nocont10 = sim.A5[[1]]$details[10,2],
                                   Accuracy_TM_nocont11 = sim.A5[[1]]$details[11,2],
                                   Accuracy_TM_nocont12 = sim.A5[[1]]$details[12,2],
                                   Accuracy_TM_nocont13 = sim.A5[[1]]$details[13,2],
                                   Accuracy_TM_nocont14 = sim.A5[[1]]$details[14,2],
                                   Accuracy_TM_nocont15 = sim.A5[[1]]$details[15,2],
                                   Accuracy_TM_nocont16 = sim.A5[[1]]$details[16,2],
                                   Accuracy_TM_nocont17 = sim.A5[[1]]$details[17,2],
                                   Accuracy_TM_nocont18 = sim.A5[[1]]$details[18,2],
                                   Accuracy_TM_nocont19 = sim.A5[[1]]$details[19,2],
                                   Accuracy_TM_nocont20 = sim.A5[[1]]$details[20,2])
  
  Accuracy_SM_cont <- data.frame(Sim = 1,
                                 Accuracy_SM_cont1 = sim.A5[[1]]$details[1,3],
                                 Accuracy_SM_cont2 = sim.A5[[1]]$details[2,3],
                                 Accuracy_SM_cont3 = sim.A5[[1]]$details[3,3],
                                 Accuracy_SM_cont4 = sim.A5[[1]]$details[4,3],
                                 Accuracy_SM_cont5 = sim.A5[[1]]$details[5,3],
                                 Accuracy_SM_cont6 = sim.A5[[1]]$details[6,3],
                                 Accuracy_SM_cont7 = sim.A5[[1]]$details[7,3],
                                 Accuracy_SM_cont8 = sim.A5[[1]]$details[8,3],
                                 Accuracy_SM_cont9 = sim.A5[[1]]$details[9,3],
                                 Accuracy_SM_cont10 = sim.A5[[1]]$details[10,3],
                                 Accuracy_SM_cont11 = sim.A5[[1]]$details[11,3],
                                 Accuracy_SM_cont12 = sim.A5[[1]]$details[12,3],
                                 Accuracy_SM_cont13 = sim.A5[[1]]$details[13,3],
                                 Accuracy_SM_cont14 = sim.A5[[1]]$details[14,3],
                                 Accuracy_SM_cont15 = sim.A5[[1]]$details[15,3],
                                 Accuracy_SM_cont16 = sim.A5[[1]]$details[16,3],
                                 Accuracy_SM_cont17 = sim.A5[[1]]$details[17,3],
                                 Accuracy_SM_cont18 = sim.A5[[1]]$details[18,3],
                                 Accuracy_SM_cont19 = sim.A5[[1]]$details[19,3],
                                 Accuracy_SM_cont20 = sim.A5[[1]]$details[20,3])
  
  Accuracy_SM_nocont <- data.frame(Sim = 1,
                                   Accuracy_SM_nocont1 = sim.A5[[1]]$details[1,4],
                                   Accuracy_SM_nocont2 = sim.A5[[1]]$details[2,4],
                                   Accuracy_SM_nocont3 = sim.A5[[1]]$details[3,4],
                                   Accuracy_SM_nocont4 = sim.A5[[1]]$details[4,4],
                                   Accuracy_SM_nocont5 = sim.A5[[1]]$details[5,4],
                                   Accuracy_SM_nocont6 = sim.A5[[1]]$details[6,4],
                                   Accuracy_SM_nocont7 = sim.A5[[1]]$details[7,4],
                                   Accuracy_SM_nocont8 = sim.A5[[1]]$details[8,4],
                                   Accuracy_SM_nocont9 = sim.A5[[1]]$details[9,4],
                                   Accuracy_SM_nocont10 = sim.A5[[1]]$details[10,4],
                                   Accuracy_SM_nocont11 = sim.A5[[1]]$details[11,4],
                                   Accuracy_SM_nocont12 = sim.A5[[1]]$details[12,4],
                                   Accuracy_SM_nocont13 = sim.A5[[1]]$details[13,4],
                                   Accuracy_SM_nocont14 = sim.A5[[1]]$details[14,4],
                                   Accuracy_SM_nocont15 = sim.A5[[1]]$details[15,4],
                                   Accuracy_SM_nocont16 = sim.A5[[1]]$details[16,4],
                                   Accuracy_SM_nocont17 = sim.A5[[1]]$details[17,4],
                                   Accuracy_SM_nocont18 = sim.A5[[1]]$details[18,4],
                                   Accuracy_SM_nocont19 = sim.A5[[1]]$details[19,4],
                                   Accuracy_SM_nocont20 = sim.A5[[1]]$details[20,4])
  
  for(i_run in 2:l)
  {
    cat ("\n run =", i_run, "\n")
    if(length(sim.A5[[i_run]])>1)
    {
      auxdf1<- sim.A5[[i_run]]$resumen
      colnames(auxdf1)[1] <- "Model"
      
      resumen <- rbind.data.frame(resumen,auxdf1)
      aux_TM_cont <- data.frame(Sim =i_run, 
                                Accuracy_TM_cont1 = sim.A5[[i_run]]$details[1,1],
                                Accuracy_TM_cont2 = sim.A5[[i_run]]$details[2,1],
                                Accuracy_TM_cont3 = sim.A5[[i_run]]$details[3,1],
                                Accuracy_TM_cont4 = sim.A5[[i_run]]$details[4,1],
                                Accuracy_TM_cont5 = sim.A5[[i_run]]$details[5,1],
                                Accuracy_TM_cont6 = sim.A5[[i_run]]$details[6,1],
                                Accuracy_TM_cont7 = sim.A5[[i_run]]$details[7,1],
                                Accuracy_TM_cont8 = sim.A5[[i_run]]$details[8,1],
                                Accuracy_TM_cont9 = sim.A5[[i_run]]$details[9,1],
                                Accuracy_TM_cont10 = sim.A5[[i_run]]$details[10,1],
                                Accuracy_TM_cont11 = sim.A5[[i_run]]$details[11,1],
                                Accuracy_TM_cont12 = sim.A5[[i_run]]$details[12,1],
                                Accuracy_TM_cont13 = sim.A5[[i_run]]$details[13,1],
                                Accuracy_TM_cont14 = sim.A5[[i_run]]$details[14,1],
                                Accuracy_TM_cont15 = sim.A5[[i_run]]$details[15,1],
                                Accuracy_TM_cont16 = sim.A5[[i_run]]$details[16,1],
                                Accuracy_TM_cont17 = sim.A5[[i_run]]$details[17,1],
                                Accuracy_TM_cont18 = sim.A5[[i_run]]$details[18,1],
                                Accuracy_TM_cont19 = sim.A5[[i_run]]$details[19,1],
                                Accuracy_TM_cont20 = sim.A5[[i_run]]$details[20,1])
      aux_TM_nocont <- data.frame(Sim =i_run, 
                                  Accuracy_TM_nocont1 = sim.A5[[i_run]]$details[1,2],
                                  Accuracy_TM_nocont2 = sim.A5[[i_run]]$details[2,2],
                                  Accuracy_TM_nocont3 = sim.A5[[i_run]]$details[3,2],
                                  Accuracy_TM_nocont4 = sim.A5[[i_run]]$details[4,2],
                                  Accuracy_TM_nocont5 = sim.A5[[i_run]]$details[5,2],
                                  Accuracy_TM_nocont6 = sim.A5[[i_run]]$details[6,2],
                                  Accuracy_TM_nocont7 = sim.A5[[i_run]]$details[7,2],
                                  Accuracy_TM_nocont8 = sim.A5[[i_run]]$details[8,2],
                                  Accuracy_TM_nocont9 = sim.A5[[i_run]]$details[9,2],
                                  Accuracy_TM_nocont10 = sim.A5[[i_run]]$details[10,2],
                                  Accuracy_TM_nocont11 = sim.A5[[i_run]]$details[11,2],
                                  Accuracy_TM_nocont12 = sim.A5[[i_run]]$details[12,2],
                                  Accuracy_TM_nocont13 = sim.A5[[i_run]]$details[13,2],
                                  Accuracy_TM_nocont14 = sim.A5[[i_run]]$details[14,2],
                                  Accuracy_TM_nocont15 = sim.A5[[i_run]]$details[15,2],
                                  Accuracy_TM_nocont16 = sim.A5[[i_run]]$details[16,2],
                                  Accuracy_TM_nocont17 = sim.A5[[i_run]]$details[17,2],
                                  Accuracy_TM_nocont18 = sim.A5[[i_run]]$details[18,2],
                                  Accuracy_TM_nocont19 = sim.A5[[i_run]]$details[19,2],
                                  Accuracy_TM_nocont20 = sim.A5[[i_run]]$details[20,2])
      aux_SM_cont <- data.frame(Sim =i_run, 
                                Accuracy_SM_cont1 = sim.A5[[i_run]]$details[1,3],
                                Accuracy_SM_cont2 = sim.A5[[i_run]]$details[2,3],
                                Accuracy_SM_cont3 = sim.A5[[i_run]]$details[3,3],
                                Accuracy_SM_cont4 = sim.A5[[i_run]]$details[4,3],
                                Accuracy_SM_cont5 = sim.A5[[i_run]]$details[5,3],
                                Accuracy_SM_cont6 = sim.A5[[i_run]]$details[6,3],
                                Accuracy_SM_cont7 = sim.A5[[i_run]]$details[7,3],
                                Accuracy_SM_cont8 = sim.A5[[i_run]]$details[8,3],
                                Accuracy_SM_cont9 = sim.A5[[i_run]]$details[9,3],
                                Accuracy_SM_cont10 = sim.A5[[i_run]]$details[10,3],
                                Accuracy_SM_cont11 = sim.A5[[i_run]]$details[11,3],
                                Accuracy_SM_cont12 = sim.A5[[i_run]]$details[12,3],
                                Accuracy_SM_cont13 = sim.A5[[i_run]]$details[13,3],
                                Accuracy_SM_cont14 = sim.A5[[i_run]]$details[14,3],
                                Accuracy_SM_cont15 = sim.A5[[i_run]]$details[15,3],
                                Accuracy_SM_cont16 = sim.A5[[i_run]]$details[16,3],
                                Accuracy_SM_cont17 = sim.A5[[i_run]]$details[17,3],
                                Accuracy_SM_cont18 = sim.A5[[i_run]]$details[18,3],
                                Accuracy_SM_cont19 = sim.A5[[i_run]]$details[19,3],
                                Accuracy_SM_cont20 = sim.A5[[i_run]]$details[20,3])
      aux_SM_nocont <- data.frame(Sim =i_run, 
                                  Accuracy_SM_nocont1 = sim.A5[[i_run]]$details[1,4],
                                  Accuracy_SM_nocont2 = sim.A5[[i_run]]$details[2,4],
                                  Accuracy_SM_nocont3 = sim.A5[[i_run]]$details[3,4],
                                  Accuracy_SM_nocont4 = sim.A5[[i_run]]$details[4,4],
                                  Accuracy_SM_nocont5 = sim.A5[[i_run]]$details[5,4],
                                  Accuracy_SM_nocont6 = sim.A5[[i_run]]$details[6,4],
                                  Accuracy_SM_nocont7 = sim.A5[[i_run]]$details[7,4],
                                  Accuracy_SM_nocont8 = sim.A5[[i_run]]$details[8,4],
                                  Accuracy_SM_nocont9 = sim.A5[[i_run]]$details[9,4],
                                  Accuracy_SM_nocont10 = sim.A5[[i_run]]$details[10,4],
                                  Accuracy_SM_nocont11 = sim.A5[[i_run]]$details[11,4],
                                  Accuracy_SM_nocont12 = sim.A5[[i_run]]$details[12,4],
                                  Accuracy_SM_nocont13 = sim.A5[[i_run]]$details[13,4],
                                  Accuracy_SM_nocont14 = sim.A5[[i_run]]$details[14,4],
                                  Accuracy_SM_nocont15 = sim.A5[[i_run]]$details[15,4],
                                  Accuracy_SM_nocont16 = sim.A5[[i_run]]$details[16,4],
                                  Accuracy_SM_nocont17 = sim.A5[[i_run]]$details[17,4],
                                  Accuracy_SM_nocont18 = sim.A5[[i_run]]$details[18,4],
                                  Accuracy_SM_nocont19 = sim.A5[[i_run]]$details[19,4],
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

find_unique_labels<-function(labels)
{  
  filas <- length(labels) 
  rawlabels<-unique(labels)
  rawlabels <- sort(rawlabels)
  nrawlabels <- length(rawlabels)
  transformlabels<-rawlabels
  registerchanges <- rep(0,nrawlabels)
  
  for(i_raw1 in 1:(nrawlabels-1) )
  {
   a <- unlist(str_split(rawlabels[i_raw1],"-")) 
    for(i_raw2 in 2:nrawlabels)
    {
      b <- unlist(str_split(rawlabels[i_raw2],"-")) 
      # compare two list of variables without taking in account order of variables
      if(registerchanges[i_raw2]==0)
        if(setequal(a,b)) 
        {
          transformlabels[i_raw1]<-rawlabels[i_raw1]
          transformlabels[i_raw2]<-rawlabels[i_raw1]
          registerchanges[i_raw2]<- 1
        } # end-if
    }# end-for i_raw2
  }# end-for i_raw1
  uniquelabels<- unique(transformlabels)
  nuniquelabels <- length(uniquelabels)
  newlabels <- labels
  registerchangedlabels <- rep(0,filas)
  for(i_raw1 in 1:(nuniquelabels) )
  {
    a <-  unlist(str_split(uniquelabels[i_raw1],"-"))
    
    for(i_raw2 in 1:filas)
    {
      b <- unlist(str_split(labels[i_raw2],"-")) 
      # compare two list of variables without taking in account order of variables
      if(registerchangedlabels[i_raw2]==0)
        if(setequal(a,b)) 
        {
         # newlabels[i_raw1]<-uniquelabels[i_raw1]
          newlabels[i_raw2]<-uniquelabels[i_raw1]
          registerchangedlabels[i_raw2]<- 1
        } # end-if
    }# end-for i_raw2
  }# end-for i_raw1
  
  #cbind.data.frame(labels,newlabels)
  #uniquelabels<-unique(transformlabels)
  #uniquelabels
  return(cbind.data.frame(labels,newlabels))
}




BigDf <- function(simulation, pathOutput,pathImages)
{
  nfolders <- length(simulation)
  for (i in 1:nfolders)
  {
    cat("Processing i",i,"-",simulation[i])
    pathOutput <- paste0(pathPro,simulation[i],"/") 
    filepath <- pathOutput
    cdir <- paste0(c(pathOutput))
    res <- process_collection3(cdir)
    head(res$resumen)
    
    df_resumen <- res$resumen
    colnames(df_resumen)
    head(df_resumen)
    df_resumen <- mutate(df_resumen,Model1 = Model)
    df_resumen <- df_resumen %>% relocate(Model1, .after = Model)
    colnames(df_resumen)
    df_resumen <- df_resumen %>% relocate(Model, .after = Model1)
    colnames(df_resumen)
    
    #  df_resumen <- rename(df_resumen,Model1 = Model)
    
    
    labels<-df_resumen$Model1
    
    find_unique_labels(labels)
    df_resumen$Model <- find_unique_labels(df_resumen$Model1)[,2]
    df_resumen$IncludeX2 <- as.numeric(str_detect(res$resumen$Model,"X2"))
    df_resumen$IncludeX4 <- as.numeric(str_detect(res$resumen$Model,"X4"))
    #df_resumen <- dplyr::mutate(res$resumen, 
    #                            IncludeX2 = as.numeric(str_detect(res$resumen$Model,"X2")),
    #                            IncludeX4 = as.numeric(str_detect(res$resumen$Model,"X4")) )
    
    head(df_resumen)
    
    png(file = paste0(pathImages,simulation[i],
                      "/BoxplotModelSize",simulation[i],".png"),
        width = 300, height = 300, pointsize = 12, bg = "white",
        restoreConsole = TRUE )
    
    boxplot(df_resumen$ModelSizeSM,
            main = "Size of the selected model",
            col = "aliceblue")
    dev.off()
    
    
    png(file = paste0(pathImages,simulation[i],
                      "/BoxplotIncorrectVar",simulation[i],".png"),
        width = 500, height = 500, pointsize = 12, bg = "white",
        restoreConsole = TRUE )
    
    boxplot(df_resumen$Number_var_incorrect_included,
            main = "Number of incorrect variables in the selected model",
            col = "aliceblue")
    dev.off()
    
    aux_df_acc_comp <- data.frame(True = df_resumen$AccuracyTM, 
                                  Selected = df_resumen$AccuracySM)
    
    dfOverallAccuracyComp <- aux_df_acc_comp %>% pivot_longer(cols = True:Selected,
                                                              names_to = "Model",
                                                              values_to = "Accuracy")
    dfOverallAccuracyComp 
    
    png(file = paste0(pathImages,simulation[i],
                      "/BoxplotTMvsSM",simulation[i],".png"),
        width = 500, height = 500, pointsize = 12, bg = "white",
        restoreConsole = TRUE )
    
    
    boxplot(Accuracy ~ Model, data = dfOverallAccuracyComp,
            col = "aliceblue", ylab = "Accuracy", xlab = "Variables",
            main = "Comparison between true model versus selected model")
    dev.off()
    
    
    
    tab1 <- table(df_resumen$Model)
    my_tab1_sort <- tab1[order(tab1, decreasing = TRUE)]
    df_models <- data.frame(frequency = as.vector(my_tab1_sort),
                            model = rownames(my_tab1_sort))
    
    
    df_models <- df_models[order(-df_models$frequency),]
    class(df_models)
    
    plt_boxplotmodels <- ggplot(df_models) +
      geom_col(aes(reorder(model,-frequency),frequency),fill = "#076fa2", width = 0.6)
    
    plt_boxplotmodels
    
    
    
    plt_boxplotmodels <- ggplot(df_models) +
      geom_col(aes(frequency,model),fill = "#076fa2", width = 0.6) +
      ylab("Variables")
    
    
    plt_boxplotmodels
    
    
    ggp <- ggplot(df_models, aes(x = reorder(model, +frequency),y = frequency) ) +
      geom_bar(stat = "identity", fill = "lightblue") +
      coord_flip()+
      ylab("Frequency") + xlab("Variables") +
      geom_text(aes(x = model, y = frequency + 0.3, label = frequency),check_overlap = TRUE)
    
    png(file = paste0(pathImages,simulation[i],
                      "/Frequency_models_",simulation[i],".png"),
        width = 300, height = 300, pointsize = 12, bg = "white",
        restoreConsole = TRUE )
    
    ggp
    
    dev.off()
    
    
    library(plotly)
    ggp %>% ggplotly
    
    
    head(df_resumen)
    head(res$Accuracy_TM_contaminated)
    df_resumen$Accuracy_TM_cont <- res$Accuracy_TM_contaminated$Accuracy_TM_cont20
    df_resumen$Accuracy_TM_nocont <- res$Accuracy_TM_no_contaminated$Accuracy_TM_nocont20
    df_resumen$Accuracy_SM_cont <- res$Accuracy_SM_contaminated$Accuracy_SM_cont20
    df_resumen$Accuracy_SM_nocont <- res$Accuracy_SM_no_contaminated$Accuracy_SM_nocont20
    
    df_resumen <- df_resumen %>% 
      relocate(IncludeX2,IncludeX4, .after = ModelSizeSM)
    df_resumen <- df_resumen %>%
      relocate(Exclusion_correctness, .after = Inclusion_correctness)
    df_resumen$Setting <- simulation[i]
    df_resumen <- df_resumen %>%
      relocate(Setting, .before = Model1)
    head(df_resumen)
    
    # run again the 100 variable cases adding missed columns
    #    if(ncol(dfBig)<ncol(df_resumen)) 
    #        df_resumen <- df_resumen[,intersect(colnames(dfBig),colnames(df_resumen))]
    #    if (ncol(df_resumen)< ncol(dfBig))
    #      dfBig <- dfBig[,intersect(colnames(dfBig),colnames(df_resumen))]
    
    
    if(i == 1) dfBig <- df_resumen else dfBig <- rbind.data.frame(df_resumen, dfBig)
    # calculate the mean of metrics for each setting
    # check why precision_SM_V has NA in one of the simulations
    
    auxmean<-df_resumen %>% dplyr::summarise(across(where(is.numeric),~ mean(.x, na.rm = TRUE)))
    auxmean$case <- simulation[i]
    auxmean <- auxmean %>% 
      relocate(case, .before = AccuracyTM)
    
    if(i == 1) dfmean <- auxmean else dfmean <- rbind.data.frame(dfmean,auxmean) 
    
    # standard deviation
    auxsd<-df_resumen %>% dplyr::summarise(across(where(is.numeric),~ sd(.x, na.rm = TRUE)))
    auxsd$case <- simulation[i]
    auxsd <- auxsd %>% 
      relocate(case, .before = AccuracyTM)
    if(i == 1) dfsd <- auxsd else dfsd <- rbind.data.frame(dfsd,auxsd) 
    
    
    # apply(df_resumen[,-c(1:2)],2,sd)
    if( i == nfolders)
    {
      saveRDS(dfmean, file = paste0(pathImages,
                                    "/meanMetrics.RDS"))
      saveRDS(dfsd, file = paste0(pathImages,
                                  "/sdMetrics.RDS"))
      saveRDS(dfBig,file = paste0(pathImages,
                                  "/Metrics.RDS"))
      
    }
    
    
    names(res$resumen)
    
    aux_TM_cont <- res$Accuracy_TM_contaminated
    ncol_TM_cont <- ncol(aux_TM_cont)
    names(aux_TM_cont)[2:ncol_TM_cont] <- paste0("Step",1:(length(aux_TM_cont)-1 )) 
    
    df_TM_cont <- aux_TM_cont %>% pivot_longer(!Sim,names_to="EM_Stop", values_to = "Accuracy_TM_cont")
    df_TM_cont <- df_TM_cont %>% mutate(EM_Step = as.numeric(str_extract(df_TM_cont$EM_Stop,"(\\d)+")))
    df_TM_cont <- df_TM_cont %>% mutate(EM_Stop = NULL)
    
    
    head(df_TM_cont)
    
    png(file = paste0(pathImages,simulation[i],
                      "/AccTMcont",simulation[i],".png"),
        width = 500, height = 500, pointsize = 12, bg = "white",
        restoreConsole = TRUE )
    
    
    boxplot(Accuracy_TM_cont ~ EM_Step, data = df_TM_cont,
            xlab = "EM Step", ylab = "Accuracy", 
            main = "Accuracy true model in contaminated samples")
    
    dev.off()
    
    aux_TM_no_cont <- res$Accuracy_TM_no_contaminated
    ncol_TM_no_cont <- ncol(aux_TM_no_cont)
    names(aux_TM_no_cont)[2:ncol_TM_no_cont] <- paste0("Step",1:(length(aux_TM_no_cont)-1 )) 
    
    df_TM_no_cont <- aux_TM_no_cont %>% 
      pivot_longer(!Sim,names_to="EM_Stop", values_to = "Accuracy_TM_no_cont")
    
    df_TM_no_cont <- df_TM_no_cont %>% 
      mutate(EM_Step = as.numeric(str_extract(df_TM_no_cont$EM_Stop,"(\\d)+")))
    df_TM_no_cont <- df_TM_no_cont %>% mutate(EM_Stop = NULL)
    
    
    head(df_TM_no_cont)
    
    png(file = paste0(pathImages,simulation[i],
                      "/AccTMnoCont",simulation[i],".png"),
        width = 500, height = 500, pointsize = 12, bg = "white",
        restoreConsole = TRUE )
    
    boxplot(Accuracy_TM_no_cont ~ EM_Step, data = df_TM_no_cont,
            xlab = "EM Step", ylab = "Accuracy", 
            main = "Accuracy true model in non-contaminated samples")
    dev.off()
    
    
    aux_SM_cont <- res$Accuracy_SM_contaminated
    ncol_SM_cont <- ncol(aux_SM_cont)
    names(aux_SM_cont)[2:ncol_SM_cont] <- paste0("Step",1:(length(aux_SM_cont)-1 )) 
    
    df_SM_cont <- aux_SM_cont %>% pivot_longer(!Sim,names_to="EM_Stop", 
                                               values_to = "Accuracy_SM_cont")
    df_SM_cont <- df_SM_cont %>% mutate(EM_Step = as.numeric(str_extract(df_SM_cont$EM_Stop,"(\\d)+")))
    df_SM_cont <- df_SM_cont %>% mutate(EM_Stop = NULL)
    
    
    head(df_SM_cont)
    
    
    png(file = paste0(pathImages,simulation[i],
                      "/AccSMcont",simulation[i],".png"),
        width = 500, height = 500, pointsize = 12, bg = "white",
        restoreConsole = TRUE )
    
    boxplot(Accuracy_SM_cont ~ EM_Step, data = df_SM_cont,
            xlab = "EM Step", ylab = "Accuracy", 
            main = "Accuracy selected model in contaminated samples")
    
    dev.off()
    
    
    aux_SM_no_cont <- res$Accuracy_SM_no_contaminated
    ncol_SM_no_cont <- ncol(aux_SM_no_cont)
    names(aux_SM_no_cont)[2:ncol_SM_no_cont] <- paste0("Step",1:(length(aux_SM_no_cont)-1 )) 
    
    df_SM_no_cont <- aux_SM_no_cont %>% 
      pivot_longer(!Sim,names_to="EM_Stop", values_to = "Accuracy_SM_no_cont")
    
    df_SM_no_cont <- df_SM_no_cont %>% 
      mutate(EM_Step = as.numeric(str_extract(df_SM_no_cont$EM_Stop,"(\\d)+")))
    df_SM_no_cont <- df_SM_no_cont %>% mutate(EM_Stop = NULL)
    
    
    head(df_SM_no_cont)
    png(file = paste0(pathImages,simulation[i],
                      "/AccSMnoCont",simulation[i],".png"),
        width = 500, height = 500, pointsize = 12, bg = "white",
        restoreConsole = TRUE )
    
    boxplot(Accuracy_SM_no_cont ~ EM_Step, data = df_SM_no_cont,
            xlab = "EM Step", ylab = "Accuracy", 
            main = "Accuracy selected model in non-contaminated samples")
    
    dev.off()
    
  }
  
}




CreateResumen <- function(simulation, pathOutput,pathImages)
{
  nfolders <- length(simulation)
  for (i in 1:nfolders)
  {
    pathOutput <- paste0(pathPro,simulation[i],"/") 
    
    filepath <- pathOutput
    
    
    
    cdir <- paste0(c(pathOutput))
    res <- process_collection3(cdir)
    head(res$resumen)
    
    df_resumen <- res$resumen
    colnames(df_resumen)
    head(df_resumen)
    df_resumen <- mutate(df_resumen,Model1 = Model)
    df_resumen <- df_resumen %>% relocate(Model1, .after = Model)
    colnames(df_resumen)
    df_resumen <- df_resumen %>% relocate(Model, .after = Model1)
    colnames(df_resumen)
    
    #  df_resumen <- rename(df_resumen,Model1 = Model)
    
    
    labels<-df_resumen$Model1
    
    find_unique_labels(labels)
    df_resumen$Model <- find_unique_labels(df_resumen$Model1)[,2]
    df_resumen$IncludeX2 <- as.numeric(str_detect(res$resumen$Model,"X2"))
    df_resumen$IncludeX4 <- as.numeric(str_detect(res$resumen$Model,"X4"))
    #df_resumen <- dplyr::mutate(res$resumen, 
    #                            IncludeX2 = as.numeric(str_detect(res$resumen$Model,"X2")),
    #                            IncludeX4 = as.numeric(str_detect(res$resumen$Model,"X4")) )
    
    head(df_resumen)
    
    png(file = paste0(pathImages,simulation[i],
                      "/BoxplotModelSize",simulation[i],".png"),
        width = 300, height = 300, pointsize = 12, bg = "white",
        restoreConsole = TRUE )
    
    boxplot(df_resumen$ModelSizeSM,
            main = "Size of the selected model",
            col = "aliceblue")
    dev.off()
    
    
    png(file = paste0(pathImages,simulation[i],
                      "/BoxplotIncorrectVar",simulation[i],".png"),
        width = 500, height = 500, pointsize = 12, bg = "white",
        restoreConsole = TRUE )
    
    boxplot(df_resumen$Number_var_incorrect_included,
            main = "Number of incorrect variables in the selected model",
            col = "aliceblue")
    dev.off()
    
    aux_df_acc_comp <- data.frame(True = df_resumen$AccuracyTM, 
                                  Selected = df_resumen$AccuracySM)
    
    dfOverallAccuracyComp <- aux_df_acc_comp %>% pivot_longer(cols = True:Selected,
                                                              names_to = "Model",
                                                              values_to = "Accuracy")
    dfOverallAccuracyComp 
    
    png(file = paste0(pathImages,simulation[i],
                      "/BoxplotTMvsSM",simulation[i],".png"),
        width = 500, height = 500, pointsize = 12, bg = "white",
        restoreConsole = TRUE )
    
    
    boxplot(Accuracy ~ Model, data = dfOverallAccuracyComp,
            col = "aliceblue", ylab = "Accuracy", xlab = "Variables",
            main = "Comparison between true model versus selected model")
    dev.off()
    
    
    
    tab1 <- table(df_resumen$Model)
    my_tab1_sort <- tab1[order(tab1, decreasing = TRUE)]
    df_models <- data.frame(frequency = as.vector(my_tab1_sort),
                            model = rownames(my_tab1_sort))
    
    
    df_models <- df_models[order(-df_models$frequency),]
    class(df_models)
    
    plt_boxplotmodels <- ggplot(df_models) +
      geom_col(aes(reorder(model,-frequency),frequency),fill = "#076fa2", width = 0.6)
    
    plt_boxplotmodels
    
    
    
    plt_boxplotmodels <- ggplot(df_models) +
      geom_col(aes(frequency,model),fill = "#076fa2", width = 0.6) +
      ylab("Variables")
    
    
    plt_boxplotmodels
    
    
    ggp <- ggplot(df_models, aes(x = reorder(model, +frequency),y = frequency) ) +
      geom_bar(stat = "identity", fill = "lightblue") +
      coord_flip()+
      ylab("Frequency") + xlab("Variables") +
      geom_text(aes(x = model, y = frequency + 0.3, label = frequency),check_overlap = TRUE)
    
    png(file = paste0(pathImages,simulation[i],
                      "/Frequency_models_",simulation[i],".png"),
        width = 300, height = 300, pointsize = 12, bg = "white",
        restoreConsole = TRUE )
    
    ggp
    
    dev.off()
    
    
    library(plotly)
    ggp %>% ggplotly
    
    
    head(df_resumen)
    head(res$Accuracy_TM_contaminated)
    df_resumen$Accuracy_TM_cont <- res$Accuracy_TM_contaminated$Accuracy_TM_cont20
    df_resumen$Accuracy_TM_nocont <- res$Accuracy_TM_no_contaminated$Accuracy_TM_nocont20
    df_resumen$Accuracy_SM_cont <- res$Accuracy_SM_contaminated$Accuracy_SM_cont20
    df_resumen$Accuracy_SM_nocont <- res$Accuracy_SM_no_contaminated$Accuracy_SM_nocont20
    
    df_resumen <- df_resumen %>% 
      relocate(IncludeX2,IncludeX4, .after = ModelSizeSM)
    df_resumen <- df_resumen %>%
      relocate(Exclusion_correctness, .after = Inclusion_correctness)
    
    head(df_resumen)
    
    # calculate the mean of metrics for each setting
    # check why precision_SM_V has NA in one of the simulations
    
    auxmean<-df_resumen %>% dplyr::summarise(across(where(is.numeric),~ mean(.x, na.rm = TRUE)))
    auxmean$case <- simulation[i]
    auxmean <- auxmean %>% 
      relocate(case, .before = AccuracyTM)
    
    if(i == 1) dfmean <- auxmean else dfmean <- rbind.data.frame(dfmean,auxmean) 
    
    # standard deviation
    auxsd<-df_resumen %>% dplyr::summarise(across(where(is.numeric),~ sd(.x, na.rm = TRUE)))
    auxsd$case <- simulation[i]
    auxsd <- auxsd %>% 
      relocate(case, .before = AccuracyTM)
    if(i == 1) dfsd <- auxsd else dfsd <- rbind.data.frame(dfsd,auxsd) 
    
    
    # apply(df_resumen[,-c(1:2)],2,sd)
    if( i == nfolders)
    {
      saveRDS(dfmean, file = paste0(pathImages,
                                    "/meanMetrics.RDS"))
      saveRDS(dfsd, file = paste0(pathImages,
                                  "/sdMetrics.RDS"))
      
    }
    
    
    names(res$resumen)
    
    aux_TM_cont <- res$Accuracy_TM_contaminated
    ncol_TM_cont <- ncol(aux_TM_cont)
    names(aux_TM_cont)[2:ncol_TM_cont] <- paste0("Step",1:(length(aux_TM_cont)-1 )) 
    
    df_TM_cont <- aux_TM_cont %>% pivot_longer(!Sim,names_to="EM_Stop", values_to = "Accuracy_TM_cont")
    df_TM_cont <- df_TM_cont %>% mutate(EM_Step = as.numeric(str_extract(df_TM_cont$EM_Stop,"(\\d)+")))
    df_TM_cont <- df_TM_cont %>% mutate(EM_Stop = NULL)
    
    
    head(df_TM_cont)
    
    png(file = paste0(pathImages,simulation[i],
                      "/AccTMcont",simulation[i],".png"),
        width = 500, height = 500, pointsize = 12, bg = "white",
        restoreConsole = TRUE )
    
    
    boxplot(Accuracy_TM_cont ~ EM_Step, data = df_TM_cont,
            xlab = "EM Step", ylab = "Accuracy", 
            main = "Accuracy true model in contaminated samples")
    
    dev.off()
    
    aux_TM_no_cont <- res$Accuracy_TM_no_contaminated
    ncol_TM_no_cont <- ncol(aux_TM_no_cont)
    names(aux_TM_no_cont)[2:ncol_TM_no_cont] <- paste0("Step",1:(length(aux_TM_no_cont)-1 )) 
    
    df_TM_no_cont <- aux_TM_no_cont %>% 
      pivot_longer(!Sim,names_to="EM_Stop", values_to = "Accuracy_TM_no_cont")
    
    df_TM_no_cont <- df_TM_no_cont %>% 
      mutate(EM_Step = as.numeric(str_extract(df_TM_no_cont$EM_Stop,"(\\d)+")))
    df_TM_no_cont <- df_TM_no_cont %>% mutate(EM_Stop = NULL)
    
    
    head(df_TM_no_cont)
    
    png(file = paste0(pathImages,simulation[i],
                      "/AccTMnoCont",simulation[i],".png"),
        width = 500, height = 500, pointsize = 12, bg = "white",
        restoreConsole = TRUE )
    
    boxplot(Accuracy_TM_no_cont ~ EM_Step, data = df_TM_no_cont,
            xlab = "EM Step", ylab = "Accuracy", 
            main = "Accuracy true model in non-contaminated samples")
    dev.off()
    
    
    aux_SM_cont <- res$Accuracy_SM_contaminated
    ncol_SM_cont <- ncol(aux_SM_cont)
    names(aux_SM_cont)[2:ncol_SM_cont] <- paste0("Step",1:(length(aux_SM_cont)-1 )) 
    
    df_SM_cont <- aux_SM_cont %>% pivot_longer(!Sim,names_to="EM_Stop", 
                                               values_to = "Accuracy_SM_cont")
    df_SM_cont <- df_SM_cont %>% mutate(EM_Step = as.numeric(str_extract(df_SM_cont$EM_Stop,"(\\d)+")))
    df_SM_cont <- df_SM_cont %>% mutate(EM_Stop = NULL)
    
    
    head(df_SM_cont)
    
    
    png(file = paste0(pathImages,simulation[i],
                      "/AccSMcont",simulation[i],".png"),
        width = 500, height = 500, pointsize = 12, bg = "white",
        restoreConsole = TRUE )
    
    boxplot(Accuracy_SM_cont ~ EM_Step, data = df_SM_cont,
            xlab = "EM Step", ylab = "Accuracy", 
            main = "Accuracy selected model in contaminated samples")
    
    dev.off()
    
    
    aux_SM_no_cont <- res$Accuracy_SM_no_contaminated
    ncol_SM_no_cont <- ncol(aux_SM_no_cont)
    names(aux_SM_no_cont)[2:ncol_SM_no_cont] <- paste0("Step",1:(length(aux_SM_no_cont)-1 )) 
    
    df_SM_no_cont <- aux_SM_no_cont %>% 
      pivot_longer(!Sim,names_to="EM_Stop", values_to = "Accuracy_SM_no_cont")
    
    df_SM_no_cont <- df_SM_no_cont %>% 
      mutate(EM_Step = as.numeric(str_extract(df_SM_no_cont$EM_Stop,"(\\d)+")))
    df_SM_no_cont <- df_SM_no_cont %>% mutate(EM_Stop = NULL)
    
    
    head(df_SM_no_cont)
    png(file = paste0(pathImages,simulation[i],
                      "/AccSMnoCont",simulation[i],".png"),
        width = 500, height = 500, pointsize = 12, bg = "white",
        restoreConsole = TRUE )
    
    boxplot(Accuracy_SM_no_cont ~ EM_Step, data = df_SM_no_cont,
            xlab = "EM Step", ylab = "Accuracy", 
            main = "Accuracy selected model in non-contaminated samples")
    
    dev.off()
    
  }
  
}
