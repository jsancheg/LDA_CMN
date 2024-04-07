pathwd <- getwd()

source(paste0(pathwd,"/Thesis/create_paths.R"))

list_ssfiles <- dir(pathSSFiles)

aux <- readRDS(paste0(pathSSFiles,"SSV_2_3_5_3000_75_BAL_SCBSV_VO_A8090_E530_10_40.RDS"))

dir(pathSSFiles)
aux$Estimates[[1]]$PseudoInformation_SaturatedM$vtrain
ind_pseudo_label <- aux$Estimates[[1]]$PseudoInformation_SM$Unlabelled_index

table(aux$Estimates[[1]]$PseudoInformation_SM$Pseudo_labels_V_Train, aux$GenData[[1]]$vtrain[ind_pseudo_label])

MLmetrics::Sensitivity(aux$GenData[[1]]$vtrain[ind_pseudo_label],aux$Estimates[[1]]$PseudoInformation_SM$Pseudo_labels_V_Train)

aux$Metrics[1,"RecallV_SM"]
ss_metrics <- lapply()
