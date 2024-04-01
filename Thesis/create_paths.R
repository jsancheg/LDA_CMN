# This is the 1st file to run in the directory


sys_info <- Sys.info()
pc_name <- sys_info["nodename"]

if(pc_name == "LAPTOP-ADR3M911")
{
    cat("Fill the paths")
  
}else if(pc_name == "WildFree")
{
    path_wd <- "/home/jsancheg/git_environment/LDA_CMN/"
    path_thesis <- paste0(path_wd,"Thesis/")
    path_scenarios_real_df <- paste0(path_thesis,"scenarios_real_datasets/")
    path_scenarios_crabs <- paste0(path_scenarios_real_df,"scenarios_crabs/")
    path_scenarios_wine <- paste0(path_scenarios_real_df,"scenarios_wine/")
    path_scenarios_wdbc <- paste0(path_scenarios_real_df,"scenarios_wdbc/")
    path_supervised <- paste0(path_thesis,"supervised/")
    
    path_s_crabs <- paste0(path_supervised,"s_crabs/")
    path_s_wine <- paste0(path_supervised,"s_wine/")
    path_s_wdbc <- paste0(path_supervised,"s_wdbc/")
    
    path_semi_supervised <- paste0(path_thesis,"semi_supervised/")
    path_ss_crabs <- paste0(path_semi_supervised,"ss_crabs/")
    path_ss_wine <- paste0(path_semi_supervised,"ss_wine/")
    path_ss_wdbc <- paste0(path_semi_supervised,"ss_wdbc/")
    
    path_real_data_metrics <- paste0(path_thesis,"real_data_metrics/")
    
}



  

