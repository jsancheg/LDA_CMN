library(dplyr)
library(tidyverse)
pseudo_labels <- readRDS("pseudo_validation.RDS")

head(pseudo_labels)

pseudo_labels %>% colnames()

table(pseudo_labels$no_labelled_data)

pseudo_labels %>% select(no_labelled_data,CCR_class_sm,sensitivity_class_sm,
                             specificity_class_sm,precision_class_sm,
                             f1_class_sm,CCR_cont_sm,sensitivity_cont_sm,
                             specificity_cont_sm,precision_cont_sm,
                             f1_cont_sm) %>%
dplyr::group_by(no_labelled_data) %>% summarise_at(vars(CCR_class_sm:f1_cont_sm), mean , na.rm = TRUE) 
