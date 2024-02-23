NameMetricFile
# Metrics_SSFiles.RDS file with metrics of Semi-supervised learning fitted models (SSFiles)
# Metrics_SFiles.RDS file with metrics of Supervised learning fitted models (SFiles)

SSmetrics <- readRDS("Metrics_SSFiles.RDS")



# Plot Model Size Selected Variables --------------------------------------


data <- SSmetrics %>% filter(Model != "")

boxplot(SSmetrics%>%filter(Variables == "Selected" & Model != "") %>% select(Model_Size),
        main = "Size of the selected model",
        col = "aliceblue")


ggplot(SSmetrics %>% filter(Model != ""), aes(x = Variables, y = Model_Size, fill = Variables)) +
  geom_boxplot()+
  labs(title = "Model Size of selected models", x = "Variables", y = "Model Size")



ggplot(data[data$Variables == "Selected", ], aes(x = "", y = Model_Size)) +
  geom_boxplot() +
  labs(title = "Model Size of selected models", x = "Set of variabkes", y = "Model size") +
  theme_minimal()


table(SSmetrics$Variables,SSmetrics$Model_Size)
SSmetrics %>% filter(Variables=="True") %>% select(Model) %>% table 
SSmetrics %>% filter(Variables=="True" & Model == "") %>% select(File) %>% table
# 5 variables
SSmetrics %>% filter(Variables=="True" & Model == "" & Number_Variables==5) %>% select(File) %>% table
