
ruta <- "E:/University of Glasgow/Literature review/R Code/Food Analysis/LDA_CMN/LDA_CMN/"

sim1 <- readRDS(paste0(ruta,"Output/Sim_Batch1.RDS"))
sim2 <- readRDS(paste0(ruta,"Output/Sim_Batch2.RDS"))
sim3 <- readRDS(paste0(ruta,"Output/Sim_Batch3.RDS"))
sim4 <- readRDS(paste0(ruta,"Output/Sim_Batch4.RDS"))
sim5 <- readRDS(paste0(ruta,"Output/Sim_Batch5.RDS"))


CM <- c(unlist(sim1$CM),
        unlist(sim2$CM),
        unlist(sim3$CM),
        unlist(sim4$CM),
        unlist(sim5$CM))
table(CM)

Accuracy <- c(unlist(sim1$Accuracy),
              unlist(sim2$Accuracy),
              unlist(sim3$Accuracy),
              unlist(sim4$Accuracy),
              unlist(sim5$Accuracy))

length(Accuracy)
mean(Accuracy)


ModelSize <- c(unlist(sim1$ModelSize),
               unlist(sim2$ModelSize),
               unlist(sim3$ModelSize),
               unlist(sim4$ModelSize),
               unlist(sim5$ModelSize))


median(ModelSize)

length(ModelSize)
mean(ModelSize)
table(ModelSize)

InclusionCorrectness <- c(unlist(sim1$Inclusion_correctness),
                          unlist(sim2$Inclusion_correctness),
                          unlist(sim3$Inclusion_correctness),
                          unlist(sim4$Inclusion_correctness),
                          unlist(sim5$Inclusion_correctness))
mean(InclusionCorrectness)

Exclusion_correctness <- c(unlist(sim1$Exclusion_correctness),
                                unlist(sim2$Exclusion_correctness),
                                unlist(sim3$Exclusion_correctness),
                                unlist(sim4$Exclusion_correctness),
                                unlist(sim5$Exclusion_correctness))

mean(Exclusion_correctness)

SensitivityA <- c(unlist(sim1$Sensitivity_Class1),
                  unlist(sim2$Sensitivity_Class1),
                  unlist(sim3$Sensitivity_Class1),
                  unlist(sim4$Sensitivity_Class1),
                  unlist(sim5$Sensitivity_Class1))

mean(SensitivityA)

SpecificityA <- c(unlist(sim1$Specificity_Class1),
                 unlist(sim2$Specificity_Class1),
                 unlist(sim3$Specificity_Class1),
                 unlist(sim4$Specificity_Class1),
                 unlist(sim5$Specificity_Class1))
mean(SpecificityA)

SensitivityB <- c(unlist(sim1$Sensitivity_Class2),
                  unlist(sim2$Sensitivity_Class2),
                  unlist(sim3$Sensitivity_Class2),
                  unlist(sim4$Sensitivity_Class2),
                  unlist(sim5$Sensitivity_Class2))

mean(SensitivityB)

SpecificityB <- c(unlist(sim1$Specificity_Class2),
                  unlist(sim2$Specificity_Class2),
                  unlist(sim3$Specificity_Class2),
                  unlist(sim4$Specificity_Class2),
                  unlist(sim5$Specificity_Class2))
mean(SpecificityB)
