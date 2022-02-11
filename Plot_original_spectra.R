# File:  Plot_original_spectra.R
# Author: Jorge Sanchez
# Date:   07/02/2022
# Purpose: Plot original spectra of samples of three type of meat 


work_path <- "E:/University of Glasgow/Literature review/R Code/Food Analysis/LDA_CMN/"
setwd(work_path)
source("utilities.R")
#source("E:/University of Glasgow/Literature review/R Code/Punzo/ContaminatedMixt-master/R/main.R")



# 2) Extract, transform, and load meat dataset ----------------------------

source("ETMeat.R")


# 3) Plot original spectra ------------------------------------------------
X <- dfMeatWide %>% select_if(is.numeric)
ymin <- min(X)
ymax <- max(X)

nChicken <- nrow(dfChickenWide)
nPork <- nrow(dfPorkWide)
nTurkey <- nrow(dfTurkeyWide)

dfMeatWide[1:2,1:4]
Wavelength

# plot Pork
plot(Wavelength, y = XChicken[1,], ylim = c(min = ymin, max= ymax), 
     type = "l", col = "black", ylab = "Intensity")
for(i in 2:nChicken)
  lines(Wavelength, y = XChicken[i,], col = "black")
for(i in 1:nPork)
  lines(Wavelength, y = XPork[i,], col = "blue")
for(i in 1:nTurkey)
  lines(Wavelength, y = XTurkey[i,], col = "orange")
legend("topleft", legend = c("Chicken","Pork","Turkey"),
       lty = c(1,1,1), col = c("black","blue","orange")
       ,cex = 0.7)
