# File:  fit_original.R
# Author: Jorge Sanchez
# Date:   07/02/2022
# Purpose: Fit LDA, to the original 
#          spectra data set containing samples of three classes 



# 1) Plot dataset ---------------------------------------------------------

source("Plot_original_spectra")



# 2) Sample training set --------------------------------------------------
n <- nrow(X)

set.seed(123)
indtrain <- sample(1:n, round(2*n/3), replace = F)

train.set <- dfMeatWide[indtrain,-2]
train.class <- dfMeatWide[indtrain,]$Class
test.set <- X[-indtrain,]
test.class <- dfMeatWide[-indtrain,]$Class


# 3) Fit LDA model --------------------------------------------------------
ccrate <- matrix(0, nrow = 1, ncol = 2) # correct classification rate
errate <- matrix(0,nrow= 1, ncol = 2)


LDA_original_spectra <- lda(Class ~ . ,data = train.set)
LDA_original_spectra

pred.original_spectra<- predict(LDA_original_spectra,train.set)
pred.test_original_spectra<- predict(LDA_original_spectra,test.set)

# train
ccrate[1,1] <- sum(diag(table(train.class, levels(train.class) [max.col(pred.original_spectra$posterior)]  )))
ccrate[1,1] <- ccrate[1,1]/length(train.class)
errate[1,1] <- 1-ccrate[1,1]
# test
ccrate[1,2] <- sum(diag(table(test.class, levels(test.class) [max.col(pred.test_original_spectra$posterior)]  )))
ccrate[1,2] <- ccrate[1,2]/length(test.class)
errate[1,2] <- 1- ccrate[1,2]


# 4) Plots of Linear discriminant functions ----------------------------------

# histograms
ldahist(data = pred.original_spectra$x[,1], g = train.set$Class)
ldahist(data = pred.original_spectra$x[,2], g = train.set$Class)

# PLot LDF1 and LDF2
Ldf.dt <- data.frame(Class = train.set$Class, lda = pred.original_spectra$x)
ggplot(Ldf.dt, aes(x = lda.LD1)) +
  geom_density(aes(group = Class, colour = Class ,fill = Class ), alpha = 0.3)

plot(pred.original_spectra, col = train.set$Class,
     pch = as.numeric(train.set$Class))

# 5) Plot Wavelengths -----------------------------------------------------


plot(x = Wavelength, y = LDA_original_spectra$scaling[,1], type = "l", 
     col = "black", ylab = "Loadings")
lines(x = Wavelength, y = LDA_original_spectra$scaling[,2], col = "blue")

temp.Wavelength <- data.frame(Wavelengths = rownames(LDA_original_spectra$scaling),
                  Weights = apply(abs(LDA_original_spectra$scaling),1,sum))

plot(x = Wavelength , y = temp.Wavelength$Weights, type = "l",
     col = "black", ylab = "Sum of abs. loadings")

Ranked.original.Wavelengths <- temp.Wavelength[order(-temp.Wavelength$Weights),]


# prueba function fit_LDA

prueba <- fit_LDA(train.set,"Class")

cbind(User = prueba$Wavelengths.ranked[1:10,], Original = Ranked.original.Wavelengths[1:10,])


