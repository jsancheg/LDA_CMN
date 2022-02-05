

work_path <- "E:/University of Glasgow/Literature review/R Code/"
setwd(work_path)
source("utilities.R")
source("E:/University of Glasgow/Literature review/R Code/Punzo/ContaminatedMixt-master/R/main.R")


# ETD step ----------------------------------------------------------------
source(paste0(work_path,"Food Analysis/ETMeat.R"))
constant <- 0.0026


# 1- Visualize spectra ----------------------------------------------------
x <- unique(dfMeat1$Wavelength)
ymin <- min(dfMeatWide[,-c(1:3)])
ymax <- max(dfMeatWide[,-c(1:3)])

dfMeatWide[1:2,1:4]


# plot Pork
plot(x, y = dfPorkWide[1,-c(1:2)], ylim = c(ymin,ymax), type = "l", col = "blue",
     main = "Pork spectra")
for(i in 2:nrow(dfPorkWide))
  lines(x, y = dfPorkWide[i,-c(1:2)], ylim = c(ymin,ymax), type = "l", col = "blue")



# 3 Simulation using corrected variance and comparison with real spectra --------
# simulation using the corrected estimated variance covariance matrix
# and the Chicken mean spectra
nSim <- 40
peaks = c(232,330,382)
ws = c(100,50,50)
shape = c(4,4,4)
c = c(0.002,0.01,0.005)
models <- c("EII","VII")
classes <- c("A","B")

s1 <- sPork

sample4.1 <- gen(nSim, XbarPork, s1 )
sample4.1df <- SampleToDf(sample4.1,"A")
mu4.2 <- modMean(XbarPork,330,ws = 30,c = 0.009,shape = 4)
sample4.2 <- gen(nSim, mu4.2[[1]], s1 + constant)
sample4.2df <- SampleToDf(sample4.2,"B")
samples4.2 <- rbind(sample4.1df,sample4.2df)


ymin <- min(sample4.1,sample4.2)
ymax <- max(sample4.1,sample4.2)

plot(x, y = sample4.1df[1,-c(1:2)], ylim = c(ymin,ymax), type = "l", col = "orange",
     main = "Class A and Class B", ylab = "Itensity")
for (i in 2:40)
  lines(x, y = sample4.1df[i,-c(1:2)], ylim = c(ymin,ymax), type = "l", col = "orange")
for (i in 1:40)
  lines(x, y = sample4.2df[i,-c(1:2)], ylim = c(ymin,ymax), type = "l", col = "green")
legend("topleft", title = "Classes",legend = c("A","B"), 
       col = c("orange","green"), lty = c(1,1), cex = 1 )



# LDA ---------------------------------------------------------------------

n <- nrow(samples4.2)
p <- ncol(samples4.2)

ind.train <- sample(1:n,round(n/3))
test.data <- samples4.2[ind.train,-2]
test.label <- samples4.2[ind.train,]$Class

train.data <- samples4.2[-ind.train,-2]
train.label <- samples4.2[-ind.train,]$Class

corr.class.rate <- matrix(0,nrow = 1, ncol = 2)
colnames(corr.class.rate) <- c("train", "test")


lda.fit <- lda(Class ~., data = train.data)
pred.train <- predict(lda.fit,train.data)
pred.class <- predict(lda.fit,test.data)
corr.class.rate[1,1] <- sum(diag(table(train.label, levels(train.label)[max.col(pred.train$posterior)])))/ (length(train.label))
corr.class.rate[1,2] <- sum(diag(table(test.label, levels(test.label)[max.col(pred.class$posterior)])))/ (length(test.label))


corr.class.rate

ldahist(data = pred.class$x[,1], g=train.data$Class)
pred.train$x
aux <-rep(0,length(pred.train$x))

train.data %*% pred.train$x
dataset <- data.frame(Type=train.data$Class, lda=pred.train$x )
ggplot(dataset, aes(x=lda)) + 
  geom_density(aes(group=Type, colour=Type, fill=Type), alpha=0.3)

#visualise how LD1 and LD2 together separate the three classes
plot(aux,pred.train$x, col=train.data$Class,
     pch=as.numeric(train.data$Species), xlab="aux", ylab="LD1")
legend("topleft", title = "class", legend = c("A","B"), 
       col = c("black","red"), lty = c(1,1),cex = 1.2)

#create a classification rule based on LD1
pred.LD1 <-rep("B",nrow(train.data))
pred.LD1[pred.train$x < -5] <- "A"
sum(pred.LD1!=train.data$Class)

plot(x = Wavelength, y = lda.fit$scaling, type = "l")

temp <- data.frame(Wavelengths = rownames(lda.fit$scaling), 
                   Weights = abs(lda.fit$scaling) ) 
head(temp)
class(temp)

Wavelengths.rank <-  temp[order(-temp$LD1),]

head(Wavelengths.rank,50)
# CMN ---------------------------------------------------------------------

lim <- 10
selected <- Wavelengths.rank$Wavelengths[1:lim]
selected

vars <- c(1,sapply(selected,match, table = colnames(train.data) ) )

train.data <- samples4.2[-ind.train,vars]
test.data <- samples4.2[ind.train,vars]

trainl<- samples4.2[-ind.train,]$Class
testl<- samples4.2[ind.train,]$Class


test.data
class(train.data)
class(trainl)
head(train.data)
component <- 2
models 

tab <- matrix(NA, nrow = length(models), ncol = 2)

mod <- CNmixt(X = as.matrix(train.data[,-1]), G = component,
              contamination = T,
              model = "EII",
              label = as.numeric(trainl),
              initialization = "random.post",
              seed = 12, parallel = F  )      

predTrain<- CNpredict(as.matrix(train.data[,-1]), prior= mod$models[[1]]$prior,
                      mu = mod$models[[1]]$mu,
                      invSigma = mod$models[[1]]$invSigma)

tabTrain <- table(predTrain,as.numeric(trainl))
tabTrain

tab[1,1] <- sum(predTrain != as.numeric(trainl))/length(predTrain)

predtest <- CNpredict(as.matrix(test.data[,-1]), prior = mod$models[[1]]$prior, 
                      mu = mod$models[[1]]$mu, 
                      invSigma = mod$models[[1]]$invSigma)

tabTest<- table(predtest, as.numeric(testl))
tab [1,2] <- sum(predtest != as.numeric(testl))/length(predtest)

tab

