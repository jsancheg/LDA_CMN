
# 1) Initialize paths and functions to use --------------------------------

work_path <- "E:/University of Glasgow/Literature review/R Code/Food Analysis/LDA_CMN/"
setwd(work_path)
source("utilities.R")
source("ETMeat.R")


# 4) Setting parameters ------------------------------------------------------

nSim <- 45  
peaks = c(232,330,382)
ws = c(50,50,50)
shape = c(4,4,4)
c = c(0.002,0.01,0.005)
models <- c("EII","VII","EEI")
classes <- c("A","B")
s1 <- sPork


# 5) Simulating class A ---------------------------------------------------
sample4.1 <- gen(nSim, XbarPork, s1 )
sample4.1df <- SampleToDf(sample4.1,"A")


# 6) Simulating a no overlapping class ----------------------------------
# Simulate class B by modifying mean of class A using
#  window size (ws) = 30, c = 0.009, shape = 4

mu4.4 <- modMean(XbarPork,330,ws = 30,c = 0.009,shape = 4)
sample4.4 <- gen(nSim, mu4.4[[1]], s1)
sample4.4df <- SampleToDf(sample4.4,"B")
samples4.4 <- rbind(sample4.1df,sample4.4df)

ymin <- min(XbarPork, mu4.4[[1]])
ymax <- max(XbarPork, mu4.4[[1]])

plot(Wavelength, XbarPork, type = "l", col = "black",
     ylim = c(ymin,ymax))
lines(Wavelength, mu4.4[[1]], col = "blue")


# Plot class  A and B
ymin <- min(sample4.1,sample4.4)
ymax <- max(sample4.1,sample4.4)

plot(Wavelength, y = sample4.1df[1,-c(1:2)], ylim = c(ymin,ymax), 
     type = "l", col = "orange",
     main = "Class A and Class B", ylab = "Itensity")
for (i in 2:40)
  lines(Wavelength, y = sample4.1df[i,-c(1:2)], col = "orange")
for (i in 1:40)
  lines(Wavelength, y = sample4.4df[i,-c(1:2)],  type = "l", col = "green")
legend("topleft", title = "Classes",legend = c("A","B"), 
       col = c("orange","green"), lty = c(1,1), cex = 1 )


n <- nrow(samples4.4)
p <- ncol(samples4.4)
set.seed(123)
ind.test1 <- sample(1:n,round(n/3))


test.data2 <- samples4.4[ind.test1,-2]
test.label2 <- samples4.4[ind.test1,]$Class

train.data2 <- samples4.4[-ind.test1,-2]
train.label2 <- samples4.4[-ind.test1,]$Class

corr.class.rate2 <- matrix(0,nrow = 1, ncol = 2)
error.rate2 <- matrix(0, nrow = 1, ncol = 2)
colnames(corr.class.rate2) <- c("train", "test")
colnames(error.rate2) <- c("train", "test")



lda.fit2 <- lda(Class ~., data = train.data2)
pred.train2 <- predict(lda.fit2,train.data2)
pred.class2 <- predict(lda.fit2,test.data2)
corr.class.rate2[1,1] <- sum(diag(table(train.label2, levels(train.label2)[max.col(pred.train2$posterior)])))/ (length(train.label2))
corr.class.rate2[1,2] <- sum(diag(table(test.label2, levels(test.label2)[max.col(pred.class2$posterior)])))/ (length(test.label2))
error.rate2[1,1] <- 1 - corr.class.rate2[1,1]
error.rate2[1,2] <- 1- corr.class.rate2[1,2]

# Correct classification rate
corr.class.rate2
error.rate2

ldahist(data = pred.class2$x[,1], g=train.data2$Class)
pred.train2$x
aux <-rep(0,length(pred.train2$x))

dataset <- data.frame(Class=train.data2$Class, lda=pred.train2$x )
ggplot(dataset, aes(x=LD1)) + 
  geom_density(aes(group=Class, colour=Class, fill=Class), alpha=0.3)




# 7) Simulating an Overlapping Class B -----------------------------------------


mu4.6 <- modMean(XbarPork,330,ws = 30,c = 0.009,shape = 5)
sample4.6 <- gen(nSim, mu4.6[[1]], s1)
sample4.6df <- SampleToDf(sample4.6,"B")
samples4.6 <- rbind(sample4.1df,sample4.6df)

ymin <- min(XbarPork, mu4.6[[1]])
ymax <- max(XbarPork, mu4.6[[1]])

plot(Wavelength, XbarPork, type = "l", col = "orange",
     ylim = c(ymin,ymax))
lines(Wavelength, mu4.6[[1]], col = "green")


# Plot class  A and B
ymin <- min(sample4.1,sample4.6)
ymax <- max(sample4.1,sample4.6)

plot(Wavelength, y = sample4.1df[1,-c(1:2)], ylim = c(ymin,ymax), 
     type = "l", col = "orange",
     main = "Class A and Class B", ylab = "Itensity")
for (i in 2:40)
  lines(Wavelength, y = sample4.1df[i,-c(1:2)], col = "orange")
for (i in 1:40)
  lines(Wavelength, y = sample4.6df[i,-c(1:2)],  type = "l", col = "green")
legend("topleft", title = "Classes",legend = c("A","B"), 
       col = c("orange","green"), lty = c(1,1), cex = 1 )


n <- nrow(samples4.6)
p <- ncol(samples4.6)
set.seed(123)
ind.test6 <- sample(1:n,round(n/3))


test.data6 <- samples4.6[ind.test6,-2]
test.label6 <- samples4.6[ind.test6,]$Class

train.data6 <- samples4.6[-ind.test6,-2]
train.label6 <- samples4.6[-ind.test6,]$Class

corr.class.rate6 <- matrix(0,nrow = 1, ncol = 2)
error.rate6 <- matrix(0, nrow = 1, ncol = 2)
colnames(corr.class.rate6) <- c("train", "test")
colnames(error.rate6) <- c("train", "test")



lda.fit6 <- lda(Class ~., data = train.data6)
pred.train6 <- predict(lda.fit6,train.data6)
pred.class6 <- predict(lda.fit6,test.data6)
corr.class.rate6[1,1] <- sum(diag(table(train.label6, levels(train.label6)[max.col(pred.train6$posterior)])))/ (length(train.label6))
corr.class.rate6[1,2] <- sum(diag(table(test.label6, levels(test.label6)[max.col(pred.class6$posterior)])))/ (length(test.label6))
error.rate6[1,1] <- 1 - corr.class.rate6[1,1]
error.rate6[1,2] <- 1- corr.class.rate6[1,2]

# Correct classification rate
corr.class.rate6
error.rate6

ldahist(data = pred.class6$x[,1], g=train.data6$Class)
pred.train6$x
aux <-rep(0,length(pred.train6$x))

dataset6 <- data.frame(Class=train.data6$Class, lda=pred.train6$x )
ggplot(dataset6, aes(x=LD1)) + 
  geom_density(aes(group=Class, colour=Class, fill=Class), alpha=0.3)

