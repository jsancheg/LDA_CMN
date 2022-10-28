### Power and Weight
setwd("~/git_environment/RegresionLab")
#Load the data
phys <- read.csv("phys1.csv")
#Check column names
names(phys)
#View data
View(phys)

#Plot power1 against weight
plot(Power1 ~ Weight, data=phys)
#Add labels
plot(Power1 ~ Weight, data=phys, xlab="Weight (kgs)", ylab="Power Output (Watts)")
#Change size
plot(Power1 ~ Weight, data=phys, xlab="Weight (kgs)", ylab="Power Output (Watts)", cex=1.5)
#Change symbol
plot(Power1 ~ Weight, data=phys, xlab="Weight (kgs)", ylab="Power Output (Watts)", cex=1.5, pch=19)
#Change colour
plot(Power1 ~ Weight, data=phys, xlab="Weight (kgs)", ylab="Power Output (Watts)", cex=1.5, pch=19,col="blue")

#Use ggplot to plot
library(ggplot2) #you need to install this package if it does not load
#install.packages("ggplot2")
ggplot(data=phys, aes(y=Power1,x= Weight))+ #Specify x and y axes
  geom_point(size = 3, color="blue") + #Specify to plot points size 2 coloured blue
  labs(x="Weight (kgs)", y="Power Output (Watts)")+ #Specify axes labels
  theme(text = element_text(size=20)) #Change size of axes labels

#Specify points to be coloured by gender.
ggplot(data=phys, aes(y=Power1,x= Weight,color=Gender)) + 
  geom_point(size = 3) + 
  labs(x="Weight (kgs)", y="Power Output (Watts)") + 
  theme(text = element_text(size=20)) 


#Estimate correlation between power1 and weight
cor(phys$Power1, phys$Weight)
#Test the null hypothesis that the population correlation coefficient is equal to zero
cor.test(phys$Power1, phys$Weight)

#Fit a simple linear regression
model1<-lm(Power1 ~ Weight, data=phys)
summary(model1)

#Fit a multiple linear regression adding gender
model2<-lm(Power1 ~ Weight + Gender, data=phys)
summary(model2)
#E(Power|Weight,Male)=13.391+249.708+14.995Weight
#E(Power|Weight,Female)=13.391+14.995Weight

#Try now including an interaction between weight and gender
model3<-lm(Power1 ~ Weight * Gender, data=phys)
summary(model3)
#Notice in this model the additional term 'Weight:GenderMale' this value, in addition to 'Weight' gives the slope estimate for males.
#E(Power|Weight,Male)=401.931-416.113+8.102Weight+10.751Weight
#E(Power|Weight,Female)=401.931+8.102Weight

#Make plot with these two separate fitted lines included from model3
newdata<-data.frame(Weight=rep(seq(40,90,length=50),2), Gender=rep(c("Male","Female"),rep(50,2)) )
pred3<-predict(model3, newdata)
newdata1<-data.frame(newdata, pred=pred3)
#Specify points to be coloured by gender.
ggplot(data=phys, aes(y=Power1,x= Weight,color=Gender)) + 
  geom_point(size = 3) + 
  labs(x="Weight (kgs)", y="Power Output (Watts)") + 
  theme(text = element_text(size=20)) +
  geom_line(aes(y=pred,x= Weight,color=Gender), data=newdata1,size=2)

#Make plot with these two separate fitted lines included from model2
newdata<-data.frame(Weight=rep(seq(40,90,length=50),2), Gender=rep(c("Male","Female"),rep(50,2)) )
pred2<-predict(model2, newdata)
newdata1<-data.frame(newdata, pred=pred2)
#Specify points to be coloured by gender.
ggplot(data=phys, aes(y=Power1,x= Weight,color=Gender)) + 
  geom_point(size = 3) + 
  labs(x="Weight (kgs)", y="Power Output (Watts)") + 
  theme(text = element_text(size=20)) +
  geom_line(aes(y=pred,x= Weight,color=Gender), data=newdata1,size=2)

#Based on these two plots, we can see that the intercept terms are different for males and females because males lie
#'above' females, that is the green cloud of points lie aboive the red cloud of points.
#Both lines have fairly similar positive slope parameters and so it may not be neccessary to have differet slope parameters.


### Hubble's Constant

#Load data
hubble <- read.csv("hubble.csv")
#Check column names
names(hubble)
#View data
View(hubble)

#Plot velocity against disance
plot(Velocity~Distance, data=hubble, cex=1.5, col="blue", pch=19, cex.lab=1.5,
     xlab="Distance (megaparsecs)", ylab="Velocity (km/sec)")
#ggplot
ggplot(data=hubble, aes(y=Velocity,x=Distance))+ 
  geom_point(size = 3, color="blue") + 
  labs(x="Distance (megaparsecs)", y="Velocity (km/sec)")+ 
  theme(text = element_text(size=20))
#We can see as distance increases, so does velocity and the relationship looks linear.  There seems to be a strong positive correlation.

#Fit a simple linear regression
model <- lm(Velocity ~ Distance, data=hubble)
summary(model)
#E(Velocity|Distance)=-40.78 + 454.16Distance

#Add fitted line to plot
plot(Velocity~Distance, data=hubble, cex=1.5, col="blue", pch=19, cex.lab=1.5,
     xlab="Distance (megaparsecs)", ylab="Velocity (km/sec)")
abline(model,lwd=2) 
#With ggplot
ggplot(data=hubble, aes(y=Velocity,x=Distance))+ 
  geom_point(size = 3, color="blue") + 
  labs(x="Distance (megaparsecs)", y="Velocity (km/sec)")+ 
  theme(text = element_text(size=20))+
geom_smooth(method="lm", fill=NA, fullrange=TRUE,color="black")

#Fit the same model with no intercept term
model2 <- lm(Velocity ~ Distance - 1, data=hubble)
summary(model2)
#E(Velocity|Distance)=423.94Distance

#Add fitted line
plot(Velocity~Distance, data=hubble, cex=1.5, col="blue", pch=19, cex.lab=1.5,
     xlab="Distance (megaparsecs)", ylab="Velocity (km/sec)")
abline(model,lwd=2) 
abline(model2,lwd=2,lty=2)
#With ggplot
ggplot(data=hubble, aes(y=Velocity,x=Distance))+ 
  geom_point(size = 3, color="blue") + 
  labs(x="Distance (megaparsecs)", y="Velocity (km/sec)")+ 
  theme(text = element_text(size=20))+
  geom_smooth(method="lm", fill=NA, fullrange=TRUE,color="black") +
  geom_smooth(method="lm", fill=NA, fullrange=TRUE,color="red",formula=y~x-1)

#There is very little difference between the two fitted lines.  In general, go for the 'simplier' of the two models.
#that is the model with the least parameters to estimate. 


### Publishing History

#Load data
books <- read.csv("books.csv")
#Check column names
names(books)
#View data
View(books)

#Plot number of books published over time
plot(Number.of.Books~Year, data=books, cex=1.5, col="blue", pch=19, cex.lab=1.5,
     xlab="Year", ylab="Number of Books Published")
#ggplot
ggplot(data=books, aes(y=Number.of.Books,x=Year))+ 
  geom_point(size = 3, color="blue") + 
  labs(x="Year", y="Number of Books Published")+ 
  theme(text = element_text(size=20))
#We can see the number of book published increase over time. The relationship does not look linear.

#Fit a simple linear regression
model0 <- lm(Number.of.Books~Year, data=books)
summary(model0)
#E(Number.of.Books|Year)=-50.1011 + 4.2599Year

#Add a quadratic term
model1 <- lm(Number.of.Books~poly(Year,2), data=books)
summary(model1)
model1 <- lm(Number.of.Books~Year+I(Year^2), data=books)
summary(model1)
model2 <- lm(Number.of.Books~poly(Year,2,raw=TRUE), data=books)
summary(model2)

#E(Number.of.Books|Year)=248.092 + 2058.861Year + 597.132Year^2

#Add fitted line to plot
plot(Number.of.Books~Year, data=books, cex=1.5, col="blue", pch=19, cex.lab=1.5,
     xlab="Year", ylab="Number of Books Published")
abline(model0,lwd=2,lty=2)
abline(model1,lwd=2,lty=1)
#Add line from quadratic model
newdata<-data.frame(Year=0:145)
pred<-predict(model1, newdata)
newdata1<-data.frame(newdata, pred=pred)
lines(newdata1$Year,newdata1$pred,lwd=2)
#ggplot
ggplot(data=books, aes(y=Number.of.Books,x=Year))+ 
  geom_point(size = 3, color="blue") + 
  labs(x="Year", y="Number of Books Published")+ 
  theme(text = element_text(size=20))+
  geom_smooth(method="lm", fill=NA, fullrange=TRUE,color="black") +
  geom_line(aes(y=pred,x= Year), data=newdata1,size=2)

#The quadratic model seems to fit the data more closely to the simple linear regression.




### The Taste of Cheese

#Load data
cheese <- read.csv("cheese.csv")
#Check column names
names(cheese)
#View data
View(cheese)

attach(cheese)
#Plot taste against Acetic Acid
plot(Acetic.Acid,Taste, cex=1.5, col="blue", pch=19, cex.lab=1.5,
     xlab="Acetic Acid", ylab="Taste")
#ggplot
ggplot(data=cheese, aes(y=Taste,x=Acetic.Acid))+ 
  geom_point(size = 3, color="blue") + 
  labs(x="Taste", y="Acetic Acid")+ 
  theme(text = element_text(size=20))
#There appears to be a weak postive correlation between taste and acetic acid.

#Plot taste against H2S
plot(H2S, Taste, cex=1.5, col="blue", pch=19, cex.lab=1.5,
     xlab="H2S", ylab="Taste")
#ggplot
ggplot(data=cheese, aes(y=Taste,x=H2S))+ 
  geom_point(size = 3, color="blue") + 
  labs(x="Taste", y="H2S")+ 
  theme(text = element_text(size=20))
#There does not seem to be a linear relationship between taste and H2S

#Plot taste against Lactic Acid
plot(Lactic.Acid, Taste, cex=1.5, col="blue", pch=19, cex.lab=1.5,
     xlab="Lactic Acid", ylab="Taste")
#ggplot
ggplot(data=cheese, aes(y=Taste,x=Lactic.Acid))+ 
  geom_point(size = 3, color="blue") + 
  labs(x="Taste", y="Lactic Acid")+ 
  theme(text = element_text(size=20))
#There appears to be a moderately strong postive correlation between taste and Lactic Acid.
#I will choose Lactic Acid

#Fit a simple linear regression
model <- lm(Taste~Lactic.Acid, data=cheese)
summary(model)
#E(Taste|Lactic Acid)=-29.859 + 37.720Lactic.Acid

#Plot taste against Lactic Acid with fitted line
plot(Lactic.Acid, Taste, cex=1.5, col="blue", pch=19, cex.lab=1.5,
     xlab="Lactic Acid", ylab="Taste") 
abline(model,lwd=2)
#ggplot
ggplot(data=cheese, aes(y=Taste,x=Lactic.Acid))+ 
  geom_point(size = 3, color="blue") + 
  labs(x="Taste", y="Lactic Acid")+ 
  theme(text = element_text(size=20)) +
  geom_smooth(method="lm", fill=NA, fullrange=TRUE,color="black")

#Correlation between taste and lactic acid
cor.test(cheese$Taste,cheese$Lactic.Acid)
#Since the p-value < 0.05, we can reject the null hypothesis that the population correlation coefficient is zero.
#We have evidence in favour or a strong postive correlation, estimated as 0.704, between taste and lactic acid.


### Ages

##Load data
data <- read.csv("AGESf.csv")

##First need to format the data
Age<-rep(data$Age,3)
Guess<-c(data[,2],data[,3],data[,4])
Class<-as.factor(rep(19:21,rep(13,3)))
df<-data.frame(Age=Age,Guess=Guess,Class=Class)
head(df)

### Plot these data 
ggplot(data=df, aes(x=Guess,y=Age, colour =Class, shape=Class))+ 
  geom_point(size = 5, alpha = 0.75)+
  theme(plot.margin = margin(0,0,0,0, "cm"),
        plot.background = element_rect(
          fill = "transparent",
          colour = NA,
          size = 1))

### Based on this fit, it seems that a parallel regression model may be appropriate.

### Fit additive model with Guess and Class
model2<-lm(Age~Guess+Class,data=df)
round(summary(model2)$coefficients,2)

###From this fit
#E(Age|2019)=7.09+1.12Guess
#E(Age|2020)=7.09+1.12Guess-19.67
#E(Age|2021)=7.09+1.12Guess+0.47


### Plot these data 
ggplot(data=df, aes(x=Guess,y=Age, colour =Class, shape=Class))+ 
  geom_point(size = 5, alpha = 0.75) +
  theme(plot.margin = margin(0,0,0,0, "cm"),
        plot.background = 
          element_rect(fill = "transparent",colour = NA,size = 1)) +
  geom_abline(intercept=7.09, slope=1.12, color="red") +
  geom_abline(intercept=7.0-19.679, slope=1.12, color="green") +
  geom_abline(intercept=7.09+0.47, slope=1.12, color="blue")


###Repeat the above steps with an interaction between Class and Guess
model3<-lm(Age~Guess*Class,data=df)
round(summary(model3)$coefficients,2)

###From this fit
#E(Age|2019)=6.08+1.14Guess
#E(Age|2020)=6.08+1.14Guess-16.67-0.06Guess
#E(Age|2021)=6.08+1.14Guess+1.15-0.02Guess


### Plot these data 
ggplot(data=df, aes(x=Guess,y=Age, colour =Class, shape=Class))+ 
  geom_point(size = 5, alpha = 0.75) +
  theme(plot.margin = margin(0,0,0,0, "cm"),
        plot.background = 
          element_rect(fill = "transparent",colour = NA,size = 1)) +
  geom_abline(intercept=6.08, slope=1.14, color="red") +
  geom_abline(intercept=6.08-16.67, slope=1.14-0.06, color="green") +
  geom_abline(intercept=6.08+1.15, slope=1.14-0.02, color="blue")


### Based on the fitted line plots, which model do you prefer?

