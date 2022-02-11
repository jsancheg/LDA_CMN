
# ET Extraction and transformation of Meat data set

dfMeat <- dfMeat %>% mutate( Sample = as.factor(Sample))

dfMeat1 <- dfMeat %>% mutate( Wavelength = paste0("X",Wavelength))
head(dfMeat1)

dfMeat1 <- dfMeat1 %>% select(Wavelength, Class, Sample, Intensitive)
dfMeat1 <- dfMeat1 %>% mutate(Wavelength1 = Wavelength) 
dfMeat1 <-dfMeat1 %>% relocate(Wavelength1, .after = Wavelength)
dfMeat1  <- dfMeat1 %>% mutate(Wavelength = str_extract(Wavelength1,"\\d+"))
dfMeat1$Wavelength <- as.numeric(dfMeat1$Wavelength)

dfMeatWide <- dfMeat1 %>% select(-Wavelength) %>% 
  pivot_wider(names_from = Wavelength1, values_from = Intensitive)
apply(dfMeatWide,2,function(x) any(is.na(x)))
dfMeatWide[1:2,1:8]

dfMeatWide$Class <- factor(dfMeatWide$Class)

dfChickenWide <- dfMeatWide %>% filter(Class == "Chicken")
dfPorkWide <- dfMeatWide %>% filter (Class == "Pork")
dfTurkeyWide <- dfMeatWide %>% filter(Class == "Turkey")

XPork <- as.matrix(dfPorkWide[,-c(1,2)])
XbarPork <- apply(XPork,2,mean)
sPork <- apply(XPork,2,sd)

XChicken <- as.matrix(dfChickenWide[,-c(1,2)])
XbarChicken <- apply(XChicken,2,mean)
sChicken <- apply(XChicken,2,sd)

XTurkey <- as.matrix(dfTurkeyWide[,-c(1,2)])
XbarTurkey <- apply(XTurkey,2,mean)
sTurkey <- apply(XTurkey,2,sd)

xbar <- apply(dfMeatWide[,-c(1:2)],2,mean)

n <- nrow(dfMeatWide)
p <- ncol(dfMeatWide) - 2

Wavelength <- as.numeric(dfMeatWide %>% select_if(is.numeric) %>% colnames() %>% str_extract("\\d+"))
