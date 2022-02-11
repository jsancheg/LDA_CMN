
source("utilities.r")


# Coffee dataset ----------------------------------------------------------


dfRawCoffee <- read.csv(paste0(rutaRaw,"FTIR_Spectra_instant_coffee.csv"),skip = 2)
head(dfRawCoffee)

aux1Coffee <- gather(dfRawCoffee,key = "Class1", value = "Intensitive",-Wavenumbers)

aux1Coffee <- aux1Coffee %>% mutate(Wavelength = floor(Wavenumbers)) %>%
  relocate(Wavelength, .before = Wavenumbers)

aux1Coffee <- aux1Coffee %>% 
  mutate(Sample  = as.numeric(str_replace_na(as.numeric(unlist(str_split_fixed(aux1Coffee$Class1,"\\.",2))[,2]),0))  ) %>%
  relocate(Class1, .before = Wavenumbers) 


head(aux1Coffee)

aux2Coffee <- aux1Coffee %>% mutate(Class = str_replace_all(Class1, "([1234567890.])","")) %>% 
  relocate(Class, .before = Class1)

aux2Coffee$Class <- factor(aux2Coffee$Class)

aux2Coffee <- aux2Coffee %>% mutate(Class1 = NULL)
aux2Coffee <- aux2Coffee %>% mutate(Sample = as.factor(as.numeric(Sample) + 1))

#ggplot(aux2Coffee, aes(x = Wavenumbers, y = Intensitive, color = Class)) +
#  geom_line(aes(group = Sample)) +
#  theme(legend.position = "bottom") + 
#  theme_minimal()
  
#qplot(x = Wavenumbers, y = Intensitive, data = aux2Coffee, color = Class, geom = "line")
write.csv(aux2Coffee, paste0(pathProcDf,"FTIR_Spectra_instant_coffee_processed.csv") )


# Meat dataset ------------------------------------------------------------
# Chicken and Pork has 4 suppliers
# Turkey has only 1 supplier and all samples belong to supplier E

print(rutaRaw)
dfRawMeat <- read.csv(paste0(rutaRaw,"MIRFreshMeats.csv"))
head(dfRawMeat)

aux1Meat <- gather(dfRawMeat,key = "Class1", value = "Intensitive",-Wavenumbers)
aux1Meat$Class1 <- ifelse(aux1Meat$Class1 == "FreshPork_SupplierD_Sample1_RunB","FreshPork_SupplierD_Sample1_RunA",aux1Meat$Class1)
aux1Meat$Class1 <- ifelse(aux1Meat$Class1 == "FreshPork_SupplierD_Sample1_RunB.1","FreshPork_SupplierD_Sample1_RunB",aux1Meat$Class1)

aux2Meat <- aux1Meat %>% separate(Class1, c("Class","Supplier","Sample","Run"),"_")
aux2Meat <- aux2Meat %>% mutate(Sample = str_c(Supplier,Sample,Run,sep = "_"))


aux2Meat <- aux2Meat %>% mutate(across(where(is.character), as.factor))


aux2Meat <- aux2Meat %>% mutate(Wavelength = floor(Wavenumbers)) %>%
  relocate(Wavelength, .before = Wavenumbers)


#-------------
aux2Meat <- aux2Meat %>% mutate(Class = str_replace(Class,"Fresh",""))


summariseMeat <- aux2Meat %>% select("Class", "Sample") %>% unique
summariseMeat <- summariseMeat %>% group_by(Class)%>% mutate(Sample1 = row_number())
summariseMeat


aux2Meat <- aux2Meat %>% inner_join( summariseMeat,by=c("Class","Sample") )

aux2Meat <- rename(aux2Meat, SampleRef = Sample)
aux2Meat <- rename(aux2Meat, Sample = Sample1)
aux2Meat <- aux2Meat %>% relocate(Sample, .after = Run)

aux2Meat <- aux2Meat %>% mutate( Sample = as.factor(Sample))


aux2Meat


head(aux2Meat)

ggplot(aux2Meat, aes(x = Wavenumbers, y = Intensitive, color = Class)) +
  geom_line(aes(group = Sample)) +
  theme(legend.position = "bottom") + 
  theme_minimal()

#qplot(x = Wavenumbers, y = Intensitive, data = aux2Coffee, color = Class, geom = "line")
write.csv(aux2Meat, paste0(pathProcDf,"MIRFreshMeats_processed.csv") )


# Olive dataset -----------------------------------------------------------

dir(rutaRaw)
dfRawOlive <- read.csv(paste0(rutaRaw,"FTIR_Spectra_olive_oils.csv"),skip=2)
head(dfRawOlive)



aux1Olive <- gather(dfRawOlive,key = "Class1", value = "Intensitive",-Wavenumbers)

aux1Olive <- aux1Olive %>% mutate(Wavelength = floor(Wavenumbers)) %>%
  relocate(Wavelength, .before = Wavenumbers)

aux1Coffee <- aux1Coffee %>% 
  mutate(Sample  = as.numeric(str_replace_na(as.numeric(unlist(str_split_fixed(aux1Coffee$Class1,"\\.",2))[,2]),0))  ) %>%
  relocate(Class1, .before = Wavenumbers) 


head(aux1Coffee)

aux2Coffee <- aux1Coffee %>% mutate(Class = str_replace_all(Class1, "([1234567890.])","")) %>% 
  relocate(Class, .before = Class1)

aux2Coffee$Class <- factor(aux2Coffee$Class)

aux2Coffee <- aux2Coffee %>% mutate(Class1 = NULL)
aux2Coffee <- aux2Coffee %>% mutate(Sample = as.factor(as.numeric(Sample) + 1))

#ggplot(aux2Coffee, aes(x = Wavenumbers, y = Intensitive, color = Class)) +
#  geom_line(aes(group = Sample)) +
#  theme(legend.position = "bottom") + 
#  theme_minimal()

#qplot(x = Wavenumbers, y = Intensitive, data = aux2Coffee, color = Class, geom = "line")
write.csv(aux2Coffee, paste0(pathProcDf,"FTIR_Spectra_instant_coffee_processed.csv") )

