pathScenarios <- "/home/jsancheg/Documents/Scenarios/"
pathSSFiles <- "/home/jsancheg/Documents/SSFiles/"
Scenarios <- dir(pathScenarios)
Scenarios
length(Scenarios)
ind5vars <- sapply(Scenarios , function(x)
  {
    aux <- str_split_1(x,"_")
    ind5 <- as.numeric(aux[4]) == 5
    return(ind5)
})

ind100vars <- !ind5vars


Scenarios5 <- Scenarios[ind5vars]
Scenarios100 <- Scenarios[ind100vars]
  

n5 <- length(Scenarios5)
n100 <- length(Scenarios100)

n5.1 <- floor(n5*1/10)
n5.2 <- floor(n5*2/10)
n5.3 <- floor(n5*3/10)
n5.4 <- floor(n5*4/10)
n5.5 <- floor(n5*5/10)
n5.6 <- floor(n5*6/10)
n5.7 <- floor(n5*7/10)
n5.8 <- floor(n5*8/10)
n5.9 <- floor(n5*9/10)

n100.1 <- floor(n100*1/10)
n100.2 <- floor(n100*2/10)
n100.3 <- floor(n100*3/10)
n100.4 <- floor(n100*4/10)
n100.5 <- floor(n100*5/10)
n100.6 <- floor(n100*6/10)
n100.7 <- floor(n100*7/10)
n100.8 <- floor(n100*8/10)
n100.9 <- floor(n100*9/10)



