rm(list=ls())
gc()

library(data.table)
library(ggplot2)
library(parallel)
library(randomForest)
library(corrplot)
library(FNN)
library(VIM)


## DANE
load("/Users/zuza/Desktop/studia/semestr7/MWM/Data/KrukUWr2024.RData")

## PODGLĄD DANYCH
summary(cases)
summary(events)

## DEKODOWANIE ZMIENNYCH CHARAKTER

cases[!is.na(Gender), Gender := ifelse(Gender == "FEMALE", 1, 0)]
cases[, Gender := as.integer(Gender)]
cases[, IsCashLoan := ifelse(Product== "Cash loan", 1, 0)]
cases[, Product := NULL]

## KORELACJA (przed imputacją)
corrplot(cor(cases, use = "complete.obs"))

## IMPUTACJA BRAKÓW DANYCH

### Others - ekspercko

cases[is.na(Other), .N]
cases[is.na(Other), Other := TOA - (Principal + Interest)]

### D_ContractDateToImportDate - mediana (mało braków danych)

cases[is.na(D_ContractDateToImportDate), .N]
cases[is.na(D_ContractDateToImportDate), D_ContractDateToImportDate := median(cases$D_ContractDateToImportDate, na.rm = TRUE)]

### Bailiff, ClosedExecution, ExternalAgency - z rozkładu

cases[is.na(Bailiff), .N]
cases[is.na(ClosedExecution), .N]
cases[is.na(ExternalAgency), .N]

probB <- mean(cases$Bailiff, na.rm = TRUE)
probCL <- mean(cases$ClosedExecution, na.rm = TRUE)
probEA <- mean(cases$ExternalAgency, na.rm = TRUE)

cases[is.na(Bailiff), Bailiff := sample(c(1, 0), .N, prob = c(probB, 1 - probB), replace = TRUE)]
cases[is.na(ClosedExecution) & Bailiff == 0, ClosedExecution := 0]
cases[is.na(ClosedExecution) & Bailiff == 1, ClosedExecution := sample(c(1, 0), .N ,prob = c(probCL, 1 - probCL), replace = TRUE)]
cases[is.na(ExternalAgency), ExternalAgency := sample(c(1, 0), .N ,prob = c(probEA, 1 - probEA), replace = TRUE)]

### Land -  z rozkładu

cases[is.na(Land), .N]
cases[, .N, by = Land][order(Land)]

problands_dt <- cases[, .("probL" = .N/nrow(cases)) ,by = Land][order(Land)]
cases[is.na(Land),  Land := sample(1:37, .N, prob = problands_dt$probL[-1], replace = TRUE)]

### GDPPerCapita, MeanSalary - średnią, biorąc pod uwagę Land

cases[is.na(GDPPerCapita), .N]
cases[is.na(MeanSalary), .N]

GDP_stats <- cases[, .("mean" = mean(GDPPerCapita, na.rm = TRUE),
                       "median" =median(GDPPerCapita, na.rm = TRUE), .N),
                   by = Land][order(Land)]

MeanSalary_stats <- cases[, .("mean" = mean(MeanSalary, na.rm = TRUE),
                              "median" =median(MeanSalary, na.rm = TRUE), .N),
                          by = Land][order(Land)]

for(i in 1:37){
  if(cases[Land == i & is.na(MeanSalary), .N] != 0){
    cases[Land == i & is.na(MeanSalary), MeanSalary := MeanSalary_stats[Land == i, mean]]
  }
}

for(i in 1:37){
  if(cases[Land == i & is.na(GDPPerCapita), .N] != 0){
    cases[Land == i & is.na(GDPPerCapita), GDPPerCapita := GDP_stats[Land == i, mean]]
  }
}

### Population - knn

casesNoMissings <- na.omit(cases)
variables <- setdiff(names(casesNoMissings), c("CaseId", "PopulationInCity"))

for (v in variables) {
  vnrm <- paste0(v, "_nrm")
  casesNoMissings[, eval(vnrm):=(get(v) - min(get(v)))/(max(get(v)) - min(get(v)))]
} 

variables <- paste0(variables, "_nrm")

ids <- sample(casesNoMissings$CaseId, 0.6 * nrow(casesNoMissings))

trnSet <- casesNoMissings[CaseId %in% ids, .SD, .SDcols = c(variables, "PopulationInCity")]
tstSet <- casesNoMissings[CaseId %notin% ids, .SD, .SDcols = c(variables, "PopulationInCity")]

# 150, 200, 220, 250, 400, 500, 700

# Wybór dobrych zmiennych
rndForest <- randomForest(PopulationInCity~., data=trnSet,
                          mtry=6, ntree=500, nodesize=200,
                          importance=TRUE, keep.inbag=TRUE, keep.forest=TRUE)
importance(rndForest)
varImpPlot(rndForest)


# sprawdzenie jakie k spoko
ModelsKnnPopulation <- mclapply(1:10, function(i){
  
  ids <- sample(casesNoMissings$CaseId, 0.6 * nrow(casesNoMissings))
  trnSet <- casesNoMissings[CaseId %in% ids, .SD, .SDcols = c(variables, "PopulationInCity")]
  
  rbindlist(lapply(c(5, 10, 50, 100), function(k){
    knn_mod_train <- FNN::knn.reg(
      train=trnSet[, .SD, .SDcols = setdiff(variables, "PopulationInCity")],
      test=trnSet[, .SD, .SDcols = setdiff(variables, "PopulationInCity")],
      y=trnSet$PopulationInCity, # train
      k=k, algorithm="kd_tree")
    
    mse <- mean((trnSet$PopulationInCity - knn_mod_train$pred)^2)
    
    Complexity <- 1 / k
    
    data.table("K" = k, "IdTrnSet" = i, 'Complex' = Complexity, "ErrorTrn" = mse)
  }
  ))
  
}, mc.cores = 3)

ModelsKnnPopulation <- rbindlist(ModelsKnnPopulation)
ModelsKnnPopulation[, mean(ErrorTrn), by = K]
ModelsKnnPopulation[, mean(ErrorTrn), by = list(IdTrnSet, K)]

ggplot(ModelsKnnPopulation) +
  geom_point(aes(x = Complex, y = ErrorTrn, color = as.factor(K)))

variables

ModelsKnnPopulation_less <- mclapply(1:10, function(i){
  
  ids <- sample(casesNoMissings$CaseId, 0.6 * nrow(casesNoMissings))
  setOfVariables <- c("Land_nrm", "MeanSalary_nrm", "GDPPerCapita_nrm", "PopulationInCity")
  
  trnSet <- casesNoMissings[CaseId %in% ids, .SD, .SDcols = setOfVariables]
  
  rbindlist(lapply(c(5, 10, 50, 100), function(k){
    knn_mod_train <- FNN::knn.reg(
      train=trnSet[, .SD, .SDcols = setdiff(setOfVariables, "PopulationInCity")],
      test=trnSet[, .SD, .SDcols = setdiff(setOfVariables, "PopulationInCity")],
      y=trnSet$PopulationInCity, # train
      k=100, algorithm="kd_tree")
    
    mse <- mean((trnSet$PopulationInCity - knn_mod_train$pred)^2)
    
    Complexity <- 1 / k
    
    data.table("K" = k, "IdTrnSet" = i, 'Complex' = Complexity, "ErrorTrn" = mse)
  }
  ))
  
}, mc.cores = 3)

ModelsKnnPopulation_less <- rbindlist(ModelsKnnPopulation_less)


ggplot(ModelsKnnPopulation_less) +
  geom_point(aes(x = K, y = ErrorTrn, color = as.factor(K)))


### Gender - knn

casesNoMissings <- na.omit(cases)
variables <- setdiff(names(casesNoMissings), c("CaseId"))

for (v in variables) {
  vnrm <- paste0(v, "_nrm")
  casesNoMissings[, eval(vnrm):=(get(v) - min(get(v)))/(max(get(v)) - min(get(v)))]
} 
variables <- paste0(variables, "_nrm")

ids <- sample(casesNoMissings$CaseId, 0.6 * nrow(casesNoMissings))

trnSet <- casesNoMissings[CaseId %in% ids]
tstSet <- casesNoMissings[CaseId %notin% ids]

knn_mods <- mclapply(c(5, 10, 50, 100, 150, 200, 220, 250, 400, 500, 700), function(k){
  
  knn_mod_tst <- FNN::knn(
    train=trnSet[, .SD, .SDcols = setdiff(variables, "Gender_nrm")],
    test=tstSet[, .SD, .SDcols = setdiff(variables, "Gender_nrm")],
    cl=trnSet$Gender_nrm, # train
    k=k, algorithm="kd_tree")
  
  cfMatrix_tst <- table(knn_mod_tst, tstSet$Gender_nrm)
  testErr <- 1 - sum(diag(cfMatrix_tst))/sum(cfMatrix_tst)
  
  knn_mod_train <- FNN::knn(
    train=trnSet[, .SD, .SDcols = setdiff(variables, "Gender_nrm")],
    test=trnSet[, .SD, .SDcols = setdiff(variables, "Gender_nrm")],
    cl=trnSet$Gender_nrm, # train
    k=k, algorithm="kd_tree")
  
  cfMatrix_trn <- table(knn_mod_train, trnSet$Gender_nrm)
  trainErr <- 1 - sum(diag(cfMatrix_trn))/sum(cfMatrix_trn)
  
  Complexity <- 1 / k
  data.table("K" = k, 'Complex' = Complexity, "ErrorTest" = testErr, "ErrorTrn" = trainErr)
  
}, mc.cores = 3)

knn_mods <- rbindlist(knn_mods)
knn_mods[order(ErrorTrn)]
knn_mods[ErrorTest == min(ErrorTest), ]
knn_mods[ErrorTrn == min(ErrorTrn), ]

plot(knn_mods$K, knn_mods$ErrorTest, ylim = c(0.25, 0.45))
lines(knn_mods$K, knn_mods$ErrorTest)

points(knn_mods$K, knn_mods$ErrorTrn, col = "red")
lines(knn_mods$K, knn_mods$ErrorTrn, col = "red")


knn_mods_2 <- mclapply(seq(200, 460, by = 20), function(k){
  
  knn_mod_tst <- FNN::knn(
    train=trnSet[, .SD, .SDcols = setdiff(variables, "Gender_nrm")],
    test=tstSet[, .SD, .SDcols = setdiff(variables, "Gender_nrm")],
    cl=trnSet$Gender_nrm, # train
    k=k, algorithm="kd_tree")
  
  cfMatrix_tst <- table(knn_mod_tst, tstSet$Gender_nrm)
  testErr <- 1 - sum(diag(cfMatrix_tst))/sum(cfMatrix_tst)
  
  knn_mod_train <- FNN::knn(
    train=trnSet[, .SD, .SDcols = setdiff(variables, "Gender_nrm")],
    test=trnSet[, .SD, .SDcols = setdiff(variables, "Gender_nrm")],
    cl=trnSet$Gender_nrm, # train
    k=k, algorithm="kd_tree")
  
  cfMatrix_trn <- table(knn_mod_train, trnSet$Gender_nrm)
  trainErr <- 1 - sum(diag(cfMatrix_trn))/sum(cfMatrix_trn)
  
  Complexity <- 1 / k
  data.table("K" = k, 'Complex' = Complexity, "ErrorTest" = testErr, "ErrorTrn" = trainErr)
  
}, mc.cores = 3)

knn_mods_2 <- rbindlist(knn_mods_2)
knn_mods_2[order(ErrorTest)]
knn_mods_2[order(ErrorTrn)]

plot(knn_mods_2$K, knn_mods_2$ErrorTest, ylim = c(0.37, 0.39))
lines(knn_mods_2$K, knn_mods_2$ErrorTest)

points(knn_mods_2$K, knn_mods_2$ErrorTrn, col = "red")
lines(knn_mods_2$K, knn_mods_2$ErrorTrn, col = "red")


variables_less <- variables[c(2, 7, 9, 13, 15, 18, 19)]


knn_mods_less <- mclapply(c(5, 10, 50, 100, 150, 200, 220, 250, 400, 500, 700), function(k){
  
  knn_mod_tst <- FNN::knn(
    train=trnSet[, .SD, .SDcols = variables_less],
    test=tstSet[, .SD, .SDcols = variables_less],
    cl=trnSet$Gender_nrm, # train
    k=k, algorithm="kd_tree")
  
  cfMatrix_tst <- table(knn_mod_tst, tstSet$Gender_nrm)
  testErr <- 1 - sum(diag(cfMatrix_tst))/sum(cfMatrix_tst)
  
  knn_mod_train <- FNN::knn(
    train=trnSet[, .SD, .SDcols = variables_less],
    test=trnSet[, .SD, .SDcols = variables_less],
    cl=trnSet$Gender_nrm, # train
    k=k, algorithm="kd_tree")
  
  cfMatrix_trn <- table(knn_mod_train, trnSet$Gender_nrm)
  trainErr <- 1 - sum(diag(cfMatrix_trn))/sum(cfMatrix_trn)
  
  Complexity <- 1 / k
  data.table("K" = k, 'Complex' = Complexity, "ErrorTest" = testErr, "ErrorTrn" = trainErr)
  
}, mc.cores = 3)


knn_mods_less <- rbindlist(knn_mods_less)
knn_mods_less[order(ErrorTest)]
knn_mods[order(ErrorTest)]
knn_mods_less[order(ErrorTrn)]

plot(knn_mods$K, knn_mods$ErrorTest, ylim = c(0.25, 0.45))
points(knn_mods_less$K, knn_mods_less$ErrorTest, col = "darkorange")
lines(knn_mods$K, knn_mods$ErrorTest)
lines(knn_mods_less$K, knn_mods_less$ErrorTest, col = "darkorange")

points(knn_mods$K, knn_mods$ErrorTrn, col = "red")
points(knn_mods_less$K, knn_mods_less$ErrorTrn, col = "brown")
lines(knn_mods$K, knn_mods$ErrorTrn, col = "red")
lines(knn_mods_less$K, knn_mods_less$ErrorTrn, col = "brown")


cases[LoanAmount < Principal,]
hist(log(cases$LoanAmount), breaks = 30)
shapiro.test(sample(log(cases$LoanAmount), 300))
cor(cases, rm.na = TRUE)

hist(sqrt(cases$DPD))

summary(cases[Product == "Cash loan", .(DPD)])
hist(cases$LastPaymentAmount, breaks =100)
summary(cases$LastPaymentAmount)
summary(cases$M_LastPaymentToImportDate)

cases[, .(mean(M_LastPaymentToImportDate, na.rm = TRUE)), by = LastPaymentAmount]
hist(cases[,mean(MeanSalary, rm.na = TRUE),by = Land][order(Land)]$V1)


### M_LastPaymentToImportDate - las

NSIZE <- seq(100, 110, by = 100)
NTREE <- seq(100, 110, by = 100)

p <- floor(length(names(cases)) / 3)

casesFactors <- na.omit(cases[, `:=`(
  ExternalAgency=as.factor(ExternalAgency),
  Bailiff=as.factor(Bailiff),
  ClosedExecution=as.factor(ClosedExecution),
  IsCashLoan=as.factor(IsCashLoan),
  Gender=as.factor(Gender),
  Land=as.factor(Land)
)])

idsM_LPTID <- sample(casesFactors$CaseId, 0.6 * nrow(casesFactors), replace = FALSE)
trnSetM_LPTID <- casesFactors[CaseId %in% idsM_LPTID, .SD, .SDcols = setdiff(names(casesFactors), "CaseId")]
tstSetM_LPTID <- casesFactors[CaseId %notin% idsM_LPTID, .SD, .SDcols = setdiff(names(casesFactors), "CaseId")]

rndForestsStats <- mclapply(NSIZE, function(ns){
  rbindlist(lapply(NTREE, function(nt){
      
      rf <- randomForest(formula=M_LastPaymentToImportDate~.,
                         data=trnSetM_LPTID,
                         mtry=p,
                         nodesize=ns,
                         ntrees=nt,
                         na.action =
                           "na.omit")
      
      Important6Vars <-  names(head(sort(importance(rf)[, 1], decreasing = TRUE), 6))
      
      MSE <- mean((rf$predicted - trnSetM_LPTID$M_LastPaymentToImportDate)^2)
      MeanAbsProp <- mean(abs(rf$predicted - trnSetM_LPTID$M_LastPaymentToImportDate) / rf$predicted)
      
      data.table("NodeSize" = ns, "Ntree" = nt, 
                 "MSE" = MSE, "MeanAbsProp" = MeanAbsProp, 
                 "ChoosenFeatures" = Important6Vars)
      
    }))
}, mc.cores = 3)



rndForestToChooseImportant <- randomForest(M_LastPaymentToImportDate~., data=trnSet,
                                           mtry=p, ntree=500, nodesize=200,
                                           importance=TRUE, keep.inbag=TRUE, keep.forest=TRUE)

ImportantVars <- names(head(sort(importance(rndForestToChooseImportant)[, 1], decreasing = TRUE), 6))

trnSetWithImportant <- trnSet[, .SD, .SDcols = c(ImportantVars, "M_LastPaymentToImportDate")]
rndForestM_LPTID <- randomForest(M_LastPaymentToImportDate~., data=trnSet,
                                 mtry=6, ntree=500, nodesize=200,
                                 importance=TRUE, keep.inbag=TRUE, keep.forest=TRUE)


mean((rndForestM_LPTID$predicted - trnSet$M_LastPaymentToImportDate)^2)
predict(rndForestM_LPTID, tstSetM_LPTID)

