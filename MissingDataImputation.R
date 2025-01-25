rm(list=ls())
gc()

library(data.table)
library(ggplot2)
library(parallel)
library(randomForest)
library(corrplot)
library(caret)
library(FNN)
library(VIM)
library(gbm)


## DANE
load("/Users/zuza/Desktop/studia/semestr7/MWM/Data/KrukUWr2024.RData")

## PODGLĄD DANYCH
summary(cases)
summary(events)

## DEKODOWANIE ZMIENNYCH CHARAKTER

cases[!is.na(Gender), IsFemale := ifelse(Gender == "FEMALE", 1, 0)]
cases[, IsFemale := as.integer(IsFemale)]
cases[, IsCashLoan := ifelse(Product== "Cash loan", 1, 0)]
cases[, IsCashLoan := as.integer(IsCashLoan)]
cases[, `:=`(Product = NULL, Gender = NULL)]

## KORELACJA (przed imputacją)
corrplot(cor(cases, use = "complete.obs"))

## IMPUTACJA BRAKÓW DANYCH

## events

events[is.na(NumberOfPayment), NumberOfPayment := 0]
events[!is.na(PaymentAmount), PaymentAmount := ifelse(PaymentAmount < 0, 0, PaymentAmount)]
events[is.na(PaymentAmount), PaymentAmount := 0]

## cases

### Others - EKSPERCKO

cases[is.na(Other), .N]
cases[is.na(Other), Other := TOA - (Principal + Interest)]
cases[is.na(Other), .N]

### D_ContractDateToImportDate - MEDIANA (MAŁO BRAKÓW DANYCH)

cases[is.na(D_ContractDateToImportDate), .N]
cases[is.na(D_ContractDateToImportDate), D_ContractDateToImportDate := median(cases$D_ContractDateToImportDate, na.rm = TRUE)]

### Bailiff, ClosedExecution, ExternalAgency - Z ROZKŁADU

cases[is.na(Bailiff), .N]
cases[is.na(ClosedExecution), .N]
cases[is.na(ExternalAgency), .N]

cases[, .N, by = .(Bailiff, ClosedExecution)]

probB <- mean(cases$Bailiff, na.rm = TRUE)
probCL <- mean(cases$ClosedExecution, na.rm = TRUE)
probEA <- mean(cases$ExternalAgency, na.rm = TRUE)

cases[is.na(Bailiff), Bailiff := sample(c(1, 0), .N, prob = c(probB, 1 - probB), replace = TRUE)]
cases[is.na(ClosedExecution) & Bailiff == 0, ClosedExecution := 0]
cases[is.na(ClosedExecution) & Bailiff == 1, ClosedExecution := sample(c(1, 0), .N ,prob = c(probCL, 1 - probCL), replace = TRUE)]
cases[is.na(ExternalAgency), ExternalAgency := sample(c(1, 0), .N ,prob = c(probEA, 1 - probEA), replace = TRUE)]

cases[is.na(Bailiff), .N]
cases[is.na(ClosedExecution), .N]
cases[is.na(ExternalAgency), .N]

### Land - Z ROZKŁADU

cases[is.na(Land), .N]
cases[, .N, by = Land][order(Land)]

problands_dt <- cases[, .("probL" = .N/nrow(cases)) ,by = Land][order(Land)]
cases[is.na(Land),  Land := sample(1:37, .N, prob = problands_dt$probL[-1], replace = TRUE)]

cases[is.na(Land), .N]

### GDPPerCapita, MeanSalary - ŚREDNIA, Z PODZIAŁEM ZE WZGLĘDU NA LAND

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

cases[is.na(GDPPerCapita), .N]
cases[is.na(MeanSalary), .N]

rm(i, probB, probCL, probEA, GDP_stats, MeanSalary_stats, problands_dt)

### IsFemale - NA KOBIETĘ + Age dla firm - ROZKŁAD WIEKU U KOBIET

cases[,.N/nrow(cases), by = IsFemale] 
cases[is.na(IsFemale), IsFemale := 1]

cases[is.na(IsFemale), .N]

AgeFemale <- cases[IsFemale == 1, Age]
cases[Age == -1, Age := sample(AgeFemale, .N, replace = TRUE)]

rm(AgeFemale)

### LoanAmount - PODZIAŁ ZE WZGLĘDU NA PRODUKT

summary(cases)
corrplot(cor(cases, cases, use = "complete.obs"))

cases[is.na(LoanAmount), .(.N), by = IsCashLoan]
cases[, .(.N), by = IsCashLoan]

ggplot(cases[, .(LoanAmount), by = IsCashLoan]) +
  geom_boxplot(aes(x=as.factor(IsCashLoan), y=LoanAmount))+ 
  coord_cartesian(ylim = c(0, quantile(cases$LoanAmount, 0.9, na.rm = TRUE)*1.05))

cases[is.na(LoanAmount) & IsCashLoan == 1, LoanAmount := cases[IsCashLoan == 1, median(LoanAmount, na.rm = TRUE)]]
cases[is.na(LoanAmount) & IsCashLoan == 0, LoanAmount := cases[IsCashLoan == 0, median(LoanAmount, na.rm = TRUE)]]

cases[is.na(LoanAmount), .N]

### M_LastPaymentToImportDate - Z ROZKŁADU

cases[is.na(M_LastPaymentToImportDate), .N]

ProbM_LPTIDdt <- cases[!is.na(M_LastPaymentToImportDate), 
             .(Pr=.N/cases[!is.na(M_LastPaymentToImportDate), .N]), 
             by=M_LastPaymentToImportDate]

ProbM_LPTIDdt <- ProbM_LPTIDdt[order(M_LastPaymentToImportDate)]

cases[is.na(M_LastPaymentToImportDate), M_LastPaymentToImportDate := sample(ProbM_LPTIDdt$M_LastPaymentToImportDate, .N, prob = ProbM_LPTIDdt$Pr, replace = TRUE)]

cases[is.na(M_LastPaymentToImportDate), .N]

rm(ProbM_LPTIDdt)

### LastPaymentAmount - UZUPEŁNIANIE LASEM ORAZ Z ROZKŁADU

casesFactors <- na.omit(cases[, `:=`(
  ExternalAgency=as.factor(ExternalAgency),
  Bailiff=as.factor(Bailiff),
  ClosedExecution=as.factor(ClosedExecution),
  IsCashLoan=as.factor(IsCashLoan),
  IsFemale=as.factor(IsFemale),
  Land=as.factor(Land)
)])

NSIZE <- seq(100, 500, by = 100)
NTREE <- seq(600, 1000, by = 100)

p <- floor(length(names(cases)) / 3)

idsLPA <- sample(casesFactors$CaseId, 0.6 * nrow(casesFactors), replace = FALSE)
trnSetLPA <- casesFactors[CaseId %in% idsLPA, .SD, .SDcols = setdiff(names(casesFactors), "CaseId")]
tstSetLPA <- casesFactors[CaseId %notin% idsLPA, .SD, .SDcols = setdiff(names(casesFactors), "CaseId")]

rndForestsStatsLPA <- mclapply(NSIZE, function(ns){
  rbindlist(lapply(NTREE, function(nt){
    
    rf <- randomForest(formula=LastPaymentAmount~.,
                       data=trnSetLPA,
                       mtry=p,
                       nodesize=ns,
                       ntrees=nt,
                       na.action =
                         "na.omit")
    
    Important6Vars <-  names(head(sort(importance(rf)[, 1], decreasing = TRUE), 6))
    
    MSE <- mean((rf$predicted - trnSetLPA$LastPaymentAmount)^2)
    MeanAbsProp <- mean(abs(rf$predicted - trnSetLPA$LastPaymentAmount) / rf$predicted)
    
    data.table("NodeSize" = ns, "Ntree" = nt,
               "MSE" = MSE, "MeanAbsProp" = MeanAbsProp,
               "ChoosenFeatures" = Important6Vars)
    
  }))
}, mc.cores = 10)

rndForestsStatsLPA_dt <- readRDS(file = "/Users/zuza/Desktop/studia/semestr7/MWM/Data_proj/rndForestsStatsLPA_dt.RDS")

rndForestsLPA_MSE <- rndForestsStatsLPA_dt[,.(minMSE = min(MSE), minMAP = min(MeanAbsProp)), by = .(NodeSize, Ntree)][order(minMSE)]
rndForestsLPA_MAP <- rndForestsStatsLPA_dt[,.(minMSE = min(MSE), minMAP = min(MeanAbsProp)), by = .(NodeSize, Ntree)][order(minMAP)]

rndForestsStatsLPA_dt[, .N/25, by = ChoosenFeatures]

paramsLPA <- rndForestsLPA_MAP[1, ]

rf_LPA<- randomForest(formula=LastPaymentAmount~ Principal + TOA + LoanAmount + DPD + Interest + M_LastPaymentToImportDate,
                           data=trnSetLPA ,
                           nodesize=paramsLPA$NodeSize,
                           ntrees=paramsLPA$Ntree)

mean(abs(rf_LPA$predicted - trnSetLPA$LastPaymentAmount) / rf_LPA$predicted)

casesdtImp_LPA <- cases[!is.na(DPD) & is.na(LastPaymentAmount), ]
cases[!is.na(DPD) & is.na(LastPaymentAmount), LastPaymentAmount := predict(rf_LPA, casesdtImp_LPA)]

#### PRZYPADKI, GDZIE BRAKUJE DPD 

cases[is.na(LastPaymentAmount), .(LastPaymentAmount, M_LastPaymentToImportDate)]
LPAbyM_LPTIDdt <- cases[, .("Med" = median(LastPaymentAmount, na.rm = TRUE)), by = M_LastPaymentToImportDate][order(M_LastPaymentToImportDate)]

for(i in LPAbyM_LPTIDdt$M_LastPaymentToImportDate){
  cases[is.na(LastPaymentAmount) & M_LastPaymentToImportDate == i, LastPaymentAmount := LPAbyM_LPTIDdt[M_LastPaymentToImportDate==i, Med]]
}

cases[is.na(LastPaymentAmount), .N]

rm(idsLPA, trnSetLPA, tstSetLPA, rf_LPA, 
   rndForestsLPA_MSE, rndForestsLPA_MAP, 
   paramsLPA, LPAbyM_LPTIDdt, casesdtImp_LPA,
   rndForestsStatsLPA, i, p, rndForestsStatsLPA_dt,
   casesFactors, NSIZE, NTREE)

### DPD - UZUPEŁNIANIE ZA POMOCĄ BOOSTINGU

casesFactors <- na.omit(cases[, `:=`(
  ExternalAgency=as.factor(ExternalAgency),
  Bailiff=as.factor(Bailiff),
  ClosedExecution=as.factor(ClosedExecution),
  IsCashLoan=as.factor(IsCashLoan),
  IsFemale=as.factor(IsFemale),
  Land=as.factor(Land)
)])

idsDPD <- sample(casesFactors$CaseId, 0.6 * nrow(casesFactors), replace = FALSE)
trnSetDPD <- casesFactors[CaseId %in% idsDPD, .SD, .SDcols = setdiff(names(casesFactors), "CaseId")]
tstSetDPD <- casesFactors[CaseId %notin% idsDPD, .SD, .SDcols = setdiff(names(casesFactors), "CaseId")]

DEPTH <- 1:10
MINNODES <- seq(100, 2000, by = 100)
PERCVARS <-  seq(0.2, 1, by = 0.2)

gbms_DPD_1 <- mclapply(DEPTH, function(d){
  rbindlist(lapply(MINNODES, function(mn){
    rbindlist(lapply(PERCVARS, function(perc){
      
      gbm_model <- gbm(formula = DPD~.,
                       distribution = "gaussian",
                       data = trnSetDPD,
                       n.trees = 100,
                       interaction.depth = d,
                       shrinkage = 0.1,
                       n.minobsinnode = mn,
                       bag.fraction = perc)
      
      MSE <- mean((round(gbm_model$fit) - trnSetDPD$DPD)^2)
      MeanAbsProp <- mean(abs(round(gbm_model$fit) - trnSetDPD$DPD) / round(gbm_model$fit))
      
      ImportanceVars <- summary.gbm(gbm_model)$var
      RelInf <- summary.gbm(gbm_model)$rel.inf
      
      data.table("Depth" = d,
                 "min.nodes" = mn,
                 "bag.fraction" = perc,
                 "MSE" = MSE, "MeanAbsProp" = MeanAbsProp,
                 "ImportanceVars" = ImportanceVars,
                 "RelInf" = RelInf)
    }))
  }))
}, mc.cores = 10)

gbms_DPD_1_dt <- readRDS(file = "/Users/zuza/Desktop/studia/semestr7/MWM/Data_proj/gbms_DPD_1_dt.RDS")

gbmsDPD_MSE <- gbms_DPD_1_dt[,.(minMSE = min(MSE), minMAP = min(MeanAbsProp)), by = .(Depth, min.nodes, bag.fraction)][order(minMSE)]
gbmsDPD_MAP <- gbms_DPD_1_dt[,.(minMSE = min(MSE), minMAP = min(MeanAbsProp)), by = .(Depth, min.nodes, bag.fraction)][order(minMAP)]

gbms_DPD_1_dt[, mean(RelInf), by = ImportanceVars][order(V1, decreasing = TRUE)]

params_gbms <- gbmsDPD_MSE[1, ]


NTREE_gbm <- seq(200, 600, by = 100)
SHRINK <- seq(0.1, 0.001, by = -0.01)

gbms_HP_ntree_sh <- mclapply(SHRINK, function(sh){
  rbindlist(lapply(NTREE_gbm, function(nt){
    
    gbm_model <- gbm(formula = DPD~.,
                     distribution = "gaussian",
                     data = trnSetDPD,
                     n.trees = nt,
                     interaction.depth = params_gbms$Depth,
                     shrinkage = sh,
                     n.minobsinnode = params_gbms$min.nodes,
                     bag.fraction = params_gbms$bag.fraction)
    
    MSE <- mean((round(gbm_model$fit) - trnSetDPD$DPD)^2)
    MeanAbsProp <- mean(abs(round(gbm_model$fit) - trnSetDPD$DPD) / round(gbm_model$fit))
    
    ImportanceVars <- summary.gbm(gbm_model)$var
    RelInf <- summary.gbm(gbm_model)$rel.inf
    
    data.table("Shrinkage" = sh,
               "N.Tree" = nt,
               "MSE" = MSE, "MeanAbsProp" = MeanAbsProp,
               "ImportanceVars" = ImportanceVars,
               "RelInf" = RelInf)
    
  }))
}, mc.cores = 3)

gbms_HP_ntree_sh_dt2 <-readRDS("/Users/zuza/Desktop/studia/semestr7/MWM/Data_proj/gbms_HP_ntree_sh_dt2.RDS")

gbmsDPD_MSE <- gbms_HP_ntree_sh_dt2[,.(minMSE = min(MSE), minMAP = min(MeanAbsProp)), by = .(Shrinkage, N.Tree)][order(minMSE)]
params_DPD <- c(gbmsDPD_MSE[1, ], params_gbms)

ggplot(gbms_HP_ntree_sh_dt2) + 
  geom_boxplot(aes(x = ImportanceVars, y = RelInf))

gbms_HP_ntree_sh_dt2[, .(mean(RelInf), mean(RelInf)>2), by = ImportanceVars][order(V1, decreasing = TRUE)]

DPDgbm_model <- gbm(formula = DPD~ D_ContractDateToImportDate+LastPaymentAmount+Other+Interest+ExternalAgency+IsCashLoan+M_LastPaymentToImportDate+LoanAmount+Bailiff,
                 distribution = "gaussian",
                 data = trnSetDPD,
                 n.trees = params_DPD$N.Tree,
                 interaction.depth = params_gbms$Depth,
                 shrinkage = params_DPD$Shrinkage,
                 n.minobsinnode = params_gbms$min.nodes,
                 bag.fraction = params_gbms$bag.fraction)

mean(abs(round(DPDgbm_model$fit) - trnSetDPD$DPD) / round(DPDgbm_model$fit))

cases[is.na(DPD), DPD := round(predict(DPDgbm_model, cases[is.na(DPD), ]))]
cases[is.na(DPD), .N]

rm(casesFactors, DPDgbm_model, gbms_DPD_1, gbms_HP_ntree_sh_dt2, 
   params_gbms, trnSetDPD, tstSetDPD, idsDPD, gbmsDPD_MAP,
   gbmsDPD_MSE, params_DPD, gbms_DPD_1_dt)

### PopulationInCity - UZUPEŁNIANIE KNN

cases[, `:=`(
  ExternalAgency=as.integer(ExternalAgency),
  Bailiff=as.integer(Bailiff),
  ClosedExecution=as.integer(ClosedExecution),
  IsCashLoan=as.integer(IsCashLoan),
  IsFemale=as.integer(IsFemale),
  Land=as.integer(Land)
)]

casestmp <- copy(cases)

corrplot(cor(cases$PopulationInCity, cases, use = "complete.obs"))

variables <- setdiff(names(casestmp), c("PopulationInCity", "CaseId"))

for (v in variables) {
  vnrm <- paste0(v, "_nrm")
  casestmp[, eval(vnrm):=(get(v) - min(get(v)))/(max(get(v)) - min(get(v)))]
} 

variables <- paste0(variables, "_nrm")
casestmp_full <- casestmp[!is.na(PopulationInCity), ]

ModelsKnnPopulation <- mclapply(1:100, function(i){
  
  ids <- sample(c(0,1), replace = TRUE, prob = c(0.6, 0.4), nrow(casestmp_full))
  trnSet <- casestmp_full[ids==0, .SD, .SDcols = c(variables, "PopulationInCity")]
  
  rbindlist(lapply(c(5:10, 20, 30), function(k){
    knn_mod_train <- FNN::knn.reg(
      train=trnSet[, .SD, .SDcols = setdiff(variables, "PopulationInCity")],
      test=trnSet[, .SD, .SDcols = setdiff(variables, "PopulationInCity")],
      y=trnSet$PopulationInCity, # train
      k=k, algorithm="kd_tree")
    
    mse <- mean((trnSet$PopulationInCity - knn_mod_train$pred)^2)
    MeanAbsProp <- mean(abs(knn_mod_train$pred - trnSet$PopulationInCity) / knn_mod_train$pred)
    
    Complexity <- 1 / k
    
    data.table("K" = k, "IdTrnSet" = i, 
               'Complex' = Complexity, 
               "ErrorTrnMSE" = mse, "ErrorTrnMeanAbsProp" = MeanAbsProp)
  }
  ))
  
}, mc.cores = 3)

ModelsKnnPopulation_dt <- rbindlist(ModelsKnnPopulation)
ModelsKnnPopulation_dt[,mean(ErrorTrnMeanAbsProp) , by = K]

saveRDS(ModelsKnnPopulation_dt, "/Users/zuza/Desktop/studia/semestr7/MWM/Data_proj/ModelsKnnPopulation_dt1.RDS")

ggplot(ModelsKnnPopulation_dt) +
  geom_boxplot(aes(x = as.factor(K), y = ErrorTrnMeanAbsProp))

trnSet <- casestmp_full
tstSet <- casestmp[is.na(PopulationInCity), ]

knn_model <- FNN::knn.reg(
  train=trnSet[, .SD, .SDcols = setdiff(variables, "PopulationInCity")],
  test=tstSet[, .SD, .SDcols = setdiff(variables, "PopulationInCity")],
  y=trnSet$PopulationInCity, # train
  k=5, algorithm="kd_tree")


cases[is.na(PopulationInCity), PopulationInCity:=knn_model$pred]

cases[is.na(PopulationInCity), .N]

rm(casestmp, knn_model, ModelsKnnPopulation_dt,
   ModelsKnnPopulation, trnSet, ids, v, variables,
   vnrm, casestmp_full)

## ZAPIS PEŁNYCH DANYCH

casesFull[, ClosedExecution := ifelse(ClosedExecution == 1, 0, 1)]
casesFull[, Bailiff := ifelse(Bailiff == 1, 0, 1)]
casesFull[, IsFemale := ifelse(IsFemale == 1, 0, 1)]
casesFull[, IsCashLoan := ifelse(IsCashLoan == 1, 0, 1)]
casesFull[, ExternalAgency := ifelse(ExternalAgency ==1, 0, 1)]

casesFull <- copy(cases)
eventsImpFull <- copy(events)

summary(casesFull)

save(casesFull, file = "/Users/zuza/Desktop/studia/semestr7/MWM/Data_proj/casesFull.RData")
save(eventsImpFull, file = "/Users/zuza/Desktop/studia/semestr7/MWM/Data_proj/eventsImpFull.RData")

