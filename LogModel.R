rm(list=ls())
gc()

library(data.table)
library(ggplot2)
library(MASS)
library(car)
library(pROC)
library(parallel)


## DANE

load("/Users/zuza/Desktop/studia/semestr7/MWM/Data_proj/casesFullClust.RData")
Baskets_dt <- readRDS("/Users/zuza/Desktop/studia/semestr7/MWM/Data_proj/Baskets_dt.RDS")

casesTmp <- copy(casesFullClust)

## PERMUTACJA ZMIENNYCH/ KOSZYKÓW

VarPermutationStatsCompute <- function(VarName, Setdt, LogModel){
  PermSet <- copy(Setdt)
  VarValues <- PermSet[, get(VarName)]
  PermSet[, eval(VarName) := sample(VarValues, nrow(PermSet))]
  
  ProbPredsLog <- predict.glm(LogModel, PermSet, type = "response")
  RocCurve <- roc(Setdt$Suc12M, ProbPredsLog)
  Coords <- coords(RocCurve, "best", best.method = "closest.topleft")
  PredLog <- sapply(ProbPredsLog, function(xi) ifelse(xi >= Coords$threshold, 1, 0))

  CfMatrix <- table(PredLog, Setdt$Suc12M)
  ClassErr <- 1 - sum(diag(CfMatrix))/sum(CfMatrix)

  data.table("VarName" = VarName,
             "ClassError" = ClassErr,
             "AUC" = RocCurve$auc)
}


BasketPermutationStatsCompute <- function(id_basket, Setdt, LogModel){
  PermSet <- copy(Setdt)
  features <- Baskets_dt[Id_Basket == id_basket, Var]
  BasketValues <- PermSet[, .SD, .SDcols = features]
  for(var in features){
    PermSet[, eval(var) := sample(BasketValues[, get(var)], nrow(PermSet))]
  }
  
  ProbPredsLog <- predict.glm(LogModel, PermSet, type = "response")
  RocCurve <- roc(Setdt$Suc12M, ProbPredsLog)
  Coords <- coords(RocCurve, "best", best.method = "closest.topleft")
  PredLog <- sapply(ProbPredsLog, function(xi) ifelse(xi >= Coords$threshold, 1, 0))
  
  CfMatrix <- table(PredLog, Setdt$Suc12M)
  ClassErr <- 1 - sum(diag(CfMatrix))/sum(CfMatrix)
  
  data.table("Id_Basket" = id_basket,
             "ClassError" = ClassErr, 
             "AUC" = RocCurve$auc)
}

## PEŁNY MODEL:

variables <- setdiff(names(casesTmp), c("CaseId", "IdPortfolio", "Suc12M"))

ids<- sample(0:1, size = nrow(casesTmp), replace = TRUE,  prob = c(0.6, 0.4))
trnSet <- casesTmp[ids==0, .SD, .SDcols = c(variables, "Suc12M")]
tstSet <- casesTmp[ids==1, .SD, .SDcols = c(variables, "Suc12M")]

LogFullModel <- glm(Suc12M ~., data = trnSet, family = binomial())

## BŁĄD TESTOWY I AUC:

ProbPredsLog <- predict.glm(LogFullModel, tstSet, type = "response")
RocCurve <- roc(tstSet$Suc12M, ProbPredsLog)
Coords <- coords(RocCurve, "best", best.method = "closest.topleft")
PredLog <- sapply(ProbPredsLog, function(xi) ifelse(xi >= Coords$threshold, 1, 0))

CfMatrix <- table(PredLog, tstSet$Suc12M)

rm(ProbPredsLog, RocCurve, Coords, PredLog, CfMatrix)

TstSetStats <- data.table("VarName" = "All",
                          "ClassError" = 1 - sum(diag(CfMatrix))/sum(CfMatrix), 
                          "AUC" = RocCurve$auc)

### ISTOTNOŚĆ CECH ZA POMOCĄ PERMUTACJI JEDNEJ Z CECH

PermOneVarStats_dt <- rbindlist(lapply(variables, VarPermutationStatsCompute, tstSet, LogFullModel))

PropStats_dt <- as.data.table(sapply(2:3, function(j) PermOneVarStats_dt[, ..j]/as.numeric(TstSetStats[, ..j])))
PropStats_dt[, "VarName" := PermOneVarStats_dt$VarName]

PropStats_dt <- PropStats_dt[order(ClassError, decreasing = TRUE)]
PropStats_dt[, "RankPermOne" := 1:19]

PropStats_dt <- PropStats_dt[order(AUC)]
PropStats_dt[, "RankPermOneAUC" := 1:19]

PermOneVarStats_dt <- rbind(PermOneVarStats_dt, TstSetStats)

### ISTOTNOŚĆ CECH ZA POMOCĄ SUMMARY

SummFullLogModel <- summary(LogFullModel)
CoefStats <- SummFullLogModel$coefficients
CoefPvals_dt <-  data.table(CoefStats[2:20, "Pr(>|z|)"])
CoefPvals_dt[, "VarName" := variables]
setnames(CoefPvals_dt, "V1", "Pval")

CoefPvals_dt <- CoefPvals_dt[order(Pval)]
CoefPvals_dt[, "RankPvals" := 1:19]

PropStats_dt <- CoefPvals_dt[PropStats_dt, on =.(VarName)]

###  ISTOTNOŚĆ CECH ZA POMOCĄ PERMUTACJI JEDNEGO Z KOSZYKÓW 

PermOneBasketStats_dt <- rbindlist(lapply(1:4, BasketPermutationStatsCompute, tstSet, LogFullModel))

PropStatsBaskets_dt <- as.data.table(sapply(2:3, function(j) PermOneBasketStats_dt[, ..j]/as.numeric(TstSetStats[, ..j])))
PropStatsBaskets_dt[, Id_Basket := PermOneBasketStats_dt$Id_Basket]
PropStatsBaskets_dt <- PropStatsBaskets_dt[order(ClassError, decreasing = TRUE)]
PropStatsBaskets_dt[, "RankBasket" := 1:4]
PropStatsBaskets_dt <- PropStatsBaskets_dt[order(AUC)]
PropStatsBaskets_dt[, "RankAUC" := 1:4] 


### ISTOTNOŚĆ KOSZYKÓW ZA POMOCĄ SUMMARY

CoefPvals_dt <- CoefPvals_dt[Baskets_dt, on =.(VarName==Var)] 
CoefPvalsBaskets_dt <- CoefPvals_dt[,.(MeanPval = mean(Pval)), by=Id_Basket]
CoefPvalsBaskets_dt <- CoefPvalsBaskets_dt[order(MeanPval)]
CoefPvalsBaskets_dt[, "RankBasketPvals" := 1:4]

LogBasketsStats_dt <- CoefPvalsBaskets_dt[PropStatsBaskets_dt, on =.(Id_Basket)]

### ZAPIS DANYCH

saveRDS(CoefPvals_dt, "/Users/zuza/Desktop/studia/semestr7/MWM/Data_proj/LogCoefPvals_dt.RDS")
saveRDS(CoefPvalsBaskets_dt, "/Users/zuza/Desktop/studia/semestr7/MWM/Data_proj/LogBasketsStats_dt.RDS")
saveRDS(PermOneVarStats_dt, "/Users/zuza/Desktop/studia/semestr7/MWM/Data_proj/LogPermOneVarStats_dt.RDS")
saveRDS(PermOneBasketStats_dt, "/Users/zuza/Desktop/studia/semestr7/MWM/Data_proj/PermOneBasketStats_dt.RDS")
saveRDS(PropStats_dt, "/Users/zuza/Desktop/studia/semestr7/MWM/Data_proj/LogPropStats_dt.RDS")


