rm(list=ls())
gc()

library(data.table)
library(ggplot2)
library(parallel)
library(gbm)
library(pROC)
library(corrplot)

## WCZYTANIE DANYCH

load("/Users/zuza/Desktop/studia/semestr7/MWM/Data_proj/casesFullClust.RData")

Baskets_dt <- readRDS("/Users/zuza/Desktop/studia/semestr7/MWM/Data_proj/Baskets_dt.RDS")


corrplot(cor(casesFull[, .SD, .SDcols = Baskets_dt[order(Id_Basket)][, Var]]),  tl.cex = 0.7)
str(casesFullClust)

casesFullFactors <- copy(casesFullClust)

casesFullFactors <- casesFullFactors[, `:=`(
  ExternalAgency=as.factor(ExternalAgency),
  Bailiff=as.factor(Bailiff),
  ClosedExecution=as.factor(ClosedExecution),
  IsCashLoan=as.factor(IsCashLoan),
  IsFemale=as.factor(IsFemale),
  Land=as.factor(Land)
)]

str(casesFullFactors)

## PERMUTACJA ZMIENNYCH/ KOSZYKÓW

VarPermutationStatsCompute <- function(VarName, Setdt, GbmModel){
  PermSet <- copy(Setdt)
  VarValues <- PermSet[, get(VarName)]
  PermSet[, eval(VarName) := sample(VarValues, nrow(Setdt))]
  
  ProbPredsGbm <- predict.gbm(GbmModel, PermSet, n.trees = GbmModel$n.trees, type = "response")
  RocCurve <- roc(PermSet$Suc12M, ProbPredsGbm)
  Coords <- coords(RocCurve, "best", best.method = "closest.topleft")
  PredGbm <- sapply(ProbPredsGbm, function(xi) ifelse(xi >= Coords$threshold, 1, 0))
  
  CfMatrix <- table(PredGbm, Setdt$Suc12M)
  ClassErr <- 1 - sum(diag(CfMatrix))/sum(CfMatrix)
  
  data.table("VarName" = VarName,
             "ClassError" = ClassErr, 
             "AUC" = RocCurve$auc, 
             "Sensitivity" = Coords$sensitivity, 
             "Specificity" = Coords$specificity)
}

BasketPermutationStatsCompute <- function(id_basket, Setdt, GbmModel){
  PermSet <- copy(Setdt)
  features <- Baskets_dt[Id_Basket == id_basket, Var]
  BasketValues <- PermSet[, .SD, .SDcols = features]
  for(var in features){
    PermSet[, eval(var) := sample(BasketValues[, get(var)], nrow(Setdt))]
  }
 
  ProbPredsGbm <- predict.gbm(GbmModel, PermSet, n.trees = GbmModel$n.trees, type = "response")
  RocCurve <- roc(Setdt$Suc12M, ProbPredsGbm)
  Coords <- coords(RocCurve, "best", best.method = "closest.topleft")
  PredGbm <- sapply(ProbPredsGbm, function(xi) ifelse(xi >= Coords$threshold, 1, 0))
  
  CfMatrix <- table(PredGbm, Setdt$Suc12M)
  ClassErr <- 1 - sum(diag(CfMatrix))/sum(CfMatrix)
  
  data.table("Id_Basket" = id_basket,
             "ClassError" = ClassErr, 
             "AUC" = RocCurve$auc, 
             "Sensitivity" = Coords$sensitivity, 
             "Specificity" = Coords$specificity)
}


## ZNALEZNIENIE NAJLEPSZEGO MODELU BOOSTING

ids<- sample(0:2, size = nrow(casesFullFactors), replace = TRUE,  prob = c(0.6, 0.2, 0.2))
trnSet <- casesFullFactors[ids==0, .SD, .SDcols = setdiff(names(casesFullFactors), c("CaseId", "IdPortfolio"))]
vldSet <- casesFullFactors[ids==1, .SD, .SDcols = setdiff(names(casesFullFactors), c("CaseId", "IdPortfolio"))]
tstSet <- casesFullFactors[ids==2, .SD, .SDcols = setdiff(names(casesFullFactors), c("CaseId", "IdPortfolio"))]

DEPTH <- 1:10
MINNODES <- seq(100, 1000, by = 100)
PERCVARS <-  seq(0.2, 1, by = 0.2)

gbms_Suc12M <- mclapply(DEPTH, function(d){
  rbindlist(lapply(MINNODES, function(mn){
    rbindlist(lapply(PERCVARS, function(perc){
      
      GbmModel <- gbm(formula = Suc12M~.,
                       distribution = "bernoulli",
                       data = trnSet,
                       n.trees = 100,
                       interaction.depth = d,
                       shrinkage = 0.1,
                       n.minobsinnode = mn,
                       bag.fraction = perc)
      
      ProbPredsGbm <- predict.gbm(GbmModel, vldSet, n.trees = GbmModel$n.trees, type = "response")
      RocCurve <- roc(vldSet$Suc12M, ProbPredsGbm)
      Coords <- coords(RocCurve, "best", best.method = "closest.topleft")
      PredGbm <- sapply(ProbPredsGbm, function(xi) ifelse(xi >= Coords$threshold, 1, 0))
      
      CfMatrix <- table(PredGbm, vldSet$Suc12M)
      ClassErr <- 1 - sum(diag(CfMatrix))/sum(CfMatrix)
      
      ImportanceVars <- summary.gbm(GbmModel, plotit = FALSE)$var
      RelInf <- summary.gbm(GbmModel, plotit = FALSE)$rel.inf
      
      data.table("Depth" = d,
                 "min.nodes" = mn,
                 "bag.fraction" = perc,
                 "ClassError" = ClassErr,
                 "threshold" = Coords$threshold,
                 "ImportanceVars" = ImportanceVars,
                 "RelInf" = RelInf)
    }))
  }))
}, mc.cores = 3)

# gbms_Suc12M_dt <- rbindlist(gbms_Suc12M)
gbms_Suc12M_dt<- readRDS("/Users/zuza/Desktop/studia/semestr7/MWM/Data_proj/gbms_Suc12M_dt.RDS")

CEdt <- gbms_Suc12M_dt[, .(ClassErr = min(ClassError)),by = .(Depth, min.nodes, bag.fraction)][order(ClassErr)]
params_1 <- CEdt[1, 1:3]

gbms_Suc12M_ImpVars_dt<- gbms_Suc12M_dt[, .(MeanRelInf = mean(RelInf)), by = .(ImportanceVars)]
setnames(gbms_Suc12M_ImpVars_dt, "ImportanceVars", "Var")
gbms_Suc12M_ImpVars_dt <- Baskets_dt[gbms_Suc12M_ImpVars_dt, on = .(Var)]
setnames(gbms_Suc12M_ImpVars_dt, "Var", "ImportanceVars")

gbms_Suc12M_AllRelInf_dt <- gbms_Suc12M_dt[, .(ImportanceVars, RelInf)][gbms_Suc12M_ImpVars_dt[, .(ImportanceVars, Id_Basket)], on = .(ImportanceVars)]

ggplot(gbms_Suc12M_AllRelInf_dt) +
  geom_boxplot(aes(x = ImportanceVars, y = RelInf, fill = as.factor(Id_Basket))) +
  theme(axis.text.x=element_text(color = "black", angle=30, vjust=.8, hjust=0.8)) 

ggplot(gbms_Suc12M_AllRelInf_dt) +
  geom_boxplot(aes(x = ImportanceVars, y = RelInf)) +
  theme(axis.text.x=element_text(color = "black", angle=30, vjust=.8, hjust=0.8)) 

ggplot(gbms_Suc12M_AllRelInf_dt) +
  geom_boxplot(aes(x = ImportanceVars, y = RelInf, col = as.factor(Id_Basket))) +
  theme(axis.text.x=element_text(color = "black", angle=30, vjust=.8, hjust=0.8)) +
  facet_wrap(Id_Basket~.)

rm(DEPTH, MINNODES, PERCVARS, ids, CEdt, gbms_Suc12M_ImpVars_dt, gbms_Suc12M_dt)


NTREE_gbm <- seq(200, 1000, by = 100)
SHRINK <- seq(0.09, 0.001, by = -0.01)

gbms_Suc12M_2 <- mclapply(SHRINK, function(sh){
  rbindlist(lapply(NTREE_gbm, function(nt){
    
    GbmModel <- gbm(formula = Suc12M~.,
                    distribution = "bernoulli",
                    data = trnSet,
                    n.trees = nt,
                    interaction.depth = params_1$Depth,
                    shrinkage = sh,
                    n.minobsinnode = params_1$min.nodes,
                    bag.fraction = params_1$bag.fraction)
    
    ProbPredsGbm <- predict.gbm(GbmModel, vldSet, n.trees = GbmModel$n.trees, type = "response")
    RocCurve <- roc(vldSet$Suc12M, ProbPredsGbm)
    Coords <- coords(RocCurve, "best", best.method = "closest.topleft")
    PredGbm <- sapply(ProbPredsGbm, function(xi) ifelse(xi >= Coords$threshold, 1, 0))
    
    CfMatrix <- table(PredGbm, vldSet$Suc12M)
    ClassErr <- 1 - sum(diag(CfMatrix))/sum(CfMatrix)
    
    ImportanceVars <- summary.gbm(GbmModel, plotit = FALSE)$var
    RelInf <- summary.gbm(GbmModel, plotit = FALSE)$rel.inf
    
    data.table("Ntree" = nt,
               "Shrinkage" = sh,
               "ClassError" = ClassErr,
               "threshold" = Coords$threshold,
               "ImportanceVars" = ImportanceVars,
               "RelInf" = RelInf)
  }))
}, mc.cores = 3)

# gbms_Suc12_2_dt <- rbindlist(gbms_Suc12M_2)
gbms_Suc12_2_dt <- readRDS("/Users/zuza/Desktop/studia/semestr7/MWM/Data_proj/gbms_Suc12_2_dt.RDS")

CE_2_dt <- gbms_Suc12_2_dt[, .(ClassErr = min(ClassError)),by = .(Ntree, Shrinkage)][order(ClassErr)]
params <- c(CE_2_dt[1, 1:2], params_1)

gbms_Suc12M_ImpVars_2_dt<- gbms_Suc12_2_dt[, .(MeanRelInf = mean(RelInf)), by = .(ImportanceVars)]
setnames(gbms_Suc12M_ImpVars_2_dt, "ImportanceVars", "Var")
gbms_Suc12M_ImpVars_2_dt <- Baskets_dt[gbms_Suc12M_ImpVars_2_dt, on = .(Var)]
setnames(gbms_Suc12M_ImpVars_2_dt, "Var", "ImportanceVars")

gbms_Suc12M_AllRelInf_2_dt <- gbms_Suc12_2_dt[, .(ImportanceVars, RelInf)][gbms_Suc12M_ImpVars_2_dt[, .(ImportanceVars, Id_Basket)], on = .(ImportanceVars)]

ggplot(gbms_Suc12M_AllRelInf_2_dt) +
  geom_boxplot(aes(x = ImportanceVars, y = RelInf, fill = as.factor(Id_Basket))) +
  theme(axis.text.x=element_text(color = "black", angle=30, vjust=.8, hjust=0.8)) 

ggplot(gbms_Suc12M_AllRelInf_2_dt) +
  geom_boxplot(aes(x = ImportanceVars, y = RelInf)) +
  theme(axis.text.x=element_text(color = "black", angle=30, vjust=.8, hjust=0.8)) 

ggplot(gbms_Suc12M_AllRelInf_2_dt) +
  geom_boxplot(aes(x = ImportanceVars, y = RelInf, col = as.factor(Id_Basket))) +
  theme(axis.text.x=element_text(color = "black", angle=30, vjust=.8, hjust=0.8)) +
  facet_wrap(Id_Basket~.)

rm(NTREE_gbm, SHRINK, params_1, gbms_Suc12M_ImpVars_2_dt, CE_2_dt,  gbms_Suc12_2_dt)

## MODEL

GbmModel <-  gbm(formula = Suc12M~.,
                 distribution = "bernoulli",
                 data = trnSet,
                 n.trees = params$Ntree,
                 interaction.depth = params$Depth,
                 shrinkage = params$Shrinkage,
                 n.minobsinnode = params$min.nodes,
                 bag.fraction = params$bag.fraction)

### STATYSTYKI ZBIORU TESTOWEGO



ProbPredsGbm <- predict.gbm(GbmModel, tstSet, n.trees = GbmModel$n.trees, type = "response")
RocCurve <- roc(tstSet$Suc12M, ProbPredsGbm)
Coords <- coords(RocCurve, "best", best.method = "closest.topleft")
PredGbm <- sapply(ProbPredsGbm, function(xi) ifelse(xi >= Coords$threshold, 1, 0))

CfMatrix <- table(PredGbm, tstSet$Suc12M)

TstSetStats <-data.table("VarName" = "All",
                         "ClassError" = 1 - sum(diag(CfMatrix))/sum(CfMatrix), 
                         "AUC" = RocCurve$auc,
                         "Sensitivity" = Coords$sensitivity, 
                         "Specificity" = Coords$specificity)

rm(ProbPredsGbm, RocCurve, Coords, PredGbm, CfMatrix)


PerPortfel <- lapply(1:20, function(i){
  ProbPredsGbm <- predict.gbm(GbmModel, casesFullFactors[ids == 1 & IdPortfolio==i, ], n.trees = GbmModel$n.trees, type = "response")
  RocCurve <- roc(casesFullFactors[ids == 1 & IdPortfolio==i, Suc12M], ProbPredsGbm)
  Coords <- coords(RocCurve, "best", best.method = "closest.topleft")
  PredGbm <- sapply(ProbPredsGbm, function(xi) ifelse(xi >= Coords$threshold, 1, 0))
  
  CfMatrix <- table(PredGbm, casesFullFactors[ids == 1 & IdPortfolio==i, Suc12M])
  mean(casesFullFactors[ids == 1 & IdPortfolio==1, Suc12M])Coords$threshold
  
  TstSetStats <-data.table("IdPortfela" = i,
                           "ClassError" = 1 - sum(diag(CfMatrix))/sum(CfMatrix), 
                           "AUC" = RocCurve$auc)
  
})

rbindlist(PerPortfel)

### ISTOTNOŚĆ CECH ZA POMOCĄ PERMUTACJI JEDNEJ Z CECH

features <- setdiff(names(tstSet), "Suc12M")
PermOneVarStats_dt <- rbindlist(lapply(features, VarPermutationStatsCompute, tstSet, GbmModel))

PropStats_dt <- as.data.table(sapply(2:5, function(j) PermOneVarStats_dt[, ..j]/as.numeric(TstSetStats[, ..j])))
PropStats_dt[, "VarName" := PermOneVarStats_dt$VarName]
PropStats_dt[, "CEgreater" := ifelse(ClassError > 1, 1, 0)]

PropStats_dt <- PropStats_dt[order(ClassError, decreasing = TRUE)]
PropStats_dt[, "RankPermOne" := 1:19]

rm(features)
PermOneVarStats_dt <- rbind(PermOneVarStats_dt, TstSetStats)

### ISTOTNOŚĆ CECH ZA POMOCĄ SUMMARY

SummaryGbmModel_dt <- as.data.table(summary.gbm(GbmModel, plotit = FALSE))
SummaryGbmModel_dt[, "RankSumm" := 1:19]
setnames(SummaryGbmModel_dt, "var", "VarName")

OneVarStatsAll_dt <- PropStats_dt[SummaryGbmModel_dt, on = .(VarName)]

### ISTOTNOŚĆ CECH ZA POMOCĄ PERMUTACJI JEDNEGO Z KOSZYKÓW 

PermOneBasketStats_dt <- rbindlist(lapply(1:4, BasketPermutationStatsCompute, tstSet, GbmModel))

PropStatsBaskets_dt <- as.data.table(sapply(2:5, function(j) PermOneBasketStats_dt[, ..j]/as.numeric(TstSetStats[, ..j])))
PropStatsBaskets_dt[, Id_Basket := PermOneBasketStats_dt$Id_Basket]
PropStatsBaskets_dt <- PropStatsBaskets_dt[order(ClassError, decreasing = TRUE)]
PropStatsBaskets_dt[, "CEgreater" := ifelse(ClassError > 1, 1, 0)]
PropStatsBaskets_dt[, "RankBasket" := 1:4]


### ISTOTNOŚĆ KOSZYKÓW ZA POMOCĄ SUMMARY

SummaryGbmModel_Basket_dt <- as.data.table(summary.gbm(GbmModel, plotit = FALSE))
setnames(SummaryGbmModel_Basket_dt, "var", "Var")
SummaryGbmModel_Basket_dt <- SummaryGbmModel_Basket_dt[Baskets_dt, on = .(Var)]
SummaryGbmModel_Basket_dt <- SummaryGbmModel_Basket_dt[, .("MeanRelInf" = mean(rel.inf)),by = Id_Basket][order(MeanRelInf, decreasing = TRUE)]
SummaryGbmModel_Basket_dt[, "RankBasketSumm" := 1:4]

BasketStatsAll_dt <- PropStatsBaskets_dt[SummaryGbmModel_Basket_dt, on = .(Id_Basket)][order(Id_Basket)]

rm(SummaryGbmModel_Basket_dt, PropStatsBaskets_dt)

## ZAPIS DANYCH

saveRDS(BasketStatsAll_dt, "/Users/zuza/Desktop/studia/semestr7/MWM/Data_proj/BasketStatsAll_dt.RDS")
saveRDS(OneVarStatsAll_dt, "/Users/zuza/Desktop/studia/semestr7/MWM/Data_proj/OneVarStatsAll_dt.RDS")
saveRDS(PermOneVarStats_dt, "/Users/zuza/Desktop/studia/semestr7/MWM/Data_proj/PermOneVarStats_dt.RDS")
saveRDS(PermOneBasketStats_dt, "/Users/zuza/Desktop/studia/semestr7/MWM/Data_proj/PermOneBasketStats_dt.RDS")

