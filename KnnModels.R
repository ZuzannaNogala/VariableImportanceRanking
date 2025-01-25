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

load("/Users/zuza/Desktop/studia/semestr7/MWM/Data_proj/casesFullClust.RData")
Baskets_dt <- readRDS("/Users/zuza/Desktop/studia/semestr7/MWM/Data_proj/Baskets_dt.RDS")

casesTmp <- copy(casesFullClust)

variables <- setdiff(names(casesTmp), c("CaseId", "IdPortfolio", "Suc12M"))

for (v in variables) {
  vnrm <- paste0(v, "_nrm")
  casesTmp[, eval(vnrm):=(get(v) - min(get(v)))/(max(get(v)) - min(get(v)))]
} 

variables <- paste0(variables, "_nrm")

## DOBÓR NAJLEPSZEGO k

ids<- sample(0:2, size = nrow(casesTmp), replace = TRUE,  prob = c(0.6, 0.2, 0.2))
trnSet <- casesTmp[ids==0, .SD, .SDcols = c(variables, "Suc12M")]
vldSet <- casesTmp[ids==1, .SD, .SDcols = c(variables, "Suc12M")]
tstSet <- casesTmp[ids==2, .SD, .SDcols = c(variables, "Suc12M")]

ks <- c(10, 50, seq(100,1000, by = 100))
  
ModelsKnnSuc12M <- mclapply(ks, function(k){

    KnnModel <- knn(
      train=trnSet[, .SD, .SDcols = setdiff(variables, "Suc12M")],
      test=vldSet[, .SD, .SDcols = setdiff(variables, "Suc12M")],
      cl=trnSet$Suc12M, # train
      k=k, algorithm="kd_tree")
    
    CfMatrix <- table(KnnModel[1:nrow(vldSet)], vldSet$Suc12M)
    ClassErr <- 1 - sum(diag(CfMatrix))/sum(CfMatrix)
    
    Complexity <- 1/k
    
    data.table("K" = k,
               'Complex' = Complexity, 
               "ClassError" = ClassErr)
    
}, mc.cores = 3)

ModelsKnnSuc12M_dt <- rbindlist(ModelsKnnSuc12M)

ModelsKnnSuc12M_dt <- readRDS("/Users/zuza/Desktop/studia/semestr7/MWM/Data_proj/ModelsKnnSuc12M_dt.RDS")
CE_dt <- ModelsKnnSuc12M_dt[K %in% ks, ][order(ClassError)]
best_k <- CE_dt[1, K]

### NAJLEPSZY MODEL

KnnModel <- knn(
  train=trnSet[, .SD, .SDcols = setdiff(variables, "Suc12M")],
  test=tstSet[, .SD, .SDcols = setdiff(variables, "Suc12M")],
  cl=trnSet$Suc12M, # train
  k=best_k, algorithm="kd_tree")

CfMatrix <- table(KnnModel[1:nrow(tstSet)], tstSet$Suc12M)
ClassErr <- 1 - sum(diag(CfMatrix))/sum(CfMatrix)

### MODELE KNN BEZ JEDNEJ CECHY

ModelsKnnSuc12M_WithoutOneVar <- mclapply(variables, function(VarName){
  
  KnnModel <- knn(
    train=trnSet[, .SD, .SDcols = setdiff(variables, c("Suc12M", VarName))],
    test=tstSet[, .SD, .SDcols = setdiff(variables, c("Suc12M", VarName))],
    cl=trnSet$Suc12M, # train
    k=best_k, algorithm="kd_tree")
  
  CfMatrix <- table(KnnModel[1:nrow(tstSet)], tstSet$Suc12M)
  ClassErr <- 1 - sum(diag(CfMatrix))/sum(CfMatrix)
  
  data.table("VarName" = VarName,
             "ClassError" = ClassErr)
  
}, mc.cores = 3)

ModelsKnnSuc12M_WithoutOneVar_dt <- rbindlist(ModelsKnnSuc12M_WithoutOneVar)
saveRDS(ModelsKnnSuc12M_WithoutOneVar,  "/Users/zuza/Desktop/studia/semestr7/MWM/Data_proj/ModelsKnnSuc12M_WithoutOneVar_dt.RDS")
ModelsKnnSuc12M_WithoutOneVar_dt <- ModelsKnnSuc12M_WithoutOneVar_dt[, .(VarName, PropOfErr = ClassError/ClassErr)][order(PropOfErr, decreasing = TRUE)]
ModelsKnnSuc12M_WithoutOneVar_dt[, RankOneVar := 1:19]

### MODELE KNN BEZ CECH Z JEDNEGO KOSZYKA

ModelsKnnSuc12M_WithoutOneBasket <- mclapply(1:4, function(id_Basket){
  
  features <- Baskets_dt[Id_Basket == id_Basket, Var]
  features <- paste0(Baskets_dt[Id_Basket == id_Basket, Var], "_nrm")
  
  KnnModel <- knn(
    train=trnSet[, .SD, .SDcols = setdiff(variables, c("Suc12M", features))],
    test=tstSet[, .SD, .SDcols = setdiff(variables, c("Suc12M", features))],
    cl=trnSet$Suc12M, # train
    k=best_k, algorithm="kd_tree")
  
  CfMatrix <- table(KnnModel[1:nrow(tstSet)], tstSet$Suc12M)
  ClassErr <- 1 - sum(diag(CfMatrix))/sum(CfMatrix)
  
  data.table("Id_Basket" =id_Basket,
             "ClassError" = ClassErr)
  
}, mc.cores = 3)

ModelsKnnSuc12M_WithoutOneBasket_dt <- rbindlist(ModelsKnnSuc12M_WithoutOneBasket)
saveRDS(ModelsKnnSuc12M_WithoutOneBasket_dt,  "/Users/zuza/Desktop/studia/semestr7/MWM/Data_proj/ModelsKnnSuc12M_WithoutOneBasket_dt.RDS")

ModelsKnnSuc12M_WithoutOneBasket_dt <- ModelsKnnSuc12M_WithoutOneBasket_dt[, .(Id_Basket, PropOfErr = ClassError/ClassErr)][order(PropOfErr, decreasing = TRUE)]
ModelsKnnSuc12M_WithoutOneBasket_dt[, RankOneBasket := 1:4]

## ZAPIS DANYCH

ModelsKnnSuc12M_WithoutOneVar_ranked_dt <- ModelsKnnSuc12M_WithoutOneVar_dt
ModelsKnnSuc12M_WithoutOneBasket_ranked_dt <- ModelsKnnSuc12M_WithoutOneBasket_dt

saveRDS(ModelsKnnSuc12M_WithoutOneBasket_ranked_dt,  "/Users/zuza/Desktop/studia/semestr7/MWM/Data_proj/ModelsKnnSuc12M_WithoutOneBasket_ranked_dt.RDS")
saveRDS(ModelsKnnSuc12M_WithoutOneVar_ranked_dt, "/Users/zuza/Desktop/studia/semestr7/MWM/Data_proj/ModelsKnnSuc12M_WithoutOneVar_ranked_dt.RDS")
