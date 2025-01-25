rm(list=ls())
gc()

library(data.table)
library(corrplot)

## WCZYTANIE DANYCH

load("/Users/zuza/Desktop/studia/semestr7/MWM/Data_proj/casesFullClust.RData")

## WYZNACZENIE KOSZYKÓW KORELACJI

features <- setdiff(names(casesFullClust), c("CaseId", "Suc12M", "IdPortfolio"))
CorMatr <- cor(casesFullClust[, .SD, .SDcols = features])

corrplot(CorMatr)

distMatr <- dist(CorMatr)
hdist <- hclust(distMatr)
plot(hdist)
baskets <- cutree(hdist, k = 4)

Baskets_dt <- data.table("Var" = names(baskets), "Id_Basket" = baskets)
Baskets_dt[Id_Basket == 1, ]
Baskets_dt[Id_Basket == 2, ]
Baskets_dt[Id_Basket == 3, ]
Baskets_dt[Id_Basket == 4, ]

saveRDS(Baskets_dt, "/Users/zuza/Desktop/studia/semestr7/MWM/Data_proj/Baskets_dt.RDS")

rm(distMatr, hdist, baskets, CorMatr)

## KORELACJA ZMIENNYCH Z SKUTECZNOŚCIĄ

CorSuc12M_lin <- cor(casesFullClust$Suc12M, casesFullClust[, .SD, .SDcols = features])
CorSuc12M_dt <- data.table("Var" = features, "CorLin" = as.vector(CorSuc12M_lin), "AbsCorLin" = abs(as.vector(CorSuc12M_lin)))

CorSuc12M_nonlin <- cor(casesFullClust$Suc12M, casesFullClust[, .SD, .SDcols = features], method = "spearman")
CorSuc12M_dt[, `:=`(CorSpear=as.vector(CorSuc12M_nonlin), 
                    AbsCorSpear=abs(as.vector(CorSuc12M_nonlin)))]

CorSuc12M_dt <- CorSuc12M_dt[order(AbsCorSpear, decreasing = TRUE)]
CorSuc12M_dt[, "RankSpear" := 1:19]


CorSuc12M_dt<- CorSuc12M_dt[order(AbsCorLin, decreasing = TRUE)]
CorSuc12M_dt[, "RankLin" := 1:19]

saveRDS(CorSuc12M_dt, "/Users/zuza/Desktop/studia/semestr7/MWM/Data_proj/CorSuc12M_dt.RDS")

rm(CorSuc12M_nonlin, CorSuc12M_lin, features)
