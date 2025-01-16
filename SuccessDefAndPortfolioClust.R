rm(list=ls())
gc()

library(data.table)
library(cluster)
library(corrplot)
library(factoextra)

## WCZYTANIE DANYCH 

load("/Users/zuza/Desktop/studia/semestr7/MWM/Data_proj/casesFull.RData")
load("/Users/zuza/Desktop/studia/semestr7/MWM/Data_proj/eventsImpFull.RData")

sum(is.na(casesFull))
sum(is.na(eventsImpFull[, .(NumberOfPayment, PaymentAmount)]))

## ZDEFINIOWANIE SKUTECZNOŚCI Z 12 MIESIĘCY

StatsPerCase <- casesFull[, .(CaseId, TOA)][eventsImpFull[, .(CaseId, Month, PaymentAmount, NumberOfPayment)]]

SummStatsPerCase12M <- StatsPerCase[Month <= 12, .(SumPayments = sum(PaymentAmount), SumNumPayments = sum(NumberOfPayment)), by =.(CaseId, TOA)]
Suc12M <- SummStatsPerCase12M[, .(Suc1 = ifelse(SumPayments / TOA >= 0.05, 1, 0),
                                  Suc2 = ifelse(SumNumPayments >= 2, 1, 0)), 
                              by = CaseId] 

Suc12M[, Suc12M := ifelse(Suc2 > Suc1, Suc2, Suc1)]
Suc12M[, `:=`(Suc1 = NULL, Suc2 = NULL)]

casesFull<- casesFull[Suc12M]

rm(StatsPerCase, Suc12M, SummStatsPerCase12M)

## PODZIAŁ SPRAW NA PORTFOLIA

corrplot(cor(casesFull))

variables <- setdiff(names(casesFull), c("CaseId", "Suc12M"))

for (v in variables) {
  vnrm <- paste0(v, "_nrm")
  casesFull[, eval(vnrm):=(get(v) - min(get(v)))/(max(get(v)) - min(get(v)))]
} 

variables <- paste0(variables, "_nrm")

clusters <- clara(casesFull[, .SD, .SDcols = variables],
                  k = 20, samples = 100, metric = "euclidean", sampsize = 500)

clusters
hist(clusters$clustering)

banks <- c(
  "PKO","Santander", "mBank","ING",
  "Millennium","BNP_Paribas","Alior",
  "BOS","Pocztowy","Getin_Noble",
  "Credit_Agricole","Nest_Bank",
  "Plus_Bank","Citi_Handlowy",
  "SGB_Bank","BPS","DNB",
  "HSBC","ANZ","Deutsche_Bank"
)

Portfoliodt <- data.table("Id" = 1:20, "Name" = banks)

casesFull[, "IdPortfolio" := clusters$clustering]

## USUNIĘCIE NIEPOTRZEBNYCH ZMIENNYCH

for(v in variables){
  casesFull[, eval(v) := NULL]
}

names(casesFull)

rm(clusters, banks, v, variables, vnrm)

## ZAPISANIE DANYCH

casesFullClust <- copy(casesFull)

save(casesFullClust, file = "/Users/zuza/Desktop/studia/semestr7/MWM/Data_proj/casesFullClust.RData")
save(Portfoliodt,  file = "/Users/zuza/Desktop/studia/semestr7/MWM/Data_proj/PortfolioNamesdt.RData")

