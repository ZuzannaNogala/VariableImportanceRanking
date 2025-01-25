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

str(casesFull)

## ZDEFINIOWANIE SKUTECZNOŚCI Z 12 MIESIĘCY

StatsPerCase <- casesFull[, .(CaseId, TOA)][eventsImpFull[, .(CaseId, Month, PaymentAmount, NumberOfPayment)]]
SummStatsPerCase12M <- StatsPerCase[, .(SumPayments = sum(PaymentAmount), SumNumPayments = sum(NumberOfPayment)), by =.(CaseId, TOA)]

Suc12M <- SummStatsPerCase12M[, .(Suc1 = ifelse(SumPayments / TOA >= 0.05, 1, 0),
                                  Suc2 = ifelse(SumNumPayments >= 2, 1, 0)), 
                              by = CaseId] 

table("suc1" = Suc12M$Suc1, "suc2" = Suc12M$Suc2)
sum(Suc12M$Suc1)
sum(Suc12M$Suc2)

Suc12M[, Suc12M := ifelse(Suc2 > Suc1, Suc2, Suc1)]
Suc12M[, `:=`(Suc1 = NULL, Suc2 = NULL)]

casesFull<- casesFull[Suc12M]

rm(Suc12M, SummStatsPerCase12M)

## PODZIAŁ SPRAW NA PORTFOLIA

corrplot(cor(casesFull[, .SD,.SDcols = setdiff(names(casesFull), "CaseId")]), tl.cex = 0.7)

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

## SR na Potftel

StatsPerCase <- StatsPerCase[casesFullClust[, .(CaseId, IdPortfolio, Suc12M)]]
StatsPerCase <- StatsPerCase[, .(IdPortfolio, 
                                 SumPayments = sum(PaymentAmount), 
                                 SumNumPayments = sum(NumberOfPayment), 
                                 Suc12M),
                             by =.(CaseId, TOA)]
summary(StatsPerCase)


StatsPerCaseSuc <- StatsPerCase[, .(Suc1 = sum(SumPayments)/sum(TOA) >= 0.05,
                                    Suc2 = mean(SumNumPayments) > 2), by = IdPortfolio]

StatsPerCaseSuc[, SucPerPorfolio := ifelse(Suc2 > Suc1, Suc2, Suc1)]
StatsPerCaseSuc[, `:=`(Suc1=NULL, Suc2=NULL)]
StatsPerCaseSuc[, SucPerPorfolio := as.integer(SucPerPorfolio)]

## ZAPISANIE DANYCH

casesFullClust <- copy(casesFull)

StatsPerCaseSuc
save(casesFullClust, file = "/Users/zuza/Desktop/studia/semestr7/MWM/Data_proj/casesFullClust.RData")
save(Portfoliodt,  file = "/Users/zuza/Desktop/studia/semestr7/MWM/Data_proj/PortfolioNamesdt.RData")

