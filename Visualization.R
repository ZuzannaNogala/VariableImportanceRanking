rm(list=ls())
gc()

library(data.table)
library(ggplot2)

## DANE

Baskets_dt <- readRDS("/Users/zuza/Desktop/studia/semestr7/MWM/Data_proj/Baskets_dt.RDS")

### KORELACJA:

LinSpearCorToSuc12M_dt <- readRDS("/Users/zuza/Desktop/studia/semestr7/MWM/Data_proj/CorSuc12M_dt.RDS")
LinSpearCorToSuc12M_dt <- LinSpearCorToSuc12M_dt[Baskets_dt, on=.(Var)]

### KNN

KNNBaskets_dt <- readRDS("/Users/zuza/Desktop/studia/semestr7/MWM/Data_proj/ModelsKnnSuc12M_WithoutOneBasket_ranked_dt.RDS")
KNNOneVar_dt <- readRDS("/Users/zuza/Desktop/studia/semestr7/MWM/Data_proj/ModelsKnnSuc12M_WithoutOneVar_ranked_dt .RDS")

KNNOneVar_dt[, VarName := gsub('.{4}$', '', KNNOneVar_dt$VarName)]
KNNOneVar_dt <- KNNOneVar_dt[Baskets_dt, on=.(VarName==Var)]
KNNBaskets_dt <- KNNBaskets_dt[order(Id_Basket)]

### BOOSTING

BoostBaskets_dt <- readRDS("/Users/zuza/Desktop/studia/semestr7/MWM/Data_proj/BasketStatsAll_dt.RDS")
BoostOneVar_dt <- readRDS("/Users/zuza/Desktop/studia/semestr7/MWM/Data_proj/OneVarStatsAll_dt.RDS")
BoostOneVarStats_dt <- readRDS("/Users/zuza/Desktop/studia/semestr7/MWM/Data_proj/PermOneVarStats_dt.RDS")
BoostBasketsStats_dt <- readRDS("/Users/zuza/Desktop/studia/semestr7/MWM/Data_proj/PermOneBasketStats_dt.RDS")

BoostOneVar_dt <- BoostOneVar_dt[Baskets_dt, on=.(VarName==Var)]
BoostOneVar_dt <- BoostOneVar_dt[BoostBaskets_dt[, .(Id_Basket, ClassError, AUC)], on =.(Id_Basket)]


### MODEL LOGISTYCZNY:

LogBaskets_dt <- readRDS("/Users/zuza/Desktop/studia/semestr7/MWM/Data_proj/LogBasketsStats_dt.RDS")
LogOneVarStats_dt <- readRDS("/Users/zuza/Desktop/studia/semestr7/MWM/Data_proj/LogPermOneVarStats_dt.RDS")
LogBasketsStats_dt <- readRDS("/Users/zuza/Desktop/studia/semestr7/MWM/Data_proj/PermOneBasketStats_dt.RDS")
LogOneVarWithPvals_dt <- readRDS("/Users/zuza/Desktop/studia/semestr7/MWM/Data_proj/LogPropStats_dt.RDS")


### RANKING ZMIENNYCH

BoostOneVar_dt <- BoostOneVar_dt[order(AUC)]
BoostOneVar_dt[, RankAUC := 1:19]

RankDT <- LinSpearCorToSuc12M_dt[, .(Var, RankLin, RankSpear)][KNNOneVar_dt[, .(VarName, RankOneVar)], on = .(Var==VarName)]
RankDT <- RankDT[BoostOneVar_dt[, .(VarName, RankPermOne, RankSumm, RankAUC)], on = .(Var==VarName)]
RankDT <- RankDT[LogOneVarWithPvals_dt[, .(VarName, RankPermOne, RankPermOneAUC, RankPvals)], on = .(Var==VarName)]

setnames(RankDT, names(RankDT), c("VarName","RankLin", "RankSpear", "KNNCE", "BoostCE",
                                  "BoostRelInf", "BoostAUC", "LogitCE", "LogitAUC", "LogitPvals"))

RankDT[, AvgPlace := rowMeans(RankDT[, .SD, .SDcols = setdiff(names(RankDT), "VarName")])]
RankDT <- RankDT[order(AvgPlace)]
RankDT[, Place := 1:19]
RankDT <- RankDT[Baskets_dt, on = .(VarName=Var)]

setkey(, "VarName")
meltedRank_dt <- melt(RankDT[, .SD, .SDcols = setdiff(names(RankDT), c("AvgPlace", "Place"))])

ggplot(RankDT) +
  geom_bar(aes(y = reorder(VarName, AvgPlace, decreasing = TRUE), x = AvgPlace), stat = "identity", 
           width = 0.5,
           fill = "steelblue") +
  ggtitle("Średnie miejsce w rankingu - pojedyncza zmienna") +
  ylab("Zmienne objaśniające") +
  xlab("Miejsce w rankingu") +
  theme_minimal() 


ggplot(RankDT) +
  geom_bar(aes(x = reorder(VarName, Place, decreasing = TRUE), y = as.factor(Place)), stat = "identity", 
           width = 0.5,
           fill = "steelblue") +
  theme_minimal() +
  coord_flip()


### RANKING KOSZYKÓW

BoostBaskets_dt <- BoostBaskets_dt[order(AUC)]
BoostBaskets_dt[, RankAUC := 1:4]

RankBasketDT <- KNNBaskets_dt[, .(Id_Basket, RankOneBasket)][BoostBaskets_dt[, .(Id_Basket, RankBasket, RankBasketSumm, RankAUC)], on =.(Id_Basket)]
RankBasketDT <- RankBasketDT[LogBaskets_dt[, .(Id_Basket, RankBasket, RankAUC, RankBasketPvals)], on = .(Id_Basket)]

setnames(RankBasketDT, names(RankBasketDT), c("Id_Basket", "KNNCE", "BoostCE", "BoostAvgRelInf",
                                              "BoostAUC", "LogitCE", "LogitAvgRelInf", "LogitPvals"))

RankBasketDT[, AvgPlace := rowMeans(RankBasketDT[, .SD, .SDcols = setdiff(names(RankBasketDT), c("Id_Basket"))])]
RankBasketDT <- RankBasketDT[order(AvgPlace)]
RankBasketDT[, Place := 1:4]

ggplot(RankBasketDT) +
  geom_bar(aes(x = reorder(Id_Basket, Place, decreasing = TRUE), y = as.factor(Place)), stat = "identity", 
           width = 0.5,
           fill = "steelblue") +
  theme_minimal() +
  coord_flip()

### KORELACJE

ggplot(LinSpearCorToSuc12M_dt) +
  geom_bar(aes(x = reorder(Var, RankSpear, decreasing = TRUE), y = as.factor(RankSpear)), 
           stat = "identity", 
           width = 0.5,
           fill = "steelblue") +
  xlab("Zmienne objaśniające") +
  ylab("Miejsce w rankingu") +
  ggtitle("Korelacja Spearsmana - relacje monotoniczne") +
  coord_flip() +
  theme_minimal()


ggplot(LinSpearCorToSuc12M_dt) +
  geom_bar(aes(x = reorder(Var, Id_Basket, decreasing = TRUE), y = as.factor(RankSpear), fill = as.factor(Id_Basket)), 
           stat = "identity", 
           width = 0.5)+
           #fill = "steelblue") +
  xlab("Zmienne objaśniające") +
  ylab("Miejsce w rankingu") +
  ggtitle("Koszyki - Korelacja Spearsmana") +
  coord_flip() +
  
  theme_minimal()


ggplot(LinSpearCorToSuc12M_dt) +
  geom_bar(aes(y = reorder(Var, RankLin, decreasing = TRUE), x =RankSpear), 
           stat = "identity", 
           width = 0.5,
           fill = "steelblue") +
  geom_bar(aes(y = reorder(Var, RankLin, decreasing = TRUE), x = -RankLin), 
           stat = "identity", 
           width = 0.5,
           fill = "pink") +
  xlab("Zmienne objaśniające") +
  ylab("Miejsce w rankingu") +
  scale_fill_manual(labels = c("Spearman", "Pearson"), values = c("steelblue", "pink")) +
  labs(fill = "Korelacja") +
  ggtitle("Korelacja Pearsona i Spearmana - porównanie") +
  #coord_flip() +
  theme_minimal()

ggplot(LinSpearCorToSuc12M_dt) +
  geom_bar(aes(x = reorder(Var, RankLin, decreasing = TRUE), y = as.factor(RankLin)), 
           stat = "identity", 
           width = 0.5, 
           fill = "steelblue") +
  xlab("Zmienne objaśniające") +
  ylab("Miejsce w rankingu") +
  ggtitle("Pojedyncze zmienne - korelacja Pearsona") +
  coord_flip() +
  theme_minimal()


ggplot(LinSpearCorToSuc12M_dt) +
  geom_bar(aes(x = reorder(Var, Id_Basket, decreasing = TRUE), y = as.factor(RankLin), fill = as.factor(Id_Basket)), 
           stat = "identity", 
           width = 0.5)+
  #fill = "steelblue") +
  xlab("Zmienne objaśniające") +
  ylab("Miejsce w rankingu") +
  ggtitle("Koszyki - korelacja Pearsona") +
  labs(fill = "Koszyk") +
  coord_flip() +
  theme_minimal()


### KOSZYKI - w formie 4 tabelek 

baskets_names <- list(
  '1'="1 - Składowe Zadłużenia + Produkt",
  '2'="2 - Aktywność sprawy + windykacja",
  '3'="3 - Nieskorelowane",
  '4'="4 - Ankietowe"
)

basket_labeller <- function(variable, value){
  return(baskets_names[value])
}


### KNN - 

ggplot(KNNOneVar_dt) + 
  geom_point(aes(x = reorder(VarName, PropOfErr), y = PropOfErr)) +
  xlab("Usunięta zmienna") +
  ylab("Wartość proporcji błędu") +
  ggtitle(expression(paste("Proporcja błędu ", frac(L^(C), L)," - ", "usunięcie pojedynczej zmiennej"))) +
  theme_minimal() +
  geom_abline(slope = 0, intercept = 1, col = "red", lwd = 1) +
  coord_flip() +
  geom_text(x = 2.5, y = 1.007,
            label = expression(paste(frac(L^(C), L), " = 1")),
            color = "red",
            size = 3)



dat_text4 <- data.frame(
  label = paste0("Koszyk: ", round(KNNBaskets_dt$PropOfErr, 4)),
  Id_Basket   = 1:4,
  x     = KNNBaskets_dt$PropOfErr + 5.5,
  y     = KNNBaskets_dt$PropOfErr + 0.004
)

ggplot(KNNOneVar_dt) + 
  geom_point(aes(x = reorder(VarName, PropOfErr), y = PropOfErr)) +
  xlab("Usunięta zmienna") +
  ylab("Wartość proporcji błędu") +
  ggtitle(expression(paste("Proporcja błędu ", frac(L^(C), L)," - ", "usunięcie pojedynczego koszyka"))) +
  theme_bw() +
  theme(axis.ticks.y=element_blank(),
        axis.text.y=element_blank()) +
  geom_abline(slope = 0, intercept = 1, col = "red", lwd = 1) +
  coord_flip() + 
  facet_wrap(Id_Basket~., labeller = basket_labeller)

ggplot(KNNOneVar_dt) + 
  geom_point(aes(x = reorder(VarName, PropOfErr), y = PropOfErr)) +
  xlab("Usunięta zmienna") +
  ylab("Wartość proporcji błędu") +
  ggtitle(expression(paste("Proporcja błędu ", frac(L^(C), L)," - ", "usunięcie pojedynczego koszyka"))) +
  theme_bw() +
  theme(axis.ticks.y=element_blank(),
        axis.text.y=element_blank()) +
  geom_abline(slope = 0, intercept = 1, col = "red", lwd = 0.5) +
  coord_flip() + 
  facet_wrap(Id_Basket~.,  labeller = basket_labeller) +
  geom_hline(data = KNNBaskets_dt, aes(yintercept=PropOfErr), colour="steelblue", lwd = 1) +
  geom_text(data = dat_text4, mapping = aes(x = x, y = y, label = label), size = 2.5, color = "steelblue", angle = 90, fontface="bold")


### Boosty 

ggplot(BoostOneVar_dt) + 
  geom_point(aes(x = reorder(VarName, ClassError), y = ClassError)) +
  xlab("Spermutowana zmienna") +
  ylab("Wartość proporcji błędu") +
  ggtitle(expression(paste("Proporcja błędu ", frac(L^(C), L)," - ", "permutacja pojedynczej zmiennej"))) +
  theme_minimal() +
  geom_abline(slope = 0, intercept = 1, col = "red", lwd = 1) +
  coord_flip() +
  geom_text(x = 2.5, y = 1.007,
          label = expression(paste(frac(L^(C), L), " = 1")),
          color = "red",
          size = 3)


dat_text <- data.frame(
  label = paste0("Koszyk: ", round(BoostBaskets_dt$ClassError, 3)),
  Id_Basket   = 1:4,
  x     = BoostBaskets_dt$ClassError + 0.005,
  y     = rep(5, 4)
)


ggplot(BoostOneVar_dt) + 
  geom_point(aes(y = reorder(VarName, ClassError), x = ClassError)) +
  ylab("Spermutowana zmienna") +
  xlab("Wartość proporcji błędu") +
  ggtitle(expression(paste("Proporcja błędu ", frac(L^(C), L)," - ", "permutacja pojedynczego koszyka"))) +
  theme_bw() +
  theme(axis.ticks.y=element_blank(),
        axis.text.y=element_blank()) +
  geom_vline(xintercept = 1, col = "red", lwd = 1) +
  facet_wrap(Id_Basket~., labeller = basket_labeller) +
  geom_vline(aes(xintercept=i.ClassError), colour="steelblue", lwd = 1) +
  geom_text(data = dat_text, mapping = aes(x = x, y = y, label = label), size = 2.5, color = "steelblue", angle = 90, fontface="bold")


ggplot(BoostOneVar_dt) + 
  geom_point(aes(x = reorder(VarName, AUC, decreasing =TRUE), y = AUC)) +
  xlab("Spermutowana zmienna") +
  ylab("Wartość proporcji AUC") +
  ggtitle(expression(paste("Proporcja AUC  ", frac(AUC^(C), AUC)," - ", "permutacja pojedynczej zmiennej"))) +
  theme_minimal() +
  geom_abline(slope = 0, intercept = 1, col = "red", lwd = 1) +
  coord_flip() +
  geom_text(x = 18.5, y = 0.996,
            label = expression(paste(frac(AUC^(C), AUC), " = 1")),
            color = "red",
            size = 3)

dat_text2 <- data.frame(
  label = paste0("Koszyk: ", round(BoostBaskets_dt$AUC, 3)),
  Id_Basket   = 1:4,
  x     = BoostBaskets_dt$AUC - 0.003,
  y     = rep(5, 4)
)

ggplot(BoostOneVar_dt) + 
  geom_point(aes(y = reorder(VarName, AUC, decreasing =TRUE), x = AUC)) +
  ylab("Spermutowana zmienna") +
  xlab("Wartość proporcji AUC") +
  ggtitle(expression(paste("Proporcja AUC  ", frac(AUC^(C), AUC)," - ", "permutacja pojedynczego koszyka"))) +
  theme_bw() +
  theme(axis.ticks.y=element_blank(),
        axis.text.y=element_blank()) +
  geom_vline(xintercept = 1, col = "red", lwd = 1) +
  facet_wrap(Id_Basket~., labeller = basket_labeller) +
  geom_vline(aes(xintercept=i.AUC), colour="steelblue", lwd = 1)+
  geom_text(data = dat_text2, mapping = aes(x = x, y = y, label = label), size = 2.5, color = "steelblue", angle = 90, fontface="bold")


rects <- data.frame(xstart = seq(0,30,10), xend = seq(10,40,10), col = letters[1:4])

ggplot(BoostOneVar_dt) + 
  geom_point(aes(y = reorder(VarName, rel.inf), x = rel.inf)) +
  geom_rect(data = rects, aes(xmin = xstart, xmax = xend, ymin = -Inf, ymax = Inf, fill = col), alpha = 0.4) +
  ylab("Spermutowana zmienna") +
  xlab("Relative Influence") +
  ggtitle("Istotność cechy z summary") +
  scale_fill_brewer(palette = "Paired") +
  theme_bw() +
  theme(legend.position="none")

ggplot(BoostOneVar_dt) + 
  geom_point(aes(y = reorder(VarName, ClassError), x = rel.inf)) +
  geom_rect(data = rects, aes(xmin = xstart, xmax = xend, ymin = -Inf, ymax = Inf, fill = col), alpha = 0.4) +
  ylab("Spermutowana zmienna") +
  xlab("Relative Influence") +
  ggtitle(expression(paste("Istotność cechy z summary - sortowanie po proporcji błędu ", frac(L^(C), L)))) +
  scale_fill_brewer(palette = "Paired") +
  theme_bw() +
  theme(legend.position="none")

dat_text3 <- data.frame(
  label = paste0("Średnia istotność koszyka: ", round(BoostBaskets_dt$MeanRelInf, 3)),
  Id_Basket   = 1:4,
  x     = BoostBaskets_dt$MeanRelInf + 2,
  y     = rep(6, 4)
)


ggplot(BoostOneVar_dt) + 
  geom_point(aes(y = reorder(VarName, rel.inf), x = rel.inf)) +
  geom_rect(data = rects, aes(xmin = xstart, xmax = xend, ymin = -Inf, ymax = Inf, fill = col), alpha = 0.4) +
  ylab("Spermutowana zmienna") +
  xlab("Relative Influence") +
  ggtitle("Istotność koszyka z summary") +
  scale_fill_brewer(palette = "Paired") +
  theme_bw() +
  theme(axis.ticks.y=element_blank(),
        axis.text.y=element_blank(),
        legend.position="none") +
  facet_wrap(Id_Basket~.) +
  geom_vline(data = BoostBaskets_dt, aes(xintercept=MeanRelInf), colour="darkred", lwd = 1)+
  geom_text(data = dat_text3, mapping = aes(x = x, y = y, label = label), size = 2, color = "darkred", angle = 90, fontface="bold")


### LOGISTYCZNY

ggplot(LogOneVarWithPvals_dt) + 
  geom_point(aes(x = reorder(VarName, ClassError), y = ClassError)) +
  xlab("Spermutowana zmienna") +
  ylab("Wartość proporcji błędu") +
  ggtitle(expression(paste("Proporcja błędu ", frac(L^(C), L)," - ", "permutacja pojedynczej zmiennej"))) +
  theme_minimal() +
  geom_abline(slope = 0, intercept = 1, col = "red", lwd = 1) +
  coord_flip() +
  geom_text(x = 2.5, y = 1.02,
            label = expression(paste(frac(L^(C), L), " = 1")),
            color = "red",
            size = 3)

LogBaskets_dt <- LogBaskets_dt[order(Id_Basket)] 

dat_text5 <- data.frame(
  label = paste0("Koszyk: ", round(LogBaskets_dt$ClassError, 4)),
  Id_Basket   = 1:4,
  x     = LogBaskets_dt$ClassError + 4,
  y     = LogBaskets_dt$ClassError + 0.01
)

ggplot(LogOneVarWithPvals_dt) + 
  geom_point(aes(x = reorder(VarName, ClassError), y =ClassError)) +
  xlab("Spermutowana zmienna") +
  ylab("Wartość proporcji błędu") +
  ggtitle(expression(paste("Proporcja błędu ", frac(L^(C), L)," - ", "permutacja pojedynczego koszyka"))) +
  theme_bw() +
  theme(axis.ticks.y=element_blank(),
        axis.text.y=element_blank()) +
  geom_abline(slope = 0, intercept = 1, col = "red", lwd = 0.5) +
  coord_flip() + 
  facet_wrap(Id_Basket~., labeller = basket_labeller) +
  geom_hline(data =LogBaskets_dt, aes(yintercept=ClassError), colour="steelblue", lwd = 1) +
  geom_text(data = dat_text5, mapping = aes(x = x, y = y, label = label), size = 2.5, color = "steelblue", angle = 90, fontface="bold")


ggplot(LogOneVarWithPvals_dt) + 
  geom_point(aes(x = reorder(VarName, AUC, decreasing = TRUE), y = AUC)) +
  xlab("Spermutowana zmienna") +
  ylab("Wartość proporcji AUC") +
  ggtitle(expression(paste("Proporcja AUC  ", frac(AUC^(C), AUC)," - ", "permutacja pojedynczej cechy"))) +
  theme_minimal() +
  geom_abline(slope = 0, intercept = 1, col = "red", lwd = 1) +
  coord_flip() +
  geom_text(x = 18.5, y = 0.985,
            label = expression(paste(frac(AUC^(C), AUC), " = 1")),
            color = "red",
            size = 3)

dat_text6 <- data.frame(
  label = paste0("Koszyk: ", round(LogBaskets_dt$AUC, 4)),
  Id_Basket   = 1:4,
  x     = LogBaskets_dt$AUC - 0.01,
  y     = LogBaskets_dt$AUC + 5
)


ggplot(LogOneVarWithPvals_dt) + 
  geom_point(aes(y = reorder(VarName, AUC, decreasing = TRUE), x = AUC)) +
  ylab("Spermutowana zmienna") +
  xlab("Wartość proporcji AUC") +
  ggtitle(expression(paste("Proporcja AUC  ", frac(AUC^(C), AUC)," - ", "permutacja pojedynczego koszyka"))) +
  theme_bw() +
  theme(axis.ticks.y=element_blank(),
        axis.text.y=element_blank()) +
  geom_vline(xintercept = 1, col = "red", lwd = 1) +
  facet_wrap(Id_Basket~., labeller = basket_labeller) +
  geom_vline(data = LogBaskets_dt, aes(xintercept=AUC), colour="steelblue", lwd = 1) +
  geom_text(data = dat_text6, mapping = aes(x = x, y = y, label = label), size = 2.5, color = "steelblue", angle = 90, fontface="bold")


ggplot(LogOneVarWithPvals_dt) + 
  geom_point(aes(x = reorder(VarName, ClassError, decreasing =FALSE), y = Pval)) +
  xlab("Zmienna") +
  ylab("Wartość p-value") +
  ggtitle(expression(paste("Wartości p-value dla pojedynczej zmiennej"))) +
  theme_minimal() +
  geom_abline(slope = 0, intercept = 0.05, col = "red", lwd = 1) +
  coord_flip() 


ggplot(LogOneVarWithPvals_dt) + 
  geom_point(aes(x = reorder(VarName, ClassError), y = Pval)) +
  xlab("Zmienna") +
  ylab("Wartość p-value") +
  ggtitle(expression(paste("Wartośc p-value dla zmiennej - sortowanie po proporcji błędu ", frac(L^(C), L)))) +
  theme_minimal() +
  geom_abline(slope = 0, intercept = 0.05, col = "red", lwd = 1) +
  coord_flip() +
  geom_text(x = 1.5, y = 0.13,
            label = expression(paste(alpha, " = 0.05")),
            color = "red",
            size = 3)


dat_text7 <- data.frame(
  label = paste0("p-value koszyka: ", round(LogBaskets_dt$MeanPval, 4)),
  Id_Basket   = 1:4,
  x     = LogBaskets_dt$MeanPval + 5,
  y     = LogBaskets_dt$MeanPval - 0.03
)

ggplot(LogOneVarWithPvals_dt) + 
  geom_point(aes(x = reorder(VarName, Pval, decreasing = TRUE), y = Pval)) +
  xlab("Zmienna") +
  ylab("Wartość p-value") +
  ggtitle(expression(paste("Wartości p-value dla pojedynczego koszyka"))) +
  theme_bw() +
  theme(axis.ticks.y=element_blank(),
        axis.text.y=element_blank()) +
  geom_abline(slope = 0, intercept = 0.05, col = "red", lwd = 1) +
  coord_flip() +
  facet_wrap(Id_Basket~.)



