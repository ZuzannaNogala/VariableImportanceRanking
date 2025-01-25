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

#  0.2798903

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
  geom_point(aes(x = reorder(Var, RankLin, decreasing = TRUE), y = AbsCorLin))+
  xlab("Zmienne objaśniające") +
  ylab("Miejsce w rankingu") +
  ggtitle("Korelacja Pearsona - wartość bezwzg") +
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
  geom_bar(aes(x = reorder(Var, RankSpear, decreasing = TRUE), y = as.factor(RankLin)), 
           stat = "identity", 
           width = 0.5, 
           fill = "steelblue") +
  xlab("Zmienne objaśniające") +
  ylab("Miejsce w rankingu") +
  ggtitle("Korelacja Pearsona - relacje liniowe") +
  coord_flip() +
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


### KNN - 

ggplot(KNNOneVar_dt) + 
  geom_point(aes(x = reorder(VarName, PropOfErr), y = PropOfErr)) +
  xlab("Spermutowana zmienna") +
  ylab("Wartość proporcji błędu") +
  ggtitle(expression(paste("Proporcja błędu ", frac(L^(C), L)," - ", "permutacja pojedynczej zmiennej"))) +
  theme_minimal() +
  geom_abline(slope = 0, intercept = 1, col = "red", lwd = 1) +
  coord_flip()  




ggplot(KNNOneVar_dt) + 
  geom_point(aes(x = reorder(VarName, PropOfErr), y = PropOfErr)) +
  xlab("Spermutowana zmienna") +
  ylab("Wartość proporcji błędu") +
  ggtitle("Proporcja błędu L* z L - - permutacja pojedynczej zmiennej -podział na koszyki") +
  theme_bw() +
  theme(axis.ticks.y=element_blank(),
        axis.text.y=element_blank()) +
  geom_abline(slope = 0, intercept = 1, col = "red") +
  coord_flip() + 
  facet_wrap(Id_Basket~.)


ggplot(KNNBaskets_dt)+
  geom_point(aes(x = as.factor(reorder(Id_Basket, PropOfErr)), y = PropOfErr)) +
  ylab("Wartość proporcji błędu") +
  xlab("Id Koszyka") +
  ggtitle("Proporcja błędu L* z L - Permutacja jednego z koszyków") +
  theme_minimal() +
  geom_abline(slope = 0, intercept = 1, col = "red") +
  coord_flip()


### Boosty 

ggplot(BoostOneVar_dt) + 
  geom_point(aes(x = reorder(VarName, ClassError), y = ClassError)) +
  xlab("Spermutowana zmienna") +
  ylab("Wartość proporcji błędu") +
  ggtitle("Proporcja błędu L* z L - permutacja pojedynczej zmiennej") +
  theme_minimal() +
  geom_abline(slope = 0, intercept = 1, col = "red", lwd = 1) +
  coord_flip() 


dat_text <- data.frame(
  label = paste0("PBK: ", round(BoostBaskets_dt$ClassError, 3)),
  Id_Basket   = 1:4,
  x     = BoostBaskets_dt$ClassError + 0.005,
  y     = rep(5, 4)
)


ggplot(BoostOneVar_dt) + 
  geom_point(aes(y = reorder(VarName, ClassError), x = ClassError)) +
  ylab("Spermutowana zmienna") +
  xlab("Wartość proporcji błędu") +
  ggtitle("Proporcja błędu L* z L - - permutacja pojedynczej zmiennej -podział na koszyki") +
  theme_bw() +
  theme(axis.ticks.y=element_blank(),
        axis.text.y=element_blank()) +
  geom_vline(xintercept = 1, col = "red", lwd = 1) +
  scale_x_continuous(c(0.98, 1.16)) +
  facet_wrap(Id_Basket~.) +
  geom_vline(aes(xintercept=i.ClassError), colour="steelblue", lwd = 1) +
  geom_text(data = dat_text, mapping = aes(x = x, y = y, label = label), size = 3, color = "steelblue", angle = 90, fontface="bold")



ggplot(BoostOneVar_dt) + 
  geom_point(aes(x = reorder(VarName, AUC, decreasing =TRUE), y = AUC)) +
  xlab("Spermutowana zmienna") +
  ylab("Wartość proporcji błędu") +
  ggtitle("Proporcja AUC L* z L - permutacja pojedynczej zmiennej") +
  theme_minimal() +
  geom_abline(slope = 0, intercept = 1, col = "red", lwd = 1) +
  coord_flip() 

dat_text2 <- data.frame(
  label = paste0("PAUCK: ", round(BoostBaskets_dt$AUC, 3)),
  Id_Basket   = 1:4,
  x     = BoostBaskets_dt$AUC - 0.003,
  y     = rep(5, 4)
)

ggplot(BoostOneVar_dt) + 
  geom_point(aes(y = reorder(VarName, AUC, decreasing =TRUE), x = AUC)) +
  ylab("Spermutowana zmienna") +
  xlab("Wartość proporcji błędu") +
  ggtitle("Proporcja błędu L* z L - - permutacja pojedynczej zmiennej -podział na koszyki") +
  theme_bw() +
  theme(axis.ticks.y=element_blank(),
        axis.text.y=element_blank()) +
  geom_vline(xintercept = 1, col = "red", lwd = 1) +
  scale_x_continuous(c(0.85, 1.05)) +
  facet_wrap(Id_Basket~.) +
  geom_vline(aes(xintercept=i.AUC), colour="steelblue", lwd = 1)+
  geom_text(data = dat_text2, mapping = aes(x = x, y = y, label = label), size = 2.5, color = "steelblue", angle = 90, fontface="bold")


rects <- data.frame(xstart = seq(0,30,10), xend = seq(10,40,10), col = letters[1:4])

ggplot(BoostOneVar_dt) + 
  geom_point(aes(y = reorder(VarName, rel.inf), x = rel.inf)) +
  geom_rect(data = rects, aes(xmin = xstart, xmax = xend, ymin = -Inf, ymax = Inf, fill = col), alpha = 0.4) +
  ylab("Spermutowana zmienna") +
  xlab("Rel.Inf") +
  ggtitle("VarImp - permutacja pojedynczej zmiennej") +
  scale_fill_brewer(palette = "Paired") +
  theme_bw() +
  theme(legend.position="none")

dat_text3 <- data.frame(
  label = paste0("SRI: ", round(BoostBaskets_dt$MeanRelInf, 3)),
  Id_Basket   = 1:4,
  x     = BoostBaskets_dt$MeanRelInf + 2,
  y     = rep(5, 4)
)


ggplot(BoostOneVar_dt) + 
  geom_point(aes(y = reorder(VarName, rel.inf), x = rel.inf)) +
  geom_rect(data = rects, aes(xmin = xstart, xmax = xend, ymin = -Inf, ymax = Inf, fill = col), alpha = 0.4) +
  ylab("Spermutowana zmienna") +
  xlab("Wartość Rel.Inf") +
  ggtitle("VarImp - permutacja pojedynczej zmiennej") +
  scale_fill_brewer(palette = "Paired") +
  theme_bw() +
  theme(axis.ticks.y=element_blank(),
        axis.text.y=element_blank(),
        legend.position="none") +
  facet_wrap(Id_Basket~.) +
  geom_vline(data = BoostBaskets_dt, aes(xintercept=MeanRelInf), colour="darkred", lwd = 1)+
  geom_text(data = dat_text3, mapping = aes(x = x, y = y, label = label), size = 2.5, color = "darkred", angle = 90, fontface="bold")


### LOGISTYCZNY

ggplot(LogOneVarWithPvals_dt) + 
  geom_point(aes(x = reorder(VarName, ClassError), y = ClassError)) +
  xlab("Spermutowana zmienna") +
  ylab("Wartość proporcji błędu") +
  ggtitle("Proporcja błędu L* z L - permutacja pojedynczej zmiennej") +
  theme_minimal() +
  geom_abline(slope = 0, intercept = 1, col = "red", lwd = 1) +
  coord_flip() 

 
ggplot(LogOneVarWithPvals_dt) + 
  geom_point(aes(y = reorder(VarName, ClassError), x = ClassError)) +
  ylab("Spermutowana zmienna") +
  xlab("Wartość proporcji błędu") +
  ggtitle("Proporcja błędu L* z L - - permutacja pojedynczej zmiennej -podział na koszyki") +
  theme_bw() +
  theme(axis.ticks.y=element_blank(),
        axis.text.y=element_blank()) +
  geom_vline(xintercept = 1, col = "red", lwd = 1) +
  scale_x_continuous(c(0.98, 1.16)) +
  facet_wrap(Id_Basket~.)


ggplot(LogOneVarWithPvals_dt) + 
  geom_point(aes(x = reorder(VarName, AUC, decreasing = TRUE), y = AUC)) +
  xlab("Spermutowana zmienna") +
  ylab("Wartość proporcji błędu") +
  ggtitle("Proporcja błędu L* z L - permutacja pojedynczej zmiennej") +
  theme_minimal() +
  geom_abline(slope = 0, intercept = 1, col = "red", lwd = 1) +
  coord_flip() 


ggplot(LogOneVarWithPvals_dt) + 
  geom_point(aes(y = reorder(VarName, AUC, decreasing = TRUE), x = AUC)) +
  ylab("Spermutowana zmienna") +
  xlab("Wartość proporcji błędu") +
  ggtitle("Proporcja błędu L* z L - - permutacja pojedynczej zmiennej -podział na koszyki") +
  theme_bw() +
  theme(axis.ticks.y=element_blank(),
        axis.text.y=element_blank()) +
  geom_vline(xintercept = 1, col = "red", lwd = 1) +
  scale_x_continuous(c(0.98, 1.16)) +
  facet_wrap(Id_Basket~.)
