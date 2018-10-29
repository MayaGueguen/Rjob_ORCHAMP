# 
# library(RFate)
# 
# setwd("~/FATE_Bauges/")
# 
# POST_FATE.graphic_evolutionCoverage(name.simulation = "FATE_Bauges/"
#                                     , file.simulParam = "FATE_Bauges/PARAM_SIMUL/paramSimul_Graz1_CA_rcp26_TEST.txt"
#                                     , no.years = 100
#                                     , abund.fixedScale = F
#                                     , opt.no_CPU = 7)

#############################################################################################################################

library(ade4)
library(ggplot2)
library(ggthemes)
library(ggtern)
library(plotly)
library(reshape2)

setwd("~/Documents/_SCRIPTS/2018_10_Orchamp_SiteSelection/Rjob_ORCHAMP/")
load("HOP_b")
HOP$ROW = 1:nrow(HOP)
acp0 = dudi.pca(HOP[, c("prob1","prob2","prob3")], scannf = F, nf = 2)

HOP$ID = rep(paste0("ID", 1:(nrow(HOP)/10)), each = 10)
HOP = HOP[which(HOP$cond.num == TRUE & HOP$cond.freq == TRUE),]
HOP$RGB = rgb(HOP$prob1, HOP$prob2, HOP$prob3)
tab_ID = table(HOP$ID)
HOP = HOP[which(HOP$ID %in% names(tab_ID)[which(tab_ID > 1)]),]
head(HOP)

## Valeurs les plus représentées pour chaque probabilité
ggplot(HOP) +
  geom_bar(aes(prob1), position = "dodge", alpha = 0.5, fill = "red") +
  geom_bar(aes(prob2), position = "dodge", alpha = 0.5, fill = "darkgreen") +
  geom_bar(aes(prob3), position = "dodge", alpha = 0.5, fill = "darkblue") +
  labs(x = "Probabilité", y = "") +
  theme_fivethirtyeight()

## Combinaisons les plus représentées ??
plot(acp0$li[HOP$ROW,], pch = 15)
abline(h = 0, v = 0, lty = 2)


pp = ggtern(data = HOP, aes(x = prob1, y = prob2, z = prob3)) +
  # breaks_tern(n = 10) +
  scale_L_continuous(limits = c(0,1), breaks = seq(0,1,0.2)) +
  scale_R_continuous(limits = c(0,1)) +
  scale_T_continuous(limits = c(0,1)) +
  geom_crosshair_tern(data = data.frame(x = 0.5, y = 0.5, z = 0.5), aes(x,y,z)) +
  geom_density_tern() +
  geom_hex_tern() +
  # labs(x = "% increase \n after X years \n without sampling"
  #      , y = "% decrease \n after sampling \n this year"
  #      , z = "% decrease \n after 3 years \n of sampling") +
  theme(tern.panel.background = element_rect(fill = "grey90")
        , tern.axis.arrow.show = TRUE)

# ggplotly(p = pp)


# library(ade4)
# acp = dudi.pca(HOP[, c("prob1","prob2","prob3")], scannf = F, nf = 2)
# s.corcircle(acp0$co)
# scatter(acp0)

# TMP = melt(HOP[,c("REP","prob1","prob2","prob3")], id.vars = "REP")
# ggplot(TMP, aes(x = value, color = variable, group = interaction(REP, variable))) +
#   geom_density()
# 
# TMP = melt(HOP[,c("REP","prob1","prob2","prob3")], id.vars = "REP")
# ggplot(TMP, aes(x = value, fill = variable, group = interaction(REP, variable))) +
#   geom_bar(position = "dodge")


# HOP = melt(HOP, id.vars = c("REP", "cond.num", "cond.freq"))
# head(HOP)
# 
# ggplot(HOP, aes(x = variable, y = value, fill = interaction(cond.num, cond.freq))) +
#   geom_boxplot()
# 
# ###
# load("HOP_b")
# head(HOP)
# 
# TMP = HOP[which(HOP$REP == 1 & HOP$prob3 == 0.5),]
# TMP = HOP[which(HOP$cond.num == TRUE & HOPcond.freq == TRUE),]
# ggplot(TMP, aes(x = prob1, y = prob2, color = cond.num)) +
#   geom_point()
# 
# ### 
# load("HOP_b")
# HOP = melt(HOP, id.vars = c("REP", "cond.num", "cond.freq"))
# TMP = HOP[which(HOP$cond.num == TRUE & HOP$cond.freq == TRUE),]
# head(TMP)
# 
# ggplot(TMP, aes(x = value, color = variable, group = interaction(variable, factor(REP)))) +
#   geom_density()
# 
# ggplot(TMP, aes(x = value, fill = variable)) + #, group = interaction(variable, factor(REP)))) +
#   geom_bar(position= "dodge")
# 
# ###
# load("HOP_b")
# HOP$ID = rep(paste0("ID", 1:(nrow(HOP)/10)), each = 10)
# HOP = HOP[which(HOP$cond.num == TRUE & HOP$cond.freq == TRUE),]
# HOP$RGB = rgb(HOP$prob1, HOP$prob2, HOP$prob3)
# head(HOP)
# 
# ggplot(HOP, aes(x = prob1, y = prob2, color = RGB)) +
#   scale_color_identity() +
#   geom_point()
# 
# ggplot(HOP, aes(x = prob1, y = prob3, color = RGB)) +
#   scale_color_identity() +
#   geom_point()
# 
# ggplot(HOP, aes(x = prob1, y = prob2, size = prob3, color = prob3)) +
#   scale_size_continuous(range=c(0.5,5)) +
#   geom_point(alpha = 0.5)
# 
# ggplot(HOP, aes(x = prob1, y = prob3, size = prob2, color = prob2)) +
#   scale_size_continuous(range=c(0.5,5)) +
#   geom_point(alpha = 0.5)
# 
# 
# HOP$CUT = cut(HOP$prob1, breaks = seq(0,1,0.2), include.lowest = T)
# TMP = melt(HOP[, c("CUT","prob1", "prob2", "prob3")], id.vars = "CUT")
# head(TMP)
# 
# ggplot(TMP, aes(x=CUT, y = value, fill = variable)) +
#   geom_boxplot(varwidth = TRUE)
# 
# # ggplot(HOP) +
# #   geom_boxplot(aes(x = CUT, y = prob2), fill = "red") +
# #   geom_boxplot(aes(x = CUT, y = prob3), fill = "blue") 
#   # geom_density(aes(x = prob1, group = REP), color = "red") +
#   # geom_density(aes(x = prob2, group = REP), color = "blue") +
#   # geom_density(aes(x = prob3, group = REP), color = "green")
