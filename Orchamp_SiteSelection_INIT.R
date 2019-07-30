
rm(list=ls())

## load required libraries
library(foreach)
library(reshape2)
library(ggplot2)
library(ggthemes)
library(plotly)
library(shiny)
library(shinyFiles)
library(shinythemes)
library(shinycssloaders)
library(DT)
library(data.table)
library(zip)
library(xlsx)

###################################################################################################################################

# DT.add.year.constraint <- data.frame()

INIT = read.xlsx(file = "ORCHAMP_gradients_INITIALISATION.xlsx", sheetIndex = 1, stringsAsFactors = F)
INIT.years = sort(unique(INIT$Yr_lancement))

sites.names = INIT$Gradient

## GET CONSTRAINT : gestionnaires
for(grp in unique(INIT$Grp_Bota))
{
  assign(x = paste0("constraint.", grp)
         , value = INIT$Gradient[which(INIT$Grp_Bota == grp)])
}

comb.sites.2 = t(combn(sites.names, 2))
comb.sites.2 = paste0(comb.sites.2[,1], "_", comb.sites.2[,2])
## GET CONSTRAINT : not together
constraint.notTogether = vector()
if (length(na.exclude(unique(INIT$Incomp))) > 0)
{
  constraint.notTogether = foreach(paire = na.exclude(unique(INIT$Incomp)), .combine = "c") %do%
  {
    site1 = INIT$Gradient[which(INIT$Code_grad == strsplit(paire, "_")[[1]][1])]
    site2 = INIT$Gradient[which(INIT$Code_grad == strsplit(paire, "_")[[1]][2])]
    return(ifelse(paste0(c(site1, site2), collapse = "_") %in% comb.sites.2
                  , paste0(c(site1, site2), collapse = "_")
                  , paste0(c(site2, site1), collapse = "_")))
  }
}
## GET CONSTRAINT : together
constraint.together = vector()
if (length(na.exclude(unique(INIT$Paires))) > 0)
{
  constraint.together = foreach(paire = na.exclude(unique(INIT$Paires)), .combine = "c") %do%
  {
    site1 = INIT$Gradient[which(INIT$Code_grad == strsplit(paire, "_")[[1]][1])]
    site2 = INIT$Gradient[which(INIT$Code_grad == strsplit(paire, "_")[[1]][2])]
    return(ifelse(paste0(c(site1, site2), collapse = "_") %in% comb.sites.2
                  , paste0(c(site1, site2), collapse = "_")
                  , paste0(c(site2, site1), collapse = "_")))  }
}

## Initialize table to store for each site : --------------------------------------------
##  last year of sampling
##  number of successive sampling
samp.INIT = list()
for(ye in as.character(INIT.years))
{
  cat(" ", ye)
  samp.sites_tab = data.frame(SITE = INIT$Gradient #[which(INIT$Yr_lancement <= ye)]
                              , LAST_YEAR = 0
                              , NB_YEAR_SUCC = 0
                              , PROB = ifelse(INIT$Yr_lancement <= ye, 1, 0)
                              , stringsAsFactors = FALSE)
  
  ## UPDATE LAST_YEAR
  for (i in 1:nrow(samp.sites_tab))
  {
    if (samp.sites_tab$SITE[i] %in% INIT$Gradient[which(INIT$Yr_lancement <= ye)])
    {
      samp.sites_tab$LAST_YEAR[i] = as.numeric(ye) - INIT$Yr_lancement[which(INIT$Gradient == samp.sites_tab$SITE[i])]
    }
  }
  
  ## UPDATE PROB
  ## Get results of previous year, if exist
  if ((as.numeric(ye) - 1) %in% names(samp.INIT))
  {
    tab = samp.INIT[[as.character(as.numeric(ye) - 1)]]
    for (i in 1:nrow(tab))
    {
      if (tab$SITE[i] %in% samp.sites_tab$SITE && tab$PROB[i] > 0)
      {
        samp.sites_tab$PROB[which(samp.sites_tab$SITE == tab$SITE[i])] = tab$PROB[i]
      }
    }
  }
  for (i in 1:nrow(samp.sites_tab))
  {
    if (samp.sites_tab$SITE[i] %in% INIT$Gradient[which(INIT$Yr_lancement == ye)])
    {
      ## Anyway, reduce probability of sampling the site next year
      samp.sites_tab$PROB[i] = samp.sites_tab$PROB[i] * (1 - 0.4) ##prob.decrease.sampThisYear
    } else
    {
      ## Increase probability of sampling next year
      ## if the site has not been sampled for more than X years
      if(samp.sites_tab$LAST_YEAR[i] > 2) ##noXYears)
      {
        samp.sites_tab$PROB[i] = samp.sites_tab$PROB[i] * (1 + 0.25) #prob.increase.sampXYears
      }
    }
  }
  samp.INIT[[ye]] = samp.sites_tab
}


## SAVE INITIALIZATION RESULTS
dir.create(paste0("ORCHAMP_INITIALISATION_", min(INIT.years), "_", max(INIT.years)))
for(ye in as.character(INIT.years))
{
  res = data.frame(YEAR = ye
                   , SITE = INIT$Gradient[which(INIT$Yr_lancement == ye)]
                   , stringsAsFactors = FALSE)
  SAV = list(SEL = res, SAMP = samp.INIT[[ye]])
  save(SAV, file = paste0("SAUVEGARDE_ANNEE_", ye, ".RData"), envir = environment())
}

