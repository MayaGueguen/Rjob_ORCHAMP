
rm(list = ls())

## load required libraries
library(foreach)
library(reshape2)
library(data.table)
library(xlsx)

#############################################################################################################

## READ FILE containing ORCHAMP site informations about initialisation
INIT = read.xlsx(file = "ORCHAMP_gradients_INITIALISATION.xlsx"
                 , sheetIndex = 1, stringsAsFactors = FALSE, encoding = "UTF-8")
INIT.years = sort(unique(INIT$Yr_lancement))

## DEFINE RULES to select sites
prob.increase.sampXYears = 1 + 0.25
prob.decrease.sampThisYear = 1 - 0.4
prob.decrease.sampSuccYears = 1 - 0.6
prob.decrease.notWorking = 1 - 0.1
noXYears = 2


#############################################################################################################
## Initialize table to store for each site :
##  last year of sampling
##  number of successive sampling
##  sampling probability

samp.INIT = list()
for(ye in INIT.years)
{
  cat(" ", ye)
  
  ## INITIALIZATION
  samp.sites_tab = data.frame(SITE = INIT$Gradient
                              , LAST_YEAR = 0
                              , NB_YEAR_SUCC = 0
                              , PROB = ifelse(INIT$Yr_lancement <= ye, 1, 0)
                              , stringsAsFactors = FALSE)
  
  ## UPDATE LAST_YEAR
  for (i in 1:nrow(samp.sites_tab))
  {
    si = samp.sites_tab$SITE[i]
    if (si %in% INIT$Gradient[which(INIT$Yr_lancement <= ye)])
    {
      samp.sites_tab$LAST_YEAR[i] = as.numeric(ye) - INIT$Yr_lancement[which(INIT$Gradient == si)]
    }
  }
  
  ## Get probabilities of previous year, if exist
  if ((as.numeric(ye) - 1) %in% names(samp.INIT))
  {
    tab = samp.INIT[[as.character(as.numeric(ye) - 1)]]
    for (i in 1:nrow(tab))
    {
      if ((tab$SITE[i] %in% samp.sites_tab$SITE) && (tab$PROB[i] > 0))
      {
        samp.sites_tab$PROB[which(samp.sites_tab$SITE == tab$SITE[i])] = tab$PROB[i]
      }
    }
  }

  ## UPDATE PROB
  for (i in 1:nrow(samp.sites_tab))
  {
    if (samp.sites_tab$SITE[i] %in% INIT$Gradient[which(INIT$Yr_lancement == ye)])
    {
      ## Anyway, reduce probability of sampling the site next year
      samp.sites_tab$PROB[i] = samp.sites_tab$PROB[i] * prob.decrease.sampThisYear
    } else
    {
      ## Increase probability of sampling next year
      ## if the site has not been sampled for more than X years
      if(samp.sites_tab$LAST_YEAR[i] > noXYears)
      {
        samp.sites_tab$PROB[i] = samp.sites_tab$PROB[i] * prob.increase.sampXYears
      }
    }
  }
  samp.INIT[[as.character(ye)]] = samp.sites_tab
}


## SAVE INITIALIZATION RESULTS
dir.save = paste0("ORCHAMP_INITIALISATION_", min(INIT.years), "_", max(INIT.years))
dir.create(dir.save)
for(ye in as.character(INIT.years))
{
  res = data.frame(YEAR = ye
                   , SITE = INIT$Gradient[which(INIT$Yr_lancement == ye)]
                   , stringsAsFactors = FALSE)
  SAV = list(SEL = res, SAMP = samp.INIT[[ye]])
  save(SAV, file = paste0(dir.save, "/SAUVEGARDE_ANNEE_", ye, ".RData"), envir = environment())
}

