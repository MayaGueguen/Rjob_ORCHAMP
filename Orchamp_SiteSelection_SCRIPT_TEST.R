
##' ---
##' title: R script to define ORCHAMP sampling plan
##' output:
##'   html_document:
##'     toc: true
##' ---

##' Step 1: 
##' 
##' Step 2:
##'  

rm(list=ls())

## make this document cache friendly
knitr::opts_chunk$set(cache=TRUE)

## load required libraries
library(foreach)
library(reshape2)
library(ggplot2)
library(ggthemes)
library(doParallel)

##' ###################################################################################################################################
##' # DEFINING PARAMETERS
##' ###################################################################################################################################

sites.names = c("Anterne", "Argentiere", "Armenaz", "Bonette", "Caramagne", "Chaillol", "Chamrousse",
                "Claree", "Devoluy Nord", "Devoluy Sud", "Lautaret", "Lauvitel", "Loriaz",
                "Peclod", "Plan Aiguille", "Ristolas", "Valloire", "Vanoise", "Ventoux Sud")

year.start = 2020
year.end = 2035
# samp.no_sites = 6

constraint.CBNA = c("Argentiere", "Armenaz", "Caramagne", "Chamrousse",
                    "Devoluy Nord", "Devoluy Sud", "Loriaz",
                    "Peclod", "Ristolas", "Vanoise")
constraint.PNE = c("Chaillol", "Lauvitel", "Plan Aiguille")
constraint.SAJF = c("Claree", "Lautaret", "Valloire")
constraint.CBNMED = c("Ventoux Sud", "Bonette")
constraint.no_sites_max = 3
## Chaillol : plusieurs contributeurs ?

comb.sites.2 = t(combn(sites.names, 2))
comb.sites.2 = paste0(comb.sites.2[,1], "_", comb.sites.2[,2])
constraint.notTogether = comb.sites.2[grep("Anterne", comb.sites.2)[1]]

noXYears = 3
noSuccYears = 3
prob.increase.sampXYears = 1
prob.decrease.sampThisYear = 1
prob.decrease.sampSuccYears = 1

## --------------------------------------------------------------------------
sites.no = length(sites.names)

samp.years = seq(year.start, year.end, 1)
samp.no_years = length(samp.years)

samp.sites_tab = data.frame(SITE = sites.names, LAST_YEAR = 0, NB_YEAR_SUCC = 0)


##' ###################################################################################################################################
##' # DEFINING SAMPLING FUNCTION 
##' ###################################################################################################################################

## Apply sampling function for each required year
FUN_SELECT_sites = function(ye, pool)
{
  # cat(" ", ye)
  # cat("\n 1. Sites selection...")
  sites.sel = sample(x = pool$COMB[which(pool$AVAIL == 1)]
                     , size = 1
                     , prob = pool$PROB[which(pool$AVAIL == 1)])
  sites = strsplit(as.character(sites.sel), "_")[[1]]
  
  ## If first year of sampling,
  if (ye == year.start)
  {
    samp.sites_tab$LAST_YEAR = ye - 1
  }
  
  ## --------------------------------------------------------------------------
  ## FOR ALL AVAILABLE SITES
  # cat("\n 2. Update of site informations...")
  for(si in samp.sites_tab$SITE)
  {
    ind_si = which(samp.sites_tab$SITE == si)
    
    ## Is the site selected this year ? ---------------------------------------
    if (si %in% sites)
    {
      ## Get all the combinations in which the site is present
      ind = grep(si, pool$COMB)
      
      ## Record this year as the last year of sampling
      samp.sites_tab$LAST_YEAR[ind_si] = ye
      
      ## Keep track of number of previous successive sampling : +1
      samp.sites_tab$NB_YEAR_SUCC[ind_si] = samp.sites_tab$NB_YEAR_SUCC[ind_si] + 1
      
      ## Reduce probability of sampling the site next year
      ## if the site has already been sampled for the last X successive years
      if (samp.sites_tab$NB_YEAR_SUCC[ind_si] >= noSuccYears)
      {
        pool$PROB[ind] = pool$PROB[ind] * prob.decrease.sampSuccYears
      } else ## Anyway, reduce probability of sampling the site next year
      {
        pool$PROB[ind] = pool$PROB[ind] * prob.decrease.sampThisYear
      }
    } else ## Otherwise -------------------------------------------------------
    {
      ## Keep track of number of previous successive sampling : 0
      samp.sites_tab$NB_YEAR_SUCC[ind_si] = 0
      
      ## Increase probability of sampling next year
      ## if the site has not been sampled for more than X years
      if((ye - samp.sites_tab$LAST_YEAR[ind_si]) > noXYears)
      {
        ind = grep(si, pool$COMB)
        pool$PROB[ind] = pool$PROB[ind] * prob.increase.sampXYears
      }
    }
  }
  
  ## --------------------------------------------------------------------------
  ## RESULTS
  res = data.frame(YEAR = ye, SITE = sites)
  if(ye < year.end)
  {
    # cat("\n 3. Removal of site combinations...")
    # combiToRemove = which(colSums(t(comb.ALL.bin[, sites])) >= samp.no_sites)
    # 
    # pool$AVAIL = 1
    # pool$AVAIL[combiToRemove] = 0
    
    res_bis = FUN_SELECT_sites(ye = ye + 1, pool = pool)
    return(list(SEL = rbind(res, res_bis$SEL), POOL = res_bis$POOL))
  } else
  {
    return(list(SEL = res, POOL = pool))
  }
}

##' ###################################################################################################################################
##' # NOTHING TO CHANGE BELOW !!!
##' ###################################################################################################################################

registerDoParallel(cores = 32)
params = expand.grid(no_sites = c(5, 6, 7)
                     , prob1 = seq(0,1,0.02)
                     , prob2 = seq(0,1,0.02)
                     , prob3 = seq(0,1,0.02))
params = params[which(params$prob2 <= params$prob3),]
# params = params[1:10,]
# i = 1
# samp.no_sites = params$no_sites[i]
# prob1 = params$prob1[i]
# prob2 = params$prob2[i]

HOP = foreach(samp.no_sites = params$no_sites
              , prob1 = params$prob1
              , prob2 = params$prob2
              , prob3 = params$prob3
              , .combine = 'rbind') %dopar%
              {
                cat("\n", prob1)
                prob.increase.sampXYears = 1 + prob1
                prob.decrease.sampThisYear = 1 - prob2
                prob.decrease.sampSuccYears = 1 - prob3
                
                
                comb.ALL = as.data.frame(t(combn(x = sites.names, m = samp.no_sites)))
                colnames(comb.ALL) = paste0("SITE_", 1:ncol(comb.ALL))
                
                ## Remove combinations for number constraints
                constraint.list = list(constraint.CBNA, constraint.PNE, constraint.SAJF)
                for(con in constraint.list)
                {
                  no_sites_inConstraint = apply(comb.ALL, 1, function(x){ sum(x %in% con)})
                  comb.ALL = comb.ALL[which(no_sites_inConstraint <= constraint.no_sites_max),]
                }
                ## Remove combinations for association constraints
                for(con in constraint.notTogether)
                {
                  con = strsplit(con, "_")[[1]]
                  no_sites_inConstraint = apply(comb.ALL, 1, function(x){ sum(x %in% con)})
                  comb.ALL = comb.ALL[which(no_sites_inConstraint < length(con)),]
                }
                
                comb.ALL.vec = apply(comb.ALL, 1, function(x) paste0(x, collapse = "_"))
                
                ## Transform combinations into binary matrix
                # comb.ALL.bin = foreach(si = sites.names, .combine = 'cbind') %do%
                # {
                #   sapply(comb.ALL.vec, function(x) length(grep(si, x)))
                # }
                # colnames(comb.ALL.bin) = sites.names
                
                ## --------------------------------------------------------------------------
                # proc.time()
                len = foreach(rep = 1:10, .combine = "rbind") %do%
                {
                  cat(" ", rep)
                  samp.sites_tab = data.frame(SITE = sites.names, LAST_YEAR = 0, NB_YEAR_SUCC = 0)
                  pool.GLOB = data.frame(COMB = comb.ALL.vec
                                         , PROB = rep(1, length(comb.ALL.vec))
                                         , AVAIL = rep(1, length(comb.ALL.vec)))
                  RES = FUN_SELECT_sites(ye = year.start, pool = pool.GLOB)
                  
                  ## Evaluate results
                  SITE_table = table(RES$SEL$SITE)
                  cond.num = (length(SITE_table) == sites.no && length(which(SITE_table >= (year.end - year.start) / 5)) == sites.no)
                  cond.freq = TRUE
                  for(ye in year.start:(year.end - 4))
                  {
                    year.window = seq(ye, ye + 4)
                    SITE_table = table(RES$SEL$SITE[which(RES$SEL$YEAR %in% year.window)])
                    cond.freq = (length(SITE_table) == sites.no && length(which(SITE_table >= 1)) == sites.no)
                  }
                  return(data.frame(REP = rep, cond.num, cond.freq))
                }
                # proc.time()
                return(data.frame(prob1,prob2,prob3,len))
              }
save(HOP, file="HOP_b")

# load("~/HOP2")
sum(HOP$cond.num)
sum(HOP$cond.freq)
length(which(HOP$cond.num == TRUE & HOP$cond.freq == TRUE))
