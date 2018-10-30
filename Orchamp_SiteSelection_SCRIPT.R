
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

##' ###################################################################################################################################
##' # DEFINING PARAMETERS
##' ###################################################################################################################################

sites.names = c("Anterne", "Argentiere", "Armenaz", "Bonette", "Caramagne", "Chaillol", "Chamrousse",
                "Claree", "Devoluy Nord", "Devoluy Sud", "Lautaret", "Lauvitel", "Loriaz",
                "Peclod", "Plan Aiguille", "Ristolas", "Valloire", "Vanoise", "Ventoux Sud")

year.start = 2020
year.end = 2050
samp.no_sites = 6

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


## --------------------------------------------------------------------------
## NOTHING TO CHANGE BELOW !!!
sites.no = length(sites.names)

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

##' ###################################################################################################################################
##' # DEFINING SAMPLING FUNCTION 
##' ###################################################################################################################################

# SEL_SITE = function(ye, pool)
# {
#   sites.sel = sample(x = pool, size = 1)
#   sites = strsplit(as.character(sites.sel), "_")[[1]]
#   res = data.frame(YEAR = ye, SITE = sites)
#   
#   if (ye < year.end)
#   {
#     tmp = foreach(mm = 4:samp.no_sites) %do%
#     {
#       combi = as.data.frame(t(combn(x = sites, m = 3)))
#       colnames(combi) = paste0("SITE_", 1:ncol(combi))
#       inds = apply(combi, 1, function(x) {
#         inds = sapply(x, function(y) grep(y, comb.ALL.vec))
#         ind_inter = intersect(inds[[1]], inds[[2]])
#         ind_inter = intersect(ind_inter, inds[[3]])
#         return(ind_inter)
#       })
#       return(unique(unlist(inds)))
#     }
#     tmp = unique(unlist(tmp))
#     pool_bis = comb.ALL.vec[-tmp]
#     
#     return(rbind(res,SEL_SITE(ye + 1, pool = pool_bis)))
#   } else 
#   {
#     return(res)
#   }
# }
# 
# SEL_SITE(ye = year.end, pool = comb.ALL.vec)
# SEL_SITE(ye = year.end - 1, pool = comb.ALL.vec)
# RES = SEL_SITE(ye = year.start, pool = comb.ALL.vec)
# 
# barplot(table(RES$SITE), las = 2)
# abline(h = 5, lty = 2)
# 
# ggplot(RES, aes(YEAR, fill = SITE)) +
#   scale_fill_discrete("") +
#   geom_density(alpha = 0.1) +
#   theme_fivethirtyeight()

##' ###################################################################################################################################
##' # DEFINING SAMPLING FUNCTION 
##' ###################################################################################################################################

## Apply sampling function for each required year
FUN_SELECT_sites = function(ye, pool, samp)
{
  cat("\n ######################", ye, "######################\n")
  # cat("\n 1. Sites selection...")
  print(head(pool))
  
  if (!file.exists(paste0("SAUVEGARDE_ANNEE_", ye)))
  {
    sites.sel = sample(x = pool$COMB[which(pool$AVAIL == 1)]
                       , size = 1
                       , prob = pool$PROB[which(pool$AVAIL == 1)])
    sites = strsplit(as.character(sites.sel), "_")[[1]]
    
    ## --------------------------------------------------------------------------
    ## FOR ALL AVAILABLE SITES
    # cat("\n 2. Update of site informations...")
    for(si in samp$SITE)
    {
      ind_si = which(samp$SITE == si)
      
      ## Is the site selected this year ? ---------------------------------------
      if (si %in% sites)
      {
        ## Get all the combinations in which the site is present
        ind = grep(si, pool.GLOB$COMB)
        
        ## Keep track of number of previous successive sampling : +1
        samp$NB_YEAR_SUCC[ind_si] = samp$NB_YEAR_SUCC[ind_si] + 1
        
        ## Reduce probability of sampling the site next year
        ## if the site has already been sampled for the last X successive years
        if (samp$NB_YEAR_SUCC[ind_si] >= noSuccYears)
        {
          pool$PROB[ind] = pool$PROB[ind] * prob.decrease.sampSuccYears
        } else ## Anyway, reduce probability of sampling the site next year
        {
          pool$PROB[ind] = pool$PROB[ind] * prob.decrease.sampThisYear
        }
      } else ## Otherwise -------------------------------------------------------
      {
        ## Keep track of last year of sampling : +1
        samp$LAST_YEAR[ind_si] = samp$LAST_YEAR[ind_si] + 1
        
        ## Keep track of number of previous successive sampling : 0
        samp$NB_YEAR_SUCC[ind_si] = 0
        
        ## Increase probability of sampling next year
        ## if the site has not been sampled for more than X years
        if(samp$LAST_YEAR[ind_si] > noXYears)
        {
          ind = grep(si, pool$COMB)
          pool$PROB[ind] = pool$PROB[ind] * prob.increase.sampXYears
        }
      }
    }
    
    ## --------------------------------------------------------------------------
    ## RESULTS
    res = data.frame(YEAR = ye, SITE = sites)
    assign(paste0("sauv_annee_", ye)
           , value = list(SEL = res
                          , POOL = pool
                          , SAMP = samp))
    save(list = paste0("sauv_annee_", ye),
         file = paste0("SAUVEGARDE_ANNEE_", ye))
  } else
  {
    cat("\n Loading previous results...\n")
    SAV = get(load(paste0("SAUVEGARDE_ANNEE_", ye)))
    res = SAV$SEL
    pool = SAV$POOL
    samp = SAV$SAMP
  }
  print(head(pool))
  
  setTxtProgressBar(prog.bar, year.end - ye + 1)
  
  
  if(ye > year.start)
  {
    # cat("\n 3. Removal of site combinations...")
    ## KEPT POSSIBILITY
    # combiToRemove = which(colSums(t(comb.ALL.bin[, sites])) >= samp.no_sites)
    # pool$AVAIL = 1
    # pool$AVAIL[combiToRemove] = 0
    
    res_bis = FUN_SELECT_sites(ye = ye - 1, pool = pool, samp = samp)
    ye = ye - 1
    tmp = rbind(res, res_bis$SEL)
    
    
    ## Evaluate results 2
    cond.freq = TRUE
    if (ye >= year.start + 5)
    {
      year.window = seq(ye - 5, ye)
      cat("\n", year.window)
      SITE_table = table(tmp$SITE[which(tmp$YEAR %in% year.window)])
      print(SITE_table[order(names(SITE_table))])
      cat("\n", length(SITE_table))
      cond.freq = (length(SITE_table) == sites.no && length(which(SITE_table >= 1)) == sites.no)
    }
    ## Evaluate results 1
    cond.num = TRUE
    if (ye == year.end - 1)
    {
      SITE_table = table(tmp$SITE)
      ref = floor((year.end - year.start) / 5)
      cat("\n", length(SITE_table))
      cat("\n", length(which(SITE_table >= ref)))
      cond.num = (length(SITE_table) == sites.no && length(which(SITE_table >= ref)) == sites.no)
    }
    
    if(cond.freq && cond.num)
    {
      # assign(paste0("sauv_annee_", ye + 1)
      #        , value = list(SEL = rbind(res, res_bis$SEL)
      #                       , POOL = res_bis$POOL
      #                       , SAMP = res_bis$SAMP))
      # save(list = paste0("sauv_annee_",ye + 1), file = paste0("SAUVEGARDE_ANNEE_", ye + 1))
      return(list(SEL = rbind(res, res_bis$SEL), POOL = res_bis$POOL, SAMP = res_bis$SAMP))
    } else
    {
      cat("\n /!\\ Certaines conditions ne sont pas remplies : redémarrage du calcul /!\\ \n")
      cat("\n cond.freq ", cond.freq)
      cat("\n cond.num ", cond.num)
      cat("\n")
      # pool$PROB = 1
      # pool$AVAIL = 1
      # return(FUN_SELECT_sites(ye = year.end, pool = pool, samp = samp))
      # new_ye = ye - 5
      # if (new_ye == year)
      # SAV = get(load(paste0("SAUVEGARDE_ANNEE_", ye)))
      if (ye == year.start + 5)
      {
        sapply(paste0("SAUVEGARDE_ANNEE_", seq(year.end, year.start)), file.remove)
        # pool$PROB = 1
        # pool$AVAIL = 1
        # samp$LAST_YEAR = 0
        # samp$NB_YEAR_SUCC = 0
        # return(FUN_SELECT_sites(ye = year.end, pool = pool, samp = samp))
      } else
      {
        # sapply(paste0("SAUVEGARDE_ANNEE_", seq(year.end, ye)), file.remove)
        # return(FUN_SELECT_sites(ye = year.end, pool = res_bis$POOL, samp = res_bis$SAMP))
        sapply(seq(year.end, ye), function(x) file.remove(paste0("SAUVEGARDE_ANNEE_", x)))
        year.toKeep = seq(ye - 1, year.start)
        cat("\n YEAR TO KEEP :", year.toKeep)
        cat("\n RENAMED IN :", year.end - 1:length(year.toKeep) + 1)
        cat("\n")
        sapply(1:length(year.toKeep), function(x) file.rename(from = paste0("SAUVEGARDE_ANNEE_", year.toKeep[x])
                                                              , to = paste0("SAUVEGARDE_ANNEE_", year.end - x + 1)))
        # return(FUN_SELECT_sites(ye = year.end - length(year.toKeep)
        #                         , pool = res_bis$POOL, samp = res_bis$SAMP))
      }
      pool$PROB = 1
      pool$AVAIL = 1
      samp$LAST_YEAR = 0
      samp$NB_YEAR_SUCC = 0
      return(FUN_SELECT_sites(ye = year.end, pool = pool, samp = samp))
      
    }
  } else
  {
    return(list(SEL = res, POOL = pool, SAMP = samp))
  }
  
}

## --------------------------------------------------------------------------
## Initialize table to store for each site :
##  last year of sampling
##  number of successive sampling
samp.sites_tab = data.frame(SITE = sites.names, LAST_YEAR = 0, NB_YEAR_SUCC = 0)

## Initialize table to store each possible combination of sites
## and probability of each combination
pool.GLOB = data.frame(COMB = comb.ALL.vec
                       , PROB = rep(1, length(comb.ALL.vec))
                       , AVAIL = rep(1, length(comb.ALL.vec)))


## --------------------------------------------------------------------------

year.start = 2020
year.end = year.start + 8
samp.years = seq(year.start, year.end, 1)
samp.no_years = length(samp.years)
noXYears = 2
noSuccYears = 2
prob.increase.sampXYears = 1.25
prob.decrease.sampThisYear = 0.5
prob.decrease.sampSuccYears = 0.2

prog.bar = txtProgressBar(min = 0, max = samp.no_years, style = 3)
RES = FUN_SELECT_sites(ye = year.end, pool = pool.GLOB, samp = samp.sites_tab)

TT = foreach(ye = samp.years, .combine = "rbind") %do%
{
  sav = get(load(paste0("SAUVEGARDE_ANNEE_",ye)))
  return(sav$SEL)
}
table(TT$YEAR)
barplot(table(TT$SITE))







##' ###################################################################################################################################
##' ###################################################################################################################################




SITE_table = table(RES$SEL$SITE)
cond.num = (length(SITE_table) == sites.no && length(which(SITE_table >= 5)) == sites.no)

cond.freq = TRUE
for(ye in year.start:(year.end - 4))
{
  year.window = seq(ye, ye + 4)
  cat("\n", year.window)
  SITE_table = table(RES$SEL$SITE[which(RES$SEL$YEAR %in% year.window)])
  cond.freq = (length(SITE_table) == sites.no && length(which(SITE_table >= 1)) == sites.no)
  cat("\n",cond.freq)
}


dim(RES$SEL)
head(RES$POOL)

par(mfrow = c(1,2))
hist(RES$POOL$PROB, breaks= 1000)
barplot(table(RES$SEL$SITE), las = 2)
abline(h = 5, lty = 2)

ggplot(RES$SEL, aes(YEAR, fill = SITE)) +
  scale_fill_discrete("") +
  geom_density(alpha = 0.1) +
  theme_fivethirtyeight()


TMP = expand.grid(SITE = sites.names, YEAR = samp.years)
TMP = merge(TMP, data.frame(RES$SEL, SAMP = 1), by = c("SITE","YEAR"), all.x = T)
TMP$SAMP[which(is.na(TMP$SAMP))] = 0

TMP.split = split(TMP, TMP$SITE)
TMP.split = foreach(x = TMP.split, .combine = "rbind") %do%
{
  x$YEAR_prev = x$YEAR
  for(i in 1:nrow(x))
  {
    if(x$SAMP[i] == 0)
    {
      x$YEAR_prev[i] = max(year.start, x$YEAR_prev[i-1])
    }
  }
  x$DIFF_YEAR = c(NA, x$YEAR_prev[2:nrow(x)] - x$YEAR_prev[1:(nrow(x)-1)])
  return(x[, c("SITE", "YEAR", "DIFF_YEAR")])
}

TMP = merge(TMP, TMP.split, by = c("SITE","YEAR"))

# # colFunc = colorRampPalette(c('#fee0d2','#fcbba1','#fc9272','#fb6a4a','#ef3b2c','#cb181d','#a50f15','#67000d'))
# colFunc = colorRampPalette(c('#238b45','#41ab5d','#74c476','#a1d99b','#fee0d2','#fcbba1','#fc9272','#fb6a4a','#ef3b2c'))
# colFunc_n = colFunc(max(TMP$DIFF_YEAR, na.rm = T)+1)
# TMP$DIFF_YEAR = colFunc_n[TMP$DIFF_YEAR + 1]

ggplot(TMP, aes(YEAR, alpha = factor(SAMP))) +#, fill = DIFF_YEAR)) +
  scale_alpha_discrete(guide = F, range = c(0,1)) +
  scale_fill_identity() +
  geom_bar(width = 1) +
  facet_wrap(~SITE) +
  theme_fivethirtyeight()

# 
# # ggplot(TMP, aes(x = YEAR, y = SAMP, color = SITE)) +
# #   scale_fill_discrete("") +
# #   geom_line() +
# #   theme_fivethirtyeight()
# # 
# # ggplot(TMP, aes(x = YEAR, y = SAMP, color = SITE)) +
# #   scale_fill_discrete("") +
# #   geom_line() +
# #   theme_fivethirtyeight()
# 
# ## --------------------------------------------------------------------------
# # library(profmem)
# # library(lineprof)
# # 
# # samp.sites_tab = data.frame(SITE = sites.names, LAST_YEAR = 0, NB_YEAR_SUCC = 0)
# # pool.GLOB = data.frame(COMB = comb.ALL.vec, PROB = rep(1, length(comb.ALL.vec)))
# # # RES = profmem(FUN_SELECT_sites(ye = year.start, pool = pool.GLOB))
# # # total(RES)
# # RES = lineprof(FUN_SELECT_sites(ye = year.start, pool = pool.GLOB))
# # write.table(RES, file = "PROFMEM_ORCHAMP.txt")
# 
# 
# 
## --------------------------------------------------------------------------
