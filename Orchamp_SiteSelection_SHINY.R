
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

###################################################################################################################################

sites.names = c("Anterne", "Argentiere", "Armenaz", "Bonette", "Caramagne", "Chaillol", "Chamrousse",
                "Claree", "Devoluy Nord", "Devoluy Sud", "Lautaret", "Lauvitel", "Loriaz",
                "Peclod", "Plan Aiguille", "Ristolas", "Valloire", "Vanoise", "Ventoux Sud")

constraint.CBNA = c("Argentiere", "Armenaz", "Caramagne", "Chamrousse",
                    "Devoluy Nord", "Devoluy Sud", "Loriaz",
                    "Peclod", "Ristolas", "Vanoise")
constraint.PNE = c("Chaillol", "Lauvitel", "Plan Aiguille")
constraint.SAJF = c("Claree", "Lautaret", "Valloire")
constraint.CBNMED = c("Ventoux Sud", "Bonette")

comb.sites.2 = t(combn(sites.names, 2))
comb.sites.2 = paste0(comb.sites.2[,1], "_", comb.sites.2[,2])
constraint.notTogether = vector()


###################################################################################################################################
# DEFINING SAMPLING FUNCTION 
###################################################################################################################################

## Apply sampling function for each required year
FUN_SELECT_sites = function(ye, pool, samp, firstOK = FALSE
                            , year.start, year.end ## fixed inputs !!
                            , samp.no_sites ## fixed inputs !!
                            , prob.decrease.sampThisYear ## fixed inputs !!
                            , noSuccYears ## fixed inputs !!
                            , prob.decrease.sampSuccYears ## fixed inputs !!
                            , noXYears ## fixed inputs !!
                            , prob.increase.sampXYears ## fixed inputs !!
                            , prob.decrease.notWorking ## fixed inputs !!
                            , test.ref ## fixed inputs !!
                            , test.win ## fixed inputs !!
)
{
  # cat(" ", ye)
  if (!file.exists(paste0("SAUVEGARDE_ANNEE_", ye)))
  {
    # cat("\n 1. Sites selection...")
    sites.sel = sample(x = pool$COMB
                       , size = 1
                       , prob = pool$PROB)
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
        ind = grep(si, pool$COMB)
        
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
    SAV = list(SEL = res, POOL = pool, SAMP = samp)
    save(SAV, file = paste0("SAUVEGARDE_ANNEE_", ye), envir = environment())
  } else
  {
    # cat("\n Loading previous results...\n")
    SAV = get(load(paste0("SAUVEGARDE_ANNEE_", ye), envir = environment()))
    res = SAV$SEL
    pool = SAV$POOL
    samp = SAV$SAMP
  }
  
  if(ye > year.start)
  {
    res_bis = FUN_SELECT_sites(ye = ye - 1, pool = pool, samp = samp, firstOK = firstOK
                               , year.start = year.start
                               , year.end = year.end
                               , samp.no_sites = samp.no_sites
                               , prob.decrease.sampThisYear = prob.decrease.sampThisYear
                               , noSuccYears = noSuccYears
                               , prob.decrease.sampSuccYears = prob.decrease.sampSuccYears
                               , noXYears = noXYears
                               , prob.increase.sampXYears = prob.increase.sampXYears
                               , prob.decrease.notWorking = prob.decrease.notWorking
                               , test.ref = test.ref
                               , test.win = test.win
    )
    
    ## EVALUATION OF RESULTS
    cond.freq = cond.num = TRUE
    res_tmp = rbind(res, res_bis$SEL)
    
    ## Frequency ?
    if (ye >= year.start + (test.win - 1))
    {
      year.window = seq(ye - (test.win - 1), ye)
      # cat("\n", year.window)
      SITE_table = table(res_tmp$SITE[which(res_tmp$YEAR %in% year.window)])
      cond.freq = (length(SITE_table) == nrow(samp) && length(which(SITE_table >= 1)) == nrow(samp))
    }
    ## Total number ?
    if (ye == year.end)
    {
      SITE_table = table(res_tmp$SITE)
      cond.num = (length(SITE_table) == nrow(samp) && length(which(SITE_table >= test.ref)) == nrow(samp))
    }
    
    ## --------------------------------------------------------------------------
    if(cond.freq && cond.num)
    {
      return(list(SEL = rbind(res, res_bis$SEL), POOL = res_bis$POOL, SAMP = res_bis$SAMP))
    } else
    {
      ## No working sequence registered
      if(!firstOK)
      {
        ## First year of test : remove all files
        if (ye == year.start + (test.win - 1))
        {
          sapply(paste0("SAUVEGARDE_ANNEE_", seq(year.end, year.start)), file.remove)
        } 
      } else
      { ## Remove only last year file
        SAV = get(load(paste0("SAUVEGARDE_ANNEE_", year.start), envir = environment()))
        SAV.sites = SAV$SEL$SITE
        sapply(paste0("SAUVEGARDE_ANNEE_", year.start), file.remove)
        
        SAV = get(load(paste0("SAUVEGARDE_ANNEE_", year.start + 1), envir = environment()))
        for(si in SAV.sites)
        {
          ind = grep(si, SAV$POOL$COMB)
          SAV$POOL$PROB[ind] = SAV$POOL$PROB[ind] * prob.decrease.notWorking
        }
        save(SAV, file = paste0("SAUVEGARDE_ANNEE_", year.start + 1), envir = environment())
      }
      
      # cat("\n /!\\ Certaines conditions ne sont pas remplies : redémarrage du calcul /!\\ \n")
      pool$PROB = 1
      samp$LAST_YEAR = 0
      samp$NB_YEAR_SUCC = 0
      return(FUN_SELECT_sites(ye = year.end, pool = pool, samp = samp, firstOK = firstOK
                              , year.start = year.start
                              , year.end = year.end
                              , samp.no_sites = samp.no_sites
                              , prob.decrease.sampThisYear = prob.decrease.sampThisYear
                              , noSuccYears = noSuccYears
                              , prob.decrease.sampSuccYears = prob.decrease.sampSuccYears
                              , noXYears = noXYears
                              , prob.increase.sampXYears = prob.increase.sampXYears
                              , prob.decrease.notWorking = prob.decrease.notWorking
                              , test.ref = test.ref
                              , test.win = test.win
      ))
    }
    
  } else
  {
    return(list(SEL = res, POOL = pool, SAMP = samp))
  }
}

###################################################################################################################################

# Define UI for application that plots features of movies
ui <- fluidPage(
  
  # theme = "cosmo",
  titlePanel("ORCHAMP : sélection des sites"
             , windowTitle = "ORCHAMP"),
  
  # Sidebar layout with a input and output definitions
  sidebarLayout(
    
    # Inputs
    sidebarPanel(
      
      ## ----------------------------------------------------------
      wellPanel(
        fluidRow(
          column(8,
                 "",
                 sliderInput("year.range"
                             , label = "Années d'échantillonnage"
                             , min = 2020
                             , max = 2080
                             , value = c(2020, 2050)
                             , sep = ""
                 )
          ),
          column(4,
                 "",
                 numericInput(inputId = "samp.no_sites"
                              , label = "Nombre de sites / an"
                              , min = 5
                              , max = 7
                              , value = 6
                              , step = 1
                 )
          )
        )
      ),
      
      ## ----------------------------------------------------------
      wellPanel(
        
        h3("A. Contraintes d'échantillonnage"),
        p(em("Les sites sont attribués aux différents partenaires impliqués.
             Un nombre limite de sites à échantillonner par an et par partenaire
             est fixé au préalable.")),
        
        numericInput(inputId = "constraint.no_sites_max"
                     , label = "Nombre de sites MAX / partenaire / an"
                     , min = 2
                     , max = 5
                     , value = 3
                     , step = 1
        ),
        
        fluidRow(
          column(6,
                 "",
                 selectInput(inputId = "constraint.CBNA"
                             , label = "Sites à charge du CBNA"
                             , choices = sites.names
                             , selected = constraint.CBNA
                             , multiple = TRUE
                 )
          ),
          column(6,
                 "",
                 fluidRow(
                   column(12,
                          "",
                          selectInput(inputId = "constraint.CBNMED"
                                      , label = "Sites à charge du CBNMED"
                                      , choices = sites.names
                                      , selected = constraint.CBNMED
                                      , multiple = TRUE
                          )
                   )
                 ),
                 fluidRow(
                   column(12,
                          "",
                          selectInput(inputId = "constraint.PNE"
                                      , label = "Sites à charge du PNE"
                                      , choices = sites.names
                                      , selected = constraint.PNE
                                      , multiple = TRUE
                          )
                   )
                 ),
                 fluidRow(
                   column(12,
                          "",
                          selectInput(inputId = "constraint.SAJF"
                                      , label = "Sites à charge de la SAJF"
                                      , choices = sites.names
                                      , selected = constraint.SAJF
                                      , multiple = TRUE
                          )
                   )
                 )
          )
        )
        ),
      
      
      ## ----------------------------------------------------------
      wellPanel(
        
        h3("B. Contraintes d'association"),
        p(em("Certains sites sont similaires en termes de conditions environnementales.
             Des sites similaires ne peuvent être échantillonnés la même année.")),
        
        selectInput(inputId = "constraint.notTogether"
                    , label = "Sites à ne pas échantillonner la même année"
                    , choices = comb.sites.2
                    , selected = constraint.notTogether
                    , multiple = TRUE
        )
        ),
      
      ## ----------------------------------------------------------
      wellPanel(
        
        h3("C. Evolution des probabilités de sélection"),
        
        p(em("La probabilité de chaque site d'etre échantillonné évolue au cours du temps")),
        p(em("(i) si le site vient d'etre échantillonné,")),
        numericInput(inputId = "prob.decrease.sampThisYear"
                     , label = "Diminution après échantillonnage (%)"
                     , min = 0.4
                     , max = 0.4
                     , value = 0.4
                     , step = 0.1
        ),
        p(em("(ii) si le site a été échantillonné plusieurs années de suite, ")),
        fluidRow(
          column(7,
                 "",
                 numericInput(inputId = "noSuccYears"
                              , label = "Seuil d'années successives"
                              , min = 2
                              , max = 2
                              , value = 2
                              , step = 1
                 )
          ),
          column(5,
                 "",
                 numericInput(inputId = "prob.decrease.sampSuccYears"
                              , label = "Diminution après seuil (%)"
                              , min = 0.6
                              , max = 0.6
                              , value = 0.6
                              , step = 0.1
                 )
          )
        ),
        
        p(em("(iii) si le site n'a pas été échantillonné depuis plusieurs années,")),
        fluidRow(
          column(7,
                 "",
                 numericInput(inputId = "noXYears"
                              , label = "Seuil d'années non-échantillonnées"
                              , min = 2
                              , max = 2
                              , value = 2
                              , step = 1
                 )
          ),
          column(5,
                 "",
                 numericInput(inputId = "prob.increase.sampXYears"
                              , label = "Augmentation après seuil (%)"
                              , min = 0.25
                              , max = 0.25
                              , value = 0.25
                              , step = 0.1
                 )
          )
        ),
        
        p(em("(iv) si le site ne valide pas les conditions à long-terme.")),
        numericInput(inputId = "prob.decrease.notWorking"
                     , label = "Diminution après échantillonnage (%)"
                     , min = 0.2
                     , max = 0.2
                     , value = 0.2
                     , step = 0.1
        )
      )
        ),
    
    # Output
    mainPanel(
      fluidRow(
        column(6
               , ""
               , p(em("Démarrer à partir des résultats précédents ?"))
        ),
        column(3
               , ""
               , p(em("Sauvegarder les résultats ?"))
        ),
        column(3
               , ""
               , actionButton(inputId = "refresh"
                              , label = "Lancer calcul"
                              , icon = icon("refresh"))
        )
      ),
      
      fluidRow(
        column(6
               , ""
               , checkboxInput(inputId = "startFromSave"
                               , label = "oui"
                               , value = TRUE)
        ),
        column(3
               , ""
               , checkboxInput(inputId = "saveResults"
                               , label = "oui"
                               , value = TRUE)
        ),
        column(3
               , ""
               , uiOutput("zip_selector")
        )
      ),
      
      fluidRow(
        column(6
               , ""
               , uiOutput("dirRes_selector")
        ),
        column(3
               , ""
               , actionButton(inputId = "doZip"
                              , label = "Archiver résultats"
                              , icon = icon("zip"))
        ),
        column(3
               , ""
               , downloadButton(outputId = "downloadSelection"
                              , label = "Télécharger résultats"
                              , icon = icon("download"))
        )
      ),
      br(),
      
      tabsetPanel(
        tabPanel(title = "Sites sélectionnés"
                 , value = "selectionsite"
                 , dataTableOutput(outputId = "RES_SEL")), 
        tabPanel(title = "Graphiques"
                 , value = "graphics"
                 , br()
                 , withSpinner(plotOutput(outputId = "plot2", width = "100%", height = "600px"), type = 4)
                 , br()
                 , br()
                 , withSpinner(plotOutput(outputId = "plot4", width = "100%", height = "1000px"), type = 1)
        )
      )
    )
    )
  )

###################################################################################################################################

# Define server function required to create the scatterplot
server <- function(input, output, session) {
  
  session$onSessionEnded(stopApp)
  
  ####################################################################
  get_allParams = eventReactive(input$refresh, {
    year.win = 6
    PARAMS = data.frame(PARAMETRE = c("ANNEE_DEPART"
                                      , "ANNEE_FIN"
                                      , "NOMBRE_SITES_PAR_AN"
                                      , "NOMBRE_SITES_MAX_PAR_PARTENAIRE")
                        , VALEUR = c(input$year.range[1]
                                     , input$year.range[2]
                                     , input$samp.no_sites
                                     , input$constraint.no_sites_max))
    if (length(input$constraint.SAJF) > 0)
    {
      PARAMS = rbind(PARAMS, data.frame(PARAMETRE = "CONTRAINTE_SAJF", VALEUR = input$constraint.SAJF))
    }
    if (length(input$constraint.PNE) > 0)
    {
      PARAMS = rbind(PARAMS, data.frame(PARAMETRE = "CONTRAINTE_PNE", VALEUR = input$constraint.PNE))
    }
    if (length(input$constraint.CBNA) > 0)
    {
      PARAMS = rbind(PARAMS, data.frame(PARAMETRE = "CONTRAINTE_CBNA", VALEUR = input$constraint.CBNA))
    }
    if (length(input$constraint.CBNMED) > 0)
    {
      PARAMS = rbind(PARAMS, data.frame(PARAMETRE = "CONTRAINTE_CBNMED", VALEUR = input$constraint.CBNMED))
    }
    if (length(input$constraint.notTogether) > 0)
    {
      PARAMS = rbind(PARAMS, data.frame(PARAMETRE = "CONTRAINTE_SIMILARITE", VALEUR = input$constraint.notTogether))
    }
    PARAMS = rbind(PARAMS, data.frame(PARAMETRE = c("SEUIL_ANNEES_PAS_ECHANTILLONNAGE"
                                                    , "PROB_AUGMENTATION_PAS_ECHANTILLONNAGE"
                                                    , "PROB_DIMINUTION_ECHANTILLONNAGE_CETTE_ANNEE"
                                                    , "SEUIL_ANNEES_ECHANTILLONNAGE_SUCCESSIF"
                                                    , "PROB_DIMINUTION_ECHANTILLONNAGE_SUCCESSIF"
                                                    , "FENETRE_CHECK_FREQUENCE"
                                                    , "SEUIL_CHECK_FREQUENCE"
                                                    , "PROB_DIMINUTION_CHECK_FREQUENCE_INCORRECT")
                                      , VALEUR = c(input$noXYears
                                                   , input$prob.increase.sampXYears
                                                   , input$prob.decrease.sampThisYear
                                                   , input$noSuccYears
                                                   , input$prob.decrease.sampSuccYears
                                                   , year.win
                                                   , floor((input$year.range[2] - input$year.range[1]) / year.win)
                                                   , input$prob.decrease.notWorking)))

    if (input$saveResults)
    {
      fwrite(PARAMS, file = paste0(get_dirSave(), "/PARAMS_echantillonnage_", get_params2(), ".txt")
             , sep = "\t", quote = FALSE, row.names = FALSE)
    }
  })
  
  get_params1 = eventReactive(input$refresh, {
    year.win = 6
    params = paste0(input$year.range[1], "_"
                    , input$year.range[2], "_"
                    , year.win, "_"
                    , input$prob.decrease.sampThisYear, "_"
                    , input$prob.decrease.sampSuccYears, "_"
                    , input$prob.increase.sampXYears, "_"
                    , input$prob.decrease.notWorking
    )
    params
  })
  
  get_version = eventReactive(input$refresh, {
    if (input$saveResults)
    {
      params = paste0(input$year.range[1], "_", input$year.range[2])
      version = length(list.files(path = getwd(), pattern = paste0("^ORCHAMP_selection_.*", params), recursive = F)) + 1
      version
    }
  })
  
  get_params2 = eventReactive(input$refresh, {
    paste0("version", get_version(), "_", input$year.range[1], "_", input$year.range[2])
  })
  
  get_params3 = eventReactive(input$refresh, {
    paste0("version", get_version(), "_", get_params1())
  })
  
  get_dirSave = eventReactive(input$refresh, {
    dirSave = paste0("ORCHAMP_selection_", get_params2())
    if (input$saveResults) dir.create(dirSave)
    dirSave
  })
  
  ####################################################################
  get_comb.ALL.vec = eventReactive(input$refresh, {
    
    ## Create all combinations of sites
    comb.ALL = as.data.frame(t(combn(x = sites.names, m = input$samp.no_sites)))
    colnames(comb.ALL) = paste0("SITE_", 1:ncol(comb.ALL))
    
    ## Remove combinations for number constraints
    constraint.list = list(input$constraint.CBNA
                           , input$constraint.CBNMED
                           , input$constraint.PNE
                           , input$constraint.SAJF)
    for(con in constraint.list)
    {
      no_sites_inConstraint = apply(comb.ALL, 1, function(x){ sum(x %in% con)})
      comb.ALL = comb.ALL[which(no_sites_inConstraint <= input$constraint.no_sites_max),]
    }
    ## Remove combinations for association constraints
    for(con in input$constraint.notTogether)
    {
      con = strsplit(con, "_")[[1]]
      no_sites_inConstraint = apply(comb.ALL, 1, function(x){ sum(x %in% con)})
      comb.ALL = comb.ALL[which(no_sites_inConstraint < length(con)),]
    }
    
    comb.ALL.vec = apply(comb.ALL, 1, function(x) paste0(x, collapse = "_"))
    comb.ALL.vec
  })
  
  ####################################################################
  get_RES = eventReactive(input$refresh, {
    
    ## Get arguments
    sites.no = length(sites.names)
    
    year.start = input$year.range[1]
    year.end = input$year.range[2]
    samp.years = seq(year.start, year.end, 1)
    year.win = 6
    
    prob.increase.sampXYears = 1 + input$prob.increase.sampXYears
    prob.decrease.sampThisYear = 1 - input$prob.decrease.sampThisYear
    prob.decrease.sampSuccYears = 1 - input$prob.decrease.sampSuccYears
    prob.decrease.notWorking = 1 - input$prob.decrease.notWorking
    
    comb.ALL.vec = get_comb.ALL.vec()
    
    ## --------------------------------------------------------------------------
    ## Initialize table to store for each site :
    ##  last year of sampling
    ##  number of successive sampling
    samp.sites_tab = data.frame(SITE = sites.names, LAST_YEAR = 0, NB_YEAR_SUCC = 0)
    
    ## Initialize table to store each possible combination of sites
    ## and probability of each combination
    pool.GLOB = data.frame(COMB = comb.ALL.vec
                           , PROB = rep(1, length(comb.ALL.vec)))
    
    if (!input$startFromSave)
    {
      sapply(list.files(pattern = "SAUVEGARDE_ANNEE_"), file.remove)
    } else
    {
      if (!is.null(input$dirRes) && nchar(input$dirRes) > 0)
      {
        sapply(list.files(pattern = "SAUVEGARDE_ANNEE_"), file.remove)
        prev_files = list.files(path = input$dirRes
                                , pattern = "SAUVEGARDE_ANNEE_"
                                , full.names = FALSE)
        sapply(prev_files, function(x) file.copy(from = paste0(input$dirRes, "/", x)
                                                 , to = paste0("./", x)))
      }
    }
    
    ## --------------------------------------------------------------------------
    withProgress(message = "CALCUL DE L'ECHANTILLONNAGE EN COURS"
                 , min = 0, max = year.end - year.start + 1, {
                   RES = foreach(ye.start = seq(year.end - (year.win - 1), year.start, -1)) %do%
                   {
                     cat(" ", ye.start)
                     if (input$startFromSave)
                     {
                       firstOK = TRUE
                     } else
                     {
                       firstOK = ifelse(ye.start == year.end - (year.win - 1), FALSE, TRUE)
                     }
                     setProgress(value = year.end - ye.start + 1, detail = paste("Année", ye.start))
                     
                     RES = FUN_SELECT_sites(ye = year.end, pool = pool.GLOB, samp = samp.sites_tab
                                            , firstOK = firstOK
                                            , year.start = ye.start
                                            , year.end = year.end
                                            , samp.no_sites = input$samp.no_sites
                                            , prob.decrease.sampThisYear = prob.decrease.sampThisYear
                                            , noSuccYears = input$noSuccYears
                                            , prob.decrease.sampSuccYears = prob.decrease.sampSuccYears
                                            , noXYears = input$noXYears
                                            , prob.increase.sampXYears = prob.increase.sampXYears
                                            , prob.decrease.notWorking = prob.decrease.notWorking
                                            , test.ref = floor((year.end - ye.start) / year.win)
                                            , test.win = year.win
                     )
                     return(RES)
                   }
                 })
    
    ## COPY outputs
    if (input$saveResults)
    {
      dirSave = get_dirSave()
      
      ## Save params
      get_allParams()
      
      ## Copy files into folder
      curr_files = list.files(path = getwd()
                              , pattern = "SAUVEGARDE_ANNEE_"
                              , full.names = FALSE)
      sapply(curr_files, function(x) file.copy(from = paste0("./", x)
                                               , to = paste0(dirSave, "/", x)))
      
      ## ZIP results
      # zip(zipfile = paste0("SAUVEGARDE_ORCHAMP_selection_", Sys.Date(), ".zip")
      #     , list.files(path = dirSave, full.names = T))
    }
    
    ## LOAD the correct RES
    SEL = foreach(ye = samp.years, .combine = "rbind") %do%
    {
      SAV = get(load(paste0("SAUVEGARDE_ANNEE_",ye)))
      return(SAV$SEL)
    }
    load(paste0("SAUVEGARDE_ANNEE_",year.start))
    RES = list(SEL = SEL, POOL = SAV$POOL, SAMP = SAV$SAMP)
    
    return(RES)
  })
  
  ####################################################################
  get_RES_SEL = eventReactive(input$refresh, {
    RES = get_RES()
    RES = RES$SEL
    RES.split = split(RES, RES$YEAR)
    RES = foreach(x = RES.split, .combine = "rbind") %do%
    {
      eval(parse(text = paste0("res = data.frame(",paste0("SITE", 1:input$samp.no_sites
                                                          ," = x$SITE[", 1:input$samp.no_sites, "]", collapse = ",")
                               ,")")))
      return(res)
    }
    rownames(RES) = names(RES.split)
    
    if (input$saveResults)
    {
      tmp_RES = RES
      tmp_RES = cbind(data.frame(ANNEE = rownames(tmp_RES)), tmp_RES)
      fwrite(tmp_RES, file = paste0(get_dirSave(), "/TABLE_echantillonnage_", get_params3(), ".txt")
             , sep = "\t", quote = FALSE, row.names = FALSE)
    }
    
    RES
  })
  
  output$RES_SEL = renderDataTable({
    get_RES_SEL()
  })

  
  ####################################################################
  get_plot2 = eventReactive(input$refresh, {
    RES = get_RES()
    RES$SEL$SITE = as.character(RES$SEL$SITE)
    
    pp = ggplot(RES$SEL, aes(SITE)) +
      geom_bar() +
      geom_hline(yintercept = floor((input$year.range[2] - input$year.range[1]) / 5)
                 , lty = 2, lwd = 1, color = "grey") +
      geom_hline(yintercept = mean(table(RES$SEL$SITE))
                    , lty = 2, lwd = 1, color = "brown") +
      labs(title = "Nombre d'années échantillonnées par site\n"
           , subtitle = paste0("En gris, le nombre minimal d'échantillonnages par site requis.\n"
                               , "En rouge, le nombre moyen d'échantillonnages par site avec la sélection calculée.\n")) +
      theme_fivethirtyeight() +
      theme(axis.text.x = element_text(angle = 45
                                       , size = 15
                                       , vjust = 0.4)
            , axis.text.y = element_text(size = 12))
    
    if (input$saveResults)
    {
      ggsave(filename = paste0("PLOT1_echantillonnage_", get_params3(), ".pdf")
             , plot = pp, path = get_dirSave(), width = 8, height = 8)
    }
    
    pp
  })
  
  output$plot2 = renderPlot({
    print(get_plot2())
  })
  
  ####################################################################
  get_plot4 = eventReactive(input$refresh, {
    
    ## Get arguments
    year.start = input$year.range[1]
    year.end = input$year.range[2]
    samp.years = seq(year.start, year.end, 1)
    
    ## Get results
    RES = get_RES()
    
    TMP = expand.grid(SITE = sites.names, YEAR = samp.years)
    TMP = merge(TMP, data.frame(RES$SEL, SAMP = 1), by = c("SITE","YEAR"), all.x = T)
    TMP$SAMP[which(is.na(TMP$SAMP))] = 0
    
    TMP.split = split(TMP, TMP$SITE)
    TMP.split = foreach(x = TMP.split, .combine = "rbind") %do%
    {
      cumul = cumsum(x$SAMP)
      cumul_bis = cumul
      for(i in 2:nrow(x))
      {
        if(cumul[i] == cumul[i-1]) { cumul_bis[i] = 0 }
        if(cumul[i] == cumul[i-1] + 1 && cumul_bis[i-1] > 0)
        {
          cumul_bis[i] = cumul_bis[i-1]
        }
      }
      cumul_ter = cumul_bis
      for(i in 1:nrow(x))
      {
        if(cumul_bis[i] > 0)
        {
          cumul_ter[i] = length(which(cumul_bis == cumul_bis[i]))
        }
      }
      return(data.frame(x[, c("SITE", "YEAR")], cumul, cumul_bis, cumul_ter))
    }
    TMP = merge(TMP, TMP.split, by = c("SITE", "YEAR"))
    TMP$cumul_ter[which(TMP$cumul_ter == 0)] = ""
    colos = c('#4477aa','#66ccee','#228833','#ccbb44','#ee6677','#aa3377','#bbbbbb')
    
    pp = ggplot(TMP, aes(YEAR, alpha = factor(SAMP), fill = factor(cumul_ter))) +
      scale_alpha_discrete(guide = F, range = c(0,1)) +
      scale_fill_manual("Nombre d'années successives d'échantillonnage :"
                        , values = c("white", colos[1:max(TMP$cumul_ter, na.rm = T)])) +
      geom_bar(width = 1) +
      facet_wrap( ~ SITE, ncol = 4) +
      labs(title = "Fréquence d'échantillonnage par site\n") +
      theme_fivethirtyeight() +
      theme(strip.text = element_text(size = 15)
            , panel.spacing = unit(x = 1, units = "lines")
            , axis.text.y = element_blank()
            , axis.text.x = element_text(size = 12)
            , legend.title = element_text(size = 15)
            , legend.text = element_text(size = 12)
            , legend.box.margin = margin(6, 0, 0, 0, "pt"))
    
    if (input$saveResults)
    {
      ggsave(filename = paste0("PLOT2_echantillonnage_", get_params3(), ".pdf")
             , plot = pp, path = get_dirSave(), width = 8, height = 11)
    }
    
    pp
  })
  
  output$plot4 = renderPlot({
    print(get_plot4())
  })
  
  ####################################################################
  output$dirRes_selector = renderUI({
    if (input$refresh)
    {
      selectInput(inputId = "dirRes"
                  , label = "Sélectionner un dossier"
                  , choices = c("",list.files(pattern = "^ORCHAMP_"))
                  , selected = NULL
                  , multiple = FALSE
      )
    }
  })
  
  ####################################################################
  get_zipfiles = eventReactive(input$refresh, {
    zip(zipfile = paste0("SAUVEGARDE_ORCHAMP_selection_", Sys.Date(), ".zip")
        , list.files(path = get_dirSave(), full.names = T))
  })
  
  observeEvent(input$doZip, {
    get_zipfiles()
  })
  
  output$zip_selector = renderUI({
    if (input$refresh || input$doZip)
    {
      selectInput(inputId = "zip"
                  , label = "Sélectionner un dossier"
                  , choices = c("",list.files(pattern = "^SAUVEGARDE_ORCHAMP_"))
                  , selected = NULL
                  , multiple = TRUE
      )
    }
  })
  
  output$downloadSelection = downloadHandler(
    filename = function(){
      paste0("SAUVEGARDE_ORCHAMP_selection_", Sys.Date(), ".zip")
    },
    content = function(file){
      file.copy(paste0("SAUVEGARDE_ORCHAMP_selection_", Sys.Date(), ".zip"), file)
    },
    contentType = "application/zip"
  )
  
  
}

###################################################################################################################################
# Create a Shiny app object
shinyApp(ui = ui, server = server)

