
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

DT.add.year.constraint <- data.frame()

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


###################################################################################################################################
# DEFINING SAMPLING FUNCTION 
###################################################################################################################################

## Apply sampling function for each required year
FUN_SELECT_sites = function(ye
                            , samp
                            , firstOK = FALSE
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
  cat("\n *-*-*-*-*-*-*-* ", ye, " *-*-*-*-*-*-*-* \n")
  if (!file.exists(paste0("SAUVEGARDE_ANNEE_", ye, ".RData")))
  {
    # cat("\n 1. Sites selection...")
    sites = sample(x = samp$SITE
                       , size = samp.no_sites
                       , prob = samp$PROB)
    
    ## --------------------------------------------------------------------------
    ## FOR ALL AVAILABLE SITES
    # cat("\n 2. Update of site informations...")
    for(si in samp$SITE)
    {
      ind_si = which(samp$SITE == si)
      
      ## Is the site selected this year ? ---------------------------------------
      if (si %in% sites)
      {
        ## Keep track of last year of sampling : 0
        samp$LAST_YEAR[ind_si] = 0
        
        ## Keep track of number of previous successive sampling : +1
        samp$NB_YEAR_SUCC[ind_si] = samp$NB_YEAR_SUCC[ind_si] + 1
        
        ## Reduce probability of sampling the site next year
        ## if the site has already been sampled for the last X successive years
        if (samp$NB_YEAR_SUCC[ind_si] >= noSuccYears)
        {
          samp$PROB[ind_si] = samp$PROB[ind_si] * prob.decrease.sampSuccYears
        } else ## Anyway, reduce probability of sampling the site next year
        {
          samp$PROB[ind_si] = samp$PROB[ind_si] * prob.decrease.sampThisYear
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
          samp$PROB[ind_si] = samp$PROB[ind_si] * prob.increase.sampXYears
        }
      }
    }
    
    ## --------------------------------------------------------------------------
    ## RESULTS
    res = data.frame(YEAR = ye, SITE = sites)
    SAV = list(SEL = res, SAMP = samp)
    save(SAV, file = paste0("SAUVEGARDE_ANNEE_", ye, ".RData"), envir = environment())
  } else
  {
    # cat("\n Loading previous results...\n")
    SAV = get(load(paste0("SAUVEGARDE_ANNEE_", ye, ".RData"), envir = environment()))
    res = SAV$SEL
    samp = SAV$SAMP
  }
  
  if(ye < year.end)
  {
    res_bis = FUN_SELECT_sites(ye = ye + 1
                               , samp = samp
                               , firstOK = firstOK
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
    
    # cat("\n 3. Evaluating results...")
    
    ## EVALUATION OF RESULTS
    cond.freq = cond.num = TRUE
    res_tmp = rbind(res, res_bis$SEL)
    
    ## Frequency ?
    if (ye <= year.end - (test.win - 1))
    {
      cat("\n ==> TEST FOR FREQUENCY IN YEAR ", ye, " <== \n")
      year.window = seq(ye, ye + (test.win - 1))
      SITE_table = table(res_tmp$SITE[which(res_tmp$YEAR %in% year.window)])
      cond.freq = (length(SITE_table) == nrow(samp) && length(which(SITE_table >= 1)) == nrow(samp))
      cond.freq.wrongSites = names(SITE_table)[which(SITE_table == 0)]
    }
    ## Total number ?
    if (ye == year.start)
    {
      cat("\n ==> TEST FOR TOTAL NUMBER IN YEAR ", ye, " <== \n")
      SITE_table = table(res_tmp$SITE)
      cond.num = (length(SITE_table) == nrow(samp) && length(which(SITE_table >= test.ref)) == nrow(samp))
    }
    
    ## --------------------------------------------------------------------------
    if(cond.freq && cond.num)
    {
      return(list(SEL = rbind(res, res_bis$SEL), SAMP = res_bis$SAMP))
    } else
    {
      ## No working sequence registered
      if(!firstOK)
      {
        ## First year of test : remove all files
        if (ye == year.end - (test.win - 1))
        {
          sapply(paste0("SAUVEGARDE_ANNEE_", seq(year.end, year.start), ".RData"), file.remove)
        } 
      } else
      {
        ## Remove only last year file
        SAV = get(load(paste0("SAUVEGARDE_ANNEE_", year.end, ".RData"), envir = environment()))
        SAV.sites = as.character(SAV$SEL$SITE)
        sapply(paste0("SAUVEGARDE_ANNEE_", year.end, ".RData"), file.remove)
        
        SAV = get(load(paste0("SAUVEGARDE_ANNEE_", year.end - 1, ".RData"), envir = environment()))
        # for(si in SAV.sites)
        for(si in cond.freq.wrongSites)
        {
          ind_si = which(SAV$SAMP$SITE == si)
          SAV$SAMP$PROB[ind_si] = SAV$SAMP$PROB[ind_si] * prob.decrease.notWorking
        }
        # if (min(SAV$SAMP$PROB) < 0.0001)
        # {
        #   SAV$SAMP$PROB = SAV$SAMP$PROB * 1000
        # }
        save(SAV, file = paste0("SAUVEGARDE_ANNEE_", year.end - 1, ".RData"), envir = environment())
      }
      
      cat("\n /!\\ Certaines conditions ne sont pas remplies (frequency : ", cond.freq
          , ", total number : ", cond.num, ") : redémarrage du calcul /!\\ \n")
      samp$PROB = 1
      samp$LAST_YEAR = 0
      samp$NB_YEAR_SUCC = 0
      return(FUN_SELECT_sites(ye = year.start
                              , samp = samp
                              , firstOK = firstOK
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
    return(list(SEL = res, SAMP = samp))
  }
}

###################################################################################################################################

# Define UI for application that plots features of movies
ui <- fluidPage(
  
  tags$body(
    tags$style(HTML("
                    @import url('https://fonts.googleapis.com/css?family=Londrina+Solid:200,300|Medula+One|Slabo+27px|Francois+One');
                    "))
    ),
  
  fluidRow(
    style = HTML(paste0("color: #FFFFFF; background-color: #3a7da8; margin-top: 20px; margin-bottom: 20px; font-family: 'Londrina Solid', cursive;")),
    column(12,
           headerPanel("ORCHAMP : sélection des sites", windowTitle = "ORCHAMP")
    )
  ),
  
  # Sidebar layout with a input and output definitions
  sidebarLayout(
    
    # Inputs
    sidebarPanel(
      
      ## ----------------------------------------------------------
      wellPanel(
        style = HTML("border-width:0px; background-color: rgba(207, 214, 227, 0.5);"),
        fluidRow(
          column(8,
                 "",
                 sliderInput(inputId = "year.range"
                             , label = "Années d'échantillonnage"
                             , min = 2016
                             , max = 2080
                             , value = c(2016, 2050)
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
        ),
        fluidRow(
          br()
          , column(3, numericInput(inputId = "spec.year"
                                   , label = "Année"
                                   , value = 2016
                                   , min = 2016
                                   , max = 2080
                                   , step = 1
                                   , width = "100%"))
          , column(3, numericInput(inputId = "spec.no_sites"
                                   , label = "Nb sites"
                                   , min = 0
                                   , max = 7
                                   , value = NULL
                                   , width = "100%"))
          , column(6, selectInput(inputId = "spec.sites"
                                  , label = "Sites"
                                  , choices = sites.names
                                  , selected = sites.names
                                  , multiple = TRUE
                                  , width = "100%"))
        ),
        fluidRow(
          column(8,
                 "",
                 actionButton(inputId = "add.year.constraint"
                              , label = "Année spécifique"
                              , icon = icon("plus")
                              , width = "100%")
          )
        ),
        fluidRow(
          column(12,
                 "",
                 tableOutput(outputId = "DT.add.year.constraint"))
        )
      ),
      
      ## ----------------------------------------------------------
      wellPanel(
        style = HTML("border-width:0px; background-color: rgba(207, 214, 227, 0.5);"),
        
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
          lapply(unique(INIT$Grp_Bota), function(grp) {
            column(12,
                   selectInput(inputId = paste0("constraint.", grp)
                               , label = paste0("Sites à charge de ", grp)
                               , choices = sites.names
                               , selected = get(paste0("constraint.", grp))
                               , multiple = TRUE
                               , width = "100%"
                   )
            )
          })
        )
        ),
      
      
      ## ----------------------------------------------------------
      wellPanel(
        style = HTML("border-width:0px; background-color: rgba(207, 214, 227, 0.5);"),
        
        h3("B. Contraintes d'association"),
        p(em("Certains sites sont similaires en termes de conditions environnementales.
             Des sites similaires ne peuvent être échantillonnés la même année.
             Au contraire, par souci de practicité d'échantillonnage, certains sites doivent toujours etre échantillonnés ensemble.")),
        
        selectInput(inputId = "constraint.notTogether"
                    , label = "Sites à ne pas échantillonner la même année"
                    , choices = comb.sites.2
                    , selected = constraint.notTogether
                    , multiple = TRUE
                    , width = "100%"
        ),
        selectInput(inputId = "constraint.together"
                    , label = "Sites à échantillonner la même année"
                    , choices = comb.sites.2
                    , selected = constraint.together
                    , multiple = TRUE
                    , width = "100%"
        )
        ),
      
      ## ----------------------------------------------------------
      wellPanel(
        style = HTML("border-width:0px; background-color: rgba(207, 214, 227, 0.5);"),
        
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
                              , min = 0.5
                              , max = 0.5
                              , value = 0.5
                              # , min = 0.25
                              # , max = 0.25
                              # , value = 0.25
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
      wellPanel(
        fluidRow(
          column(4
                 , ""
                 , p(em("Démarrer à partir des résultats précédents ?"))
          ),
          column(5
                 , ""
                 , p(em("Sauvegarder les résultats ?"))
          ),
          column(3
                 , ""
                 , p()
          )
        ),
        
        fluidRow(
          column(4
                 , ""
                 , checkboxInput(inputId = "startFromSave"
                                 , label = "oui"
                                 , value = TRUE)
          ),
          column(5
                 , ""
                 , checkboxInput(inputId = "saveResults"
                                 , label = "oui"
                                 , value = TRUE)
          ),
          column(3
                 , ""
                 , actionButton(inputId = "refresh"
                                , label = "Lancer calcul"
                                , icon = icon("refresh"))
          )
        ),
        
        fluidRow(
          column(4
                 , ""
                 , uiOutput("dirRes_selector")
          ),
          column(5
                 , ""
                 , p()
          ),
          column(3
                 , ""
                 , actionButton(inputId = "load"
                                , label = "Charger résultats"
                                , icon = icon("upload"))
          )
        )
      ),
      wellPanel(
        fluidRow(
          column(4
                 , ""
                 , p(em("1. Rassembler tous les résultats sous forme d'archive"))
          ),
          column(5
                 , ""
                 , p(em("2. Sélectionner l'archive à récupérer"))
          ),
          column(3
                 , ""
                 , p(em("3. Télécharger l'archive"))
          )
        ),
        fluidRow(
          column(4
                 , ""
                 , actionButton(inputId = "doZip"
                                , label = "Archiver résultats"
                                , icon = icon("zip"))
          ),
          column(5
                 , ""
                 , uiOutput("zip_selector")
          ),
          column(3
                 , ""
                 , downloadButton(outputId = "downloadSelection"
                                  , label = "Télécharger résultats"
                                  , icon = icon("download"))
          )
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
  
  observeEvent(input$add.year.constraint, {
    output$DT.add.year.constraint = renderTable({
      DT.add.year.constraint <<- rbind(DT.add.year.constraint
                                       , data.frame(YEAR = input$spec.year
                                                    , NB_SITES = input$spec.no_sites
                                                    , SITES = input$spec.sites))
      return(DT.add.year.constraint[, 1:2])
    })
  })
  
  ####################################################################
  get_allParams = eventReactive(input$refresh, {
    cat("\n >> SAVING : getting sampling parameters...\n")
    
    year.win = 6
    PARAMS = data.frame(PARAMETRE = c("ANNEE_DEPART"
                                      , "ANNEE_FIN"
                                      , "NOMBRE_SITES_PAR_AN"
                                      , "NOMBRE_SITES_MAX_PAR_PARTENAIRE")
                        , VALEUR = c(input$year.range[1]
                                     , input$year.range[2]
                                     , input$samp.no_sites
                                     , input$constraint.no_sites_max))
    for(grp in unique(INIT$Grp_Bota))
    {
      if (length(get(paste0("input$constraint.", grp))) > 0)
      {
        PARAMS = rbind(PARAMS, data.frame(PARAMETRE = paste0("CONTRAINTE_", grp)
                                          , VALEUR = get(paste0("input$constraint.", grp))))
      }
    }
    if (length(input$constraint.notTogether) > 0)
    {
      PARAMS = rbind(PARAMS, data.frame(PARAMETRE = "CONTRAINTE_PAS_ENSEMBLE", VALEUR = input$constraint.notTogether))
    }
    if (length(input$constraint.together) > 0)
    {
      PARAMS = rbind(PARAMS, data.frame(PARAMETRE = "CONTRAINTE_ENSEMBLE", VALEUR = input$constraint.together))
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
  
  get_params1 = reactive({
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
  
  get_version = eventReactive(input$year.range, {
    if (input$saveResults)
    {
      params = paste0(input$year.range[1], "_", input$year.range[2])
      version = length(list.files(path = getwd(), pattern = paste0("^ORCHAMP_selection_.*", params), recursive = F)) + 1
      version
    }
  })
  
  get_params2 = eventReactive(input$year.range, {
    paste0("version", get_version(), "_", input$year.range[1], "_", input$year.range[2])
  })
  
  get_params3 = eventReactive(input$year.range, {
    paste0("version", get_version(), "_", get_params1())
  })
  
  get_dirSave = eventReactive(input$year.range, {
    dirSave = paste0("ORCHAMP_selection_", get_params2())
    if (input$saveResults) dir.create(dirSave)
    dirSave
  })
  
  
  ####################################################################
  output$dirRes_selector = renderUI({
    dir_names = list.dirs(full.names = FALSE
                          , recursive = FALSE)
    dir_names = dir_names[grep("^ORCHAMP_", dir_names)]
    
    selectInput(inputId = "dirRes"
                , label = "Sélectionner un dossier"
                , choices = c("", dir_names)
                , selected = NULL
                , multiple = FALSE
    )
  })
  
  observeEvent(input$refresh, {
    output$dirRes_selector = renderUI({
      dir_names = list.dirs(full.names = FALSE
                            , recursive = FALSE)
      dir_names = dir_names[grep("^ORCHAMP_", dir_names)]
      
      selectInput(inputId = "dirRes"
                  , label = "Sélectionner un dossier"
                  , choices = c("", dir_names)
                  , selected = NULL
                  , multiple = FALSE
      )
    })
  })
  
  ####################################################################
  get_RES = eventReactive(tagList(input$load, input$refresh), {
    
    cat(" oyoo")
    year.start = input$year.range[1]
    year.end = input$year.range[2]
    samp.years = seq(year.start, year.end, 1)
    
    ## LOAD the correct RES
    SEL = foreach(ye = samp.years, .combine = "rbind") %do%
    {
      file_name = paste0("./", input$dirRes, "/SAUVEGARDE_ANNEE_", ye, ".RData")
      if (file.exists(file_name))
      {
        SAV = get(load(file_name))
        return(SAV$SEL)
      }
    }
    # load(paste0("./", input$dirRes, "/SAUVEGARDE_ANNEE_", year.start, ".RData"))
    RES = list(SEL = SEL, SAMP = 1) #SAV$SAMP)
    
    return(RES)
  })

  ####################################################################
  get_CALC = observeEvent(input$refresh, {
    cat("\n >> PREPARATION : initialize parameters...\n")
    
    ## Get arguments
    sites.no = length(sites.names)
    
    year.start = input$year.range[1]
    year.end = input$year.range[2]
    samp.years = seq(year.start, year.end, 1)
    year.win = ifelse((year.end - year.start) < 6, year.end - year.start, 6)
    
    prob.increase.sampXYears = 1 + input$prob.increase.sampXYears
    prob.decrease.sampThisYear = 1 - input$prob.decrease.sampThisYear
    prob.decrease.sampSuccYears = 1 - input$prob.decrease.sampSuccYears
    prob.decrease.notWorking = 1 - input$prob.decrease.notWorking
    
    
    ## --------------------------------------------------------------------------
    ## Initialize table to store for each site :
    ##  last year of sampling
    ##  number of successive sampling
    samp.sites_tab = data.frame(SITE = sites.names
                                , LAST_YEAR = 0
                                , NB_YEAR_SUCC = 0
                                , PROB = 1
                                , stringsAsFactors = FALSE)
    
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
    cat("\n >> STARTING !!\n")
    
    withProgress(message = "CALCUL DE L'ECHANTILLONNAGE EN COURS"
                 , min = 0, max = year.end - year.start + 1, {
                   RES = foreach(ye.end = seq(year.start + (year.win - 1), year.end, 1)) %do%
                   {
                     cat("\n ############ ", ye.end, " ############ \n")
                     if (input$startFromSave)
                     {
                       firstOK = TRUE
                     } else
                     {
                       firstOK = ifelse(ye.end == year.start + (year.win - 1), FALSE, TRUE)
                     }
                     setProgress(value = ye.end - year.start + 1, detail = paste("Année", ye.end))
                     
                     RES = FUN_SELECT_sites(ye = year.start
                                            # , pool = pool.GLOB
                                            , samp = samp.sites_tab
                                            , firstOK = firstOK
                                            , year.start = year.start
                                            , year.end = ye.end
                                            , samp.no_sites = input$samp.no_sites
                                            , prob.decrease.sampThisYear = prob.decrease.sampThisYear
                                            , noSuccYears = input$noSuccYears
                                            , prob.decrease.sampSuccYears = prob.decrease.sampSuccYears
                                            , noXYears = input$noXYears
                                            , prob.increase.sampXYears = prob.increase.sampXYears
                                            , prob.decrease.notWorking = prob.decrease.notWorking
                                            , test.ref = floor((ye.end - year.start) / year.win)
                                            , test.win = year.win
                     )
                     return(RES)
                   }
                 })
    
    ## COPY outputs
    if (input$saveResults)
    {
      cat("\n >> SAVING !!\n")
      
      dirSave = get_dirSave()
      
      ## Save params
      get_allParams()
      
      ## Copy files into folder
      curr_files = list.files(path = getwd()
                              , pattern = "SAUVEGARDE_ANNEE_"
                              , full.names = FALSE)
      sapply(curr_files, function(x) file.copy(from = paste0("./", x)
                                               , to = paste0(dirSave, "/", x)))
    }
    
    # ## LOAD the correct RES
    # SEL = foreach(ye = samp.years, .combine = "rbind") %do%
    # {
    #   SAV = get(load(paste0("SAUVEGARDE_ANNEE_", ye, ".RData")))
    #   return(SAV$SEL)
    # }
    # load(paste0("SAUVEGARDE_ANNEE_", year.start, ".RData"))
    # RES = list(SEL = SEL, SAMP = SAV$SAMP)
    # 
    # return(RES)
  })
  
  
  ####################################################################
  observeEvent(get_RES(), {
    RES = get_RES()
    RES = RES$SEL
    
    if (!is.null(RES) && nrow(RES) > 0)
    {
      RES.split = split(RES, RES$YEAR)
      RES = foreach(x = RES.split, .combine = "rbind") %do%
      {
        eval(parse(text = paste0("res = data.frame(",paste0("SITE", 1:input$samp.no_sites
                                                            ," = x$SITE[", 1:input$samp.no_sites, "]", collapse = ",")
                                 ,")")))
        return(res)
      }
      RES = cbind(data.frame(ANNEE = names(RES.split)), RES)
      
      if (input$saveResults)
      {
        fwrite(RES, file = paste0(get_dirSave(), "/TABLE_echantillonnage_", get_params3(), ".txt")
               , sep = "\t", quote = FALSE, row.names = FALSE)
      }
      
      output$RES_SEL = renderDataTable({ as.data.table(RES) })
    }
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
  get_zipfiles = eventReactive(input$refresh, {
    zip(zipfile = paste0("SAUVEGARDE_ORCHAMP_selection_", Sys.Date(), ".zip")
        , list.files(path = get_dirSave(), full.names = T))
  })
  
  observeEvent(input$doZip, {
    get_zipfiles()
  })
  
  output$zip_selector = renderUI({
    if (input$doZip)
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

