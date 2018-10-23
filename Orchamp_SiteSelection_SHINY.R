
rm(list=ls())

## load required libraries
library(foreach)
library(reshape2)
library(ggplot2)
library(ggthemes)
library(plotly)
library(shiny)
library(shinythemes)
library(shinycssloaders)
library(DT)

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
constraint.notTogether = "Chamrousse_Ventoux Sud"


###################################################################################################################################
# DEFINING SAMPLING FUNCTION 
###################################################################################################################################

## Apply sampling function for each required year
FUN_SELECT_sites = function(ye, pool
                            , year.start, year.end ## fixed inputs !!
                            , samp.sites_tab ## fixed inputs !!
                            , samp.no_sites ## fixed inputs !!
                            , comb.ALL.bin ## fixed inputs !!
                            , prob.decrease.sampThisYear ## fixed inputs !!
                            , noSuccYears ## fixed inputs !!
                            , prob.decrease.sampSuccYears ## fixed inputs !!
                            , noXYears ## fixed inputs !!
                            , prob.increase.sampXYears ## fixed inputs !!
)
{
  cat(" ", ye)
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
    combiToRemove = which(colSums(t(comb.ALL.bin[, sites])) >= 4)
    
    pool$AVAIL = 1
    pool$AVAIL[combiToRemove] = 0
    
    res_bis = FUN_SELECT_sites(ye = ye + 1, pool = pool
                               , year.start = year.start
                               , year.end = year.end
                               , samp.sites_tab = samp.sites_tab
                               , samp.no_sites = samp.no_sites
                               , comb.ALL.bin = comb.ALL.bin
                               , prob.decrease.sampThisYear = prob.decrease.sampThisYear
                               , noSuccYears = noSuccYears
                               , prob.decrease.sampSuccYears = prob.decrease.sampSuccYears
                               , noXYears = noXYears
                               , prob.increase.sampXYears = prob.increase.sampXYears
    )
    return(list(SEL = rbind(res, res_bis$SEL), POOL = res_bis$POOL))
  } else
  {
    return(list(SEL = res, POOL = pool))
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
        
        sliderInput("year.range"
                    , label = "Années d'échantillonnage :"
                    , min = 2020
                    , max = 2080
                    , value = c(2020, 2050)
                    , sep = ""
        ),
        
        numericInput(inputId = "samp.no_sites"
                     , label = "Nombre de sites / an :"
                     , min = 5
                     , max = 7
                     , value = 5
                     , step = 1
        )
      ),
      
      ## ----------------------------------------------------------
      wellPanel(
        
        h3("A. Contraintes d'échantillonnage"),
        p(em("Les sites sont attribués aux différents établissements impliqués.
             Un nombre limite de sites à échantillonner par an et par établissement
             est fixé au préalable.")),
        
        fluidRow(
          column(6,
                 "",
                 selectInput(inputId = "constraint.CBNA"
                             , label = "Sites à charge du CBNA :"
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
                                      , label = "Sites à charge du CBNMED :"
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
                                      , label = "Sites à charge du PNE :"
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
                                      , label = "Sites à charge de la SAJF :"
                                      , choices = sites.names
                                      , selected = constraint.SAJF
                                      , multiple = TRUE
                          )
                   )
                 )
          )
        ),
        
        numericInput(inputId = "constraint.no_sites_max"
                     , label = "Nombre de sites MAX / établissement / an :"
                     , min = 2
                     , max = 5
                     , value = 3
                     , step = 1
        )
        ),
      
      ## ----------------------------------------------------------
      wellPanel(
        
        h3("B. Contraintes d'association"),
        p(em("Certains sites sont similaires en termes de conditions environnementales.
             Des sites similaires ne peuvent être échantillonnés la même année.")),
        
        selectInput(inputId = "constraint.notTogether"
                    , label = "Sites à ne pas échantillonner la même année :"
                    , choices = comb.sites.2
                    , selected = constraint.notTogether
                    , multiple = TRUE
        )
      ),
      
      ## ----------------------------------------------------------
      wellPanel(
        
        h3("C. Evolution des probabilités de sélection"),
        p(em("La probabilité de chaque site d'etre échantillonné évolue au cours
             du temps (i) si le site vient d'etre échantillonné, (ii) si le site a été
             échantillonné plusieurs années de suite, (iii) si le site n'a pas été
             échantillonné depuis plusieurs années.")),
        
        numericInput(inputId = "prob.decrease.sampThisYear"
                     , label = "Diminution après échantillonnage (%) :"
                     , min = 0
                     , max = 1
                     , value = 0.01
                     , step = 0.1
        ),
        
        fluidRow(
          column(6,
                 "",
                 numericInput(inputId = "noXYears"
                              , label = "Seuil d'années sans échantillonnage :"
                              , min = 2
                              , max = 5
                              , value = 3
                              , step = 1
                 )
          ),
          column(6,
                 "",
                 numericInput(inputId = "noSuccYears"
                              , label = "Seuil d'années d'échantillonnage successif :"
                              , min = 2
                              , max = 5
                              , value = 3
                              , step = 1
                 )
          )
        ),
        
        fluidRow(
          column(6,
                 "",
                 numericInput(inputId = "prob.increase.sampXYears"
                              , label = "Augmentation après seuil (%) :"
                              , min = 0
                              , max = 1
                              , value = 0.01
                              , step = 0.1
                 )
          ),
          column(6,
                 "",
                 numericInput(inputId = "prob.decrease.sampSuccYears"
                              , label = "Diminution après seuil (%) :"
                              , min = 0
                              , max = 1
                              , value = 0.2
                              , step = 0.1
                 )
          )
        )
        )
        ),
    
    # Output
    mainPanel(
      submitButton("Update View", icon("refresh")),
      br(),
      
      tabsetPanel(
        tabPanel("Sites sélectionnés", dataTableOutput(outputId = "RES_SEL")), 
        tabPanel("Graphiques",
                 fluidRow(
                   column(6
                          , ""
                          , withSpinner(plotOutput(outputId = "plot1"), type = 4)
                   ),
                   column(6
                          , ""
                          , plotOutput(outputId = "plot2")
                   )
                 )
                 , withSpinner(plotlyOutput(outputId = "plot3"), type = 1)
                 , plotOutput(outputId = "plot4"))
      )
    )
        )
  )

###################################################################################################################################

# Define server function required to create the scatterplot
server <- function(input, output, session) {
  
  session$onSessionEnded(stopApp)
  
  ####################################################################
  get_comb.ALL.vec = reactive({
    
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
  get_comb.ALL.bin = reactive({
    comb.ALL.vec = get_comb.ALL.vec()
    
    ## Transform combinations into binary matrix
    comb.ALL.bin = foreach(si = sites.names, .combine = 'cbind') %do%
    {
      sapply(comb.ALL.vec, function(x) length(grep(si, x)))
    }
    colnames(comb.ALL.bin) = sites.names
    comb.ALL.bin
  })
  
  ####################################################################
  get_RES = reactive({
    
    ## Get arguments
    sites.no = length(sites.names)
    
    year.start = input$year.range[1]
    year.end = input$year.range[2]
    
    samp.years = seq(year.start, year.end, 1)
    samp.no_years = length(samp.years)
    
    prob.increase.sampXYears = 1 + input$prob.increase.sampXYears
    prob.decrease.sampThisYear = 1 - input$prob.decrease.sampThisYear
    prob.decrease.sampSuccYears = 1 - input$prob.decrease.sampSuccYears
    
    comb.ALL.vec = get_comb.ALL.vec()
    comb.ALL.bin = get_comb.ALL.bin()
    
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
    
    conditions.num_freq = FALSE
    while(!conditions.num_freq)
    {
      cat("\n\n ==> SELECTION FOR YEAR")
      RES = FUN_SELECT_sites(ye = year.start, pool = pool.GLOB
                             , year.start = year.start
                             , year.end = year.end
                             , samp.sites_tab = samp.sites_tab
                             , samp.no_sites = input$samp.no_sites
                             , comb.ALL.bin = comb.ALL.bin
                             , prob.decrease.sampThisYear = prob.decrease.sampThisYear
                             , noSuccYears = input$noSuccYears
                             , prob.decrease.sampSuccYears = prob.decrease.sampSuccYears
                             , noXYears = input$noXYears
                             , prob.increase.sampXYears = prob.increase.sampXYears
      )
      # cat("\n 4. Check for number and frequency conditions...")
      SITE_table = table(RES$SEL$SITE)
      cond.num = (length(which(SITE_table >= 5)) == sites.no)
      
      cond.freq = TRUE
      for(ye in year.start:(year.end - 4))
      {
        year.window = seq(ye, ye + 4)
        SITE_table = table(RES$SEL$SITE[which(RES$SEL$YEAR %in% year.window)])
        cond.freq = (length(SITE_table) == sites.no && length(which(SITE_table >= 1)) == sites.no)
      }
      if (cond.num && cond.freq) conditions.num_freq = TRUE
    }
    RES
  })
  
  ####################################################################
  output$RES_SEL = renderDataTable({
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
    RES
  })
  output$RES_POOL = renderDataTable({
    RES = get_RES()
    RES$POOL
  })
  
  ####################################################################
  output$plot1 = renderPlot({
    RES = get_RES()
    
    ggplot(RES$POOL, aes(PROB)) +
      geom_histogram() +
      labs(title = "Distribution des probabilités de sélection des sites") +
      theme_fivethirtyeight()
  })
  
  ####################################################################
  output$plot2 = renderPlot({
    RES = get_RES()
    
    ggplot(RES$SEL, aes(SITE)) +
      geom_bar() +
      geom_hline(yintercept = 5, lty = 2) +
      labs(title = "Nombre d'années échantillonnées par site") +
      theme_fivethirtyeight() +
      theme(axis.text.x = element_text(angle = 90))
  })
  
  ####################################################################
  output$plot3 = renderPlotly({
    RES = get_RES()
    
    p = ggplot(RES$SEL, aes(YEAR, fill = SITE)) +
      scale_fill_discrete("") +
      geom_density(alpha = 0.1) +
      labs(title = "Densité d'échantillonnage par site au cours du temps") +
      theme_fivethirtyeight()
    ggplotly(p)
  })
  
  ####################################################################
  output$plot4 = renderPlot({
    
    ## Get arguments
    year.start = input$year.range[1]
    year.end = input$year.range[2]
    samp.years = seq(year.start, year.end, 1)
    
    ## Get results
    RES = get_RES()

    TMP = expand.grid(SITE = sites.names, YEAR = samp.years)
    TMP = merge(TMP, data.frame(RES$SEL, SAMP = 1), by = c("SITE","YEAR"), all.x = T)
    TMP$SAMP[which(is.na(TMP$SAMP))] = 0
    
    ggplot(TMP, aes(YEAR, alpha = factor(SAMP))) +
      scale_alpha_discrete(guide = F, range = c(0,1)) +
      scale_fill_identity() +
      geom_bar(width = 1) +
      facet_wrap(~SITE) +
      theme_fivethirtyeight()
  })
}

###################################################################################################################################
# Create a Shiny app object
shinyApp(ui = ui, server = server)
