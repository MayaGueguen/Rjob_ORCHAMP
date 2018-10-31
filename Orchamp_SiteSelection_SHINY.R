
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
FUN_SELECT_sites = function(ye, pool, samp, firstOK = FALSE
                            , year.start, year.end ## fixed inputs !!
                            , samp.no_sites ## fixed inputs !!
                            , prob.decrease.sampThisYear ## fixed inputs !!
                            , noSuccYears ## fixed inputs !!
                            , prob.decrease.sampSuccYears ## fixed inputs !!
                            , noXYears ## fixed inputs !!
                            , prob.increase.sampXYears ## fixed inputs !!
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
    save(SAV, file = paste0("SAUVEGARDE_ANNEE_", ye))
  } else
  {
    # cat("\n Loading previous results...\n")
    SAV = get(load(paste0("SAUVEGARDE_ANNEE_", ye)))
    res = SAV$SEL
    pool = SAV$POOL
    samp = SAV$SAMP
  }
  
  setProgress(value = year.end - ye + 1, detail = paste("Année", ye))
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
      cat("\n", year.window)
      SITE_table = table(res_tmp$SITE[which(res_tmp$YEAR %in% year.window)])
      print(SITE_table[order(names(SITE_table))])
      cat("\n", length(SITE_table))
      cond.freq = (length(SITE_table) == nrow(samp) && length(which(SITE_table >= 1)) == nrow(samp))
    }
    ## Total number ?
    if (ye == year.end)
    {
      SITE_table = table(res_tmp$SITE)
      cond.num = (length(SITE_table) == nrow(samp) && length(which(SITE_table >= test.ref)) == nrow(samp))
    }
    # cat("\n cond.freq ", cond.freq)
    # cat("\n cond.num ", cond.num)
    # cat("\n")
    
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
        # else
        # { ## Second year of test : shift files
        #   sapply(seq(year.end, ye), function(x) file.remove(paste0("SAUVEGARDE_ANNEE_", x)))
        #   year.toKeep = seq(ye - 1, year.start)
        #   sapply(1:length(year.toKeep), function(x)
        #   {
        #     year.prev = year.toKeep[x]
        #     year.new = year.end - x + 1
        #     file.rename(from = paste0("SAUVEGARDE_ANNEE_", year.prev)
        #                 , to = paste0("SAUVEGARDE_ANNEE_", year.new))
        #     SAV = get(load(paste0("SAUVEGARDE_ANNEE_", year.new)))
        #     SAV$SEL$YEAR = year.new
        #     save(SAV, file = paste0("SAUVEGARDE_ANNEE_", year.new))
        #   })
        #   firstOK = TRUE
        # }
      } else
      { ## Remove only last year file
        # sapply(paste0("SAUVEGARDE_ANNEE_", year.start), file.remove)
        SAV = get(load(paste0("SAUVEGARDE_ANNEE_", year.start)))
        SAV.sites = SAV$SEL$SITE
        sapply(paste0("SAUVEGARDE_ANNEE_", year.start), file.remove)
        
        SAV = get(load(paste0("SAUVEGARDE_ANNEE_", year.start + 1)))
        for(si in SAV.sites)
        {
          ind = grep(si, SAV$POOL$COMB)
          SAV$POOL$PROB[ind] = SAV$POOL$PROB[ind] * 0.8
        }
        save(SAV, file = paste0("SAUVEGARDE_ANNEE_", year.start + 1))
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
        
        sliderInput("year.range"
                    , label = "Années d'échantillonnage :"
                    , min = 2020
                    , max = 2080
                    , value = c(2020, 2030)
                    , sep = ""
        ),
        
        numericInput(inputId = "samp.no_sites"
                     , label = "Nombre de sites / an :"
                     , min = 6
                     , max = 6
                     , value = 6
                     , step = 1
                     # , min = 5
                     # , max = 7
                     # , value = 5
                     # , step = 1
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
                     , min = 0.4
                     , max = 0.4
                     , value = 0.4
                     , step = 0.1
                     # , min = 0
                     # , max = 1
                     # , value = 0.01
                     # , step = 0.1
        ),
        
        fluidRow(
          column(6,
                 "",
                 numericInput(inputId = "noXYears"
                              , label = "Seuil d'années sans échantillonnage :"
                              , min = 2
                              , max = 2
                              , value = 2
                              , step = 1
                              # , min = 2
                              # , max = 5
                              # , value = 3
                              # , step = 1
                 )
          ),
          column(6,
                 "",
                 numericInput(inputId = "noSuccYears"
                              , label = "Seuil d'années d'échantillonnage successif :"
                              , min = 2
                              , max = 2
                              , value = 2
                              , step = 1
                              # , min = 2
                              # , max = 5
                              # , value = 3
                              # , step = 1
                 )
          )
        ),
        
        fluidRow(
          column(6,
                 "",
                 numericInput(inputId = "prob.increase.sampXYears"
                              , label = "Augmentation après seuil (%) :"
                              , min = 0.25
                              , max = 0.25
                              , value = 0.25
                              , step = 0.1
                              # , min = 0
                              # , max = 1
                              # , value = 0.01
                              # , step = 0.1
                 )
          ),
          column(6,
                 "",
                 numericInput(inputId = "prob.decrease.sampSuccYears"
                              , label = "Diminution après seuil (%) :"
                              , min = 0.6
                              , max = 0.6
                              , value = 0.6
                              , step = 0.1
                              # , min = 0
                              # , max = 1
                              # , value = 0.2
                              # , step = 0.1
                 )
          )
        )
        )
        ),
    
    # Output
    mainPanel(
      submitButton("Update View", icon("refresh")),
      # wellPanel("CONSOLE",style = "overflow-y:scroll; max-height: 600px",
      #           verbatimTextOutput("CONSOLE")
      # ),
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
                 # , withSpinner(plotlyOutput(outputId = "plot3"), type = 1)
                 , withSpinner(plotOutput(outputId = "plot3"), type = 1)
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
  # get_comb.ALL.bin = reactive({
  #   comb.ALL.vec = get_comb.ALL.vec()
  #   
  #   ## Transform combinations into binary matrix
  #   comb.ALL.bin = foreach(si = sites.names, .combine = 'cbind') %do%
  #   {
  #     sapply(comb.ALL.vec, function(x) length(grep(si, x)))
  #   }
  #   colnames(comb.ALL.bin) = sites.names
  #   comb.ALL.bin
  # })
  
  ####################################################################
  get_RES = reactive({
    
    ## Get arguments
    sites.no = length(sites.names)
    
    year.start = input$year.range[1]
    year.end = input$year.range[2]
    samp.years = seq(year.start, year.end, 1)
    year.win = 5
    
    prob.increase.sampXYears = 1 + input$prob.increase.sampXYears
    prob.decrease.sampThisYear = 1 - input$prob.decrease.sampThisYear
    prob.decrease.sampSuccYears = 1 - input$prob.decrease.sampSuccYears
    
    comb.ALL.vec = get_comb.ALL.vec()
    # comb.ALL.bin = get_comb.ALL.bin()
    
    ## --------------------------------------------------------------------------
    ## Initialize table to store for each site :
    ##  last year of sampling
    ##  number of successive sampling
    samp.sites_tab = data.frame(SITE = sites.names, LAST_YEAR = 0, NB_YEAR_SUCC = 0)
    
    ## Initialize table to store each possible combination of sites
    ## and probability of each combination
    pool.GLOB = data.frame(COMB = comb.ALL.vec
                           , PROB = rep(1, length(comb.ALL.vec)))
    
    ## REMOVE previous results
    sapply(list.files(pattern = "SAUVEGARDE_ANNEE_"), file.remove)
    
    ## --------------------------------------------------------------------------
    # conditions.num_freq = FALSE
    # while(!conditions.num_freq)
    # {
      withProgress(message = "CALCUL DE L'ECHANTILLONNAGE EN COURS"
                   , min = 0, max = year.end - year.start + 1, {
                     # cat("\n\n ==> CALCUL DE L'ECHANTILLONNAGE EN COURS \n")
                     RES = foreach(ye.start = seq(year.end - (year.win - 1), year.start, -1)) %do%
                     {
                       cat("\n\n ooooooooooooooooooooooooooooooooooo \n")
                       cat("\n", ye.start)
                       cat("\n")
                       RES = FUN_SELECT_sites(ye = year.end, pool = pool.GLOB, samp = samp.sites_tab
                                              , firstOK = ifelse(ye.start == year.end - (year.win - 1), FALSE, TRUE)
                                              , year.start = ye.start
                                              , year.end = year.end
                                              , samp.no_sites = input$samp.no_sites
                                              , prob.decrease.sampThisYear = prob.decrease.sampThisYear
                                              , noSuccYears = input$noSuccYears
                                              , prob.decrease.sampSuccYears = prob.decrease.sampSuccYears
                                              , noXYears = input$noXYears
                                              , prob.increase.sampXYears = prob.increase.sampXYears
                                              , test.ref = floor((year.end - ye.start) / year.win)
                                              , test.win = year.win
                       )
                       return(RES)
                     }
                   })
      ## LOAD the correct RES
      SEL = foreach(ye = samp.years, .combine = "rbind") %do%
      {
        SAV = get(load(paste0("SAUVEGARDE_ANNEE_",ye)))
        return(SAV$SEL)
      }
      load(paste0("SAUVEGARDE_ANNEE_",year.start))
      RES = list(SEL = SEL, POOL = SAV$POOL, SAMP = SAV$SAMP)
      
    #   # cat("\n 4. Check for number and frequency conditions...")
    #   SITE_table = table(RES$SEL$SITE)
    #   ref = floor((year.end - year.start) / 5)
    #   cat("\n", length(SITE_table))
    #   cat("\n", length(which(SITE_table >= ref)))
    #   cond.num = (length(SITE_table) == sites.no && length(which(SITE_table >= ref)) == sites.no)
    #   
    #   cond.freq = TRUE
    #   for(ye in year.start:(year.end - 5))
    #   {
    #     year.window = seq(ye, ye + 5)
    #     SITE_table = table(RES$SEL$SITE[which(RES$SEL$YEAR %in% year.window)])
    #     # print(SITE_table[order(names(SITE_table))])
    #     # cat("\n", length(SITE_table))
    #     cond.freq = (length(SITE_table) == sites.no && length(which(SITE_table >= 1)) == sites.no)
    #   }
    #   if (cond.num && cond.freq) conditions.num_freq = TRUE
    #   cat("\n\n cond.num ", cond.num)
    #   cat("\n\n cond.freq ", cond.freq)
    # }

    return(RES)
  })
  
  ####################################################################
  # output$CONSOLE = renderPrint({
  #   RES = get_RES()
  # })
  
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
      geom_hline(yintercept = floor((input$year.range[2] - input$year.range[1]) / 5), lty = 2) +
      labs(title = "Nombre d'années échantillonnées par site") +
      theme_fivethirtyeight() +
      theme(axis.text.x = element_text(angle = 90))
  })
  
  ####################################################################
  output$plot3 = renderPlot({ #renderPlotly({
    RES = get_RES()
    
    ggplot(RES$SEL, aes(YEAR, fill = SITE)) +
      scale_fill_discrete("") +
      geom_density(alpha = 0.1) +
      labs(title = "Densité d'échantillonnage par site au cours du temps") +
      theme_fivethirtyeight()
    # ggplotly(p)
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
