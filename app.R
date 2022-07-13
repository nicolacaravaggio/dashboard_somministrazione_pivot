options("repos" = c("CRAN" = "https://cran.rstudio.com"))

# ==============================================================================
# DASHBOARD - PIVOT STOCK SOMMINISTRAZIONE
# ==============================================================================

# Required packages
library(shiny)
library(shinydashboard)
library(htmlwidgets)
library(pivottabler)


# Work directory
#if (Sys.getenv('USERNAME')=='Nicola'){
#    setwd("C:/Users/Nicola Caravaggio/OneDrive/Desktop/Dati Ministeriali/Dashboard/Pivot")
#} else if (Sys.getenv('USERNAME')=='cilu')
#{Sys.getenv('USERNAME')=='cilu'
#    setwd("C:/Users/NickS/OneDrive/Desktop/Dati Ministeriali/Dashboard/Pivot")
#} else {choose.dir()}

# ==============================================================================
# IMPORT DATA
# ------------------------------------------------------------------------------

# Import Tabella mensile
df_m_all <- read.csv('df_stock_m_all.csv', header = TRUE, sep = ";") 
df_m_all$anno_mese <- paste0(df_m_all$anno,"-",df_m_all$mese)
df_m_all$trim <- ifelse(df_m_all$mese %in% c(1,2,3),1,ifelse(df_m_all$mese %in% c(4,5,6),2,ifelse(df_m_all$mese %in% c(7,8,9),3,4)))
df_m_all$trims <- ifelse(df_m_all$mese %in% c(1,2,3),'I trim.',ifelse(df_m_all$mese %in% c(4,5,6),'II trim.',ifelse(df_m_all$mese %in% c(7,8,9),'III trim.','IV trim.')))
df_m_all$anno_trim <- paste0(df_m_all$anno,"-",df_m_all$trim)
df_m_all$anno_trims <- paste0(df_m_all$anno,"-",df_m_all$trims)
df_m_all$regione[which(df_m_all$regione == "Friuli-Venezia Giulia")] <- "Friuli Venezia Giulia"

# Import Tabella trimestrale
df_t_all <- read.csv('df_stock_t_all.csv', header = TRUE, sep = ";") 
df_t_all$anno_trim <- paste0(df_t_all$anno,"-",df_t_all$trim)
df_t_all$trims <- ifelse(df_t_all$trim == 1,'I trim.',ifelse(df_t_all$trim == 2,'II trim.',ifelse(df_t_all$trim == 3,'III trim.','IV trim.')))
df_t_all$anno_trims <- paste0(df_t_all$anno,"-",df_t_all$trims)
df_t_all$regione[which(df_t_all$regione == "Friuli-Venezia Giulia")] <- "Friuli Venezia Giulia"

# Anni
anni = unique(df_m_all$anno)
# Mesi
mesi = unique(df_m_all$mese)
# Trimestri
trimestri = unique(df_m_all$trim)
# Regioni
regioni = unique(df_m_all$regione)
regioni = regioni[! regioni %in% c("N.D.")]
regioni = c("Totale",sort(regioni))
# Genere
genere = unique(df_m_all$genere)
genere = genere[! genere %in% c("N.D.")]
genere = c("Totale",sort(genere))
# Classi di eta'
eta = unique(df_m_all$eta)
eta = eta[! eta %in% c("N.D.")]
eta = c("Totale",sort(eta))
# Nazionalita'
cit = unique(df_m_all$citt)
cit = cit[! cit %in% c("N.D.")]
cit = c("Totale",sort(cit))
# Titolo di studio
studio = unique(df_m_all$studio)
studio = studio[! studio %in% c("N.D.")]
studio = c("Totale",sort(studio))
# Settori economici
settori = unique(df_m_all$settori_7)
settori = settori[! settori %in% c("N.D.")]
settori = c("Totale",sort(settori))
# Grandi gruppi professionali
professioni = unique(df_m_all$prof_8)
professioni = professioni[! professioni %in% c("N.D.")]
professioni = c("Totale",sort(professioni))

# Sistemazioni dataframe
df_m_all$lav <- "Lavoratori"
df_m_all$ct_cti <- ifelse(df_m_all$ct_cti == "", df_m_all$ct, df_m_all$ct_cti)
df_t_all$lav <- "Lavoratori"
df_t_all$ct_cti <- ifelse(df_t_all$ct_cti == "", df_t_all$ct, df_t_all$ct_cti)

# ==============================================================================
# UI
# ------------------------------------------------------------------------------

ui <- dashboardPage(
    
    dashboardHeader(title = "",
                    tags$li(class = "dropdown",
                            tags$a(href = "https://www.assolavoro.eu/", target = "_blank", 
                                   tags$img(height = '20px', src = "assolavoro_logo_lite.png")
                            )),
                    tags$li(class = "dropdown",
                            tags$a(href = "https://economia.uniroma3.it/ricerca/laboratori-e-osservatori/il-lavoro-in-somministrazione-in-italia/", target = "_blank", 
                                   tags$img(height = '20px', src = "roma3_logo.png")
                            )),
                    tags$li(class = "dropdown",
                            tags$a(href = "http://shiny.rstudio.com", target = "_blank", 
                                   tags$img(height = '20px', src = "shiny_logo.png")
                            ))
                    #,
                    #tags$li(class = "dropdown",
                            #tags$a(href = "https://www.highcharts.com/", target = "_blank", 
                                   #tags$img(height = '20px', src = "highcharts_logo.png")
                            #))
    ),
    
    dashboardSidebar(
        
        sidebarMenu(
            
            # Seleziona arco temporale
            sliderInput("daterange_1", "Arco temporale:",
                        min = min(anni),
                        max   = max(anni),
                        value = c(max(anni)-1, max(anni)),
                        sep = "",
                        step = 1),
            # Seleziona genere
            selectInput(inputId = "genere_1",
                        label = "Genere:",
                        choices = genere,
                        selected = c("Totale")),
            # Seleziona eta
            selectInput(inputId = "eta_1",
                        label = "Classi di eta':",
                        choices = eta,
                        selected = c("Totale")),
            # Seleziona nazionalita'
            selectInput(inputId = "cit_1",
                        label = "Nazionalita':",
                        choices = cit,
                        selected = c("Totale")),
            # Seleziona titolo di studio
            selectInput(inputId = "studio_1",
                        label = "Titolo di studio:",
                        choices = studio,
                        selected = c("Totale")),
            # Seleziona settore economico
            selectInput(inputId = "settori_1",
                        label = "Settore economico:",
                        choices = settori,
                        selected = c("Totale")),
            # Seleziona professioni
            selectInput(inputId = "professioni_1",
                        label = "Grandi gruppi professionali:",
                        choices = professioni,
                        selected = c("Totale")),
            # Seleziona regioni
            selectInput(inputId = "region_1",
                        label = "Regione:",
                        choices = regioni,
                        selected = c("Italia"))
            
            )),
            
    dashboardBody(

        #titlePanel("Somministrazione"),
                
        titlePanel(img(src = "assolavoro_logo.png", height = "70px", style = "padding-left: 20px; padding-bottom: 15px")),
        
        fluidPage(
            
        fluidRow(
                    
            box(title = "Selezione righe e colonne", solidHeader = TRUE, status = 'primary', width = 2,
                        
            tabPanel("", 
            
            # Righe 
            selectInput("selectRows_1", label = "Riga 1:",
                        choices = list("N.D." = "mese",
                                       "Genere" = "genere",
                                       "Contratto" = "ct",
                                       "Contratti (CTI)" = "ct_cti",
                                       "Classi di eta'" = "eta",
                                       "Nazionalita'" = "citt",
                                       "Titolo di studio" = "studio",
                                       "Settori" = "settori_7",
                                       "Grandi gruppi professionali" = "prof_8"), selected = "N.D."),
            selectInput("selectRows_2", label = "Riga 2:",
                        choices = list("N.D." = "mese",
                                       "Genere" = "genere",
                                       "Contratto" = "ct",
                                       "Contratti (CTI)" = "ct_cti",
                                       "Classi di eta'" = "eta",
                                       "Nazionalita'" = "citt",
                                       "Titolo di studio" = "studio",
                                       "Settori" = "settori_7",
                                       "Grandi gruppi professionali" = "prof_8"), selected = "N.D."),
            # Colonne
            selectInput("selectCols_1", label = "Colonna 1:",
                        choices = list("N.D." = "lav",
                                       "Genere" = "genere",
                                       "Contratto" = "ct",
                                       "Contratti (CTI)" = "ct_cti",
                                       "Classi di eta'" = "eta",
                                       "Nazionalita'" = "citt",
                                       "Titolo di studio" = "studio",
                                       "Settori" = "settori_7",
                                       "Grandi gruppi professionali" = "prof_8"), selected = "N.D."),
            selectInput("selectCols_2", label = "Colonna 2:",
                        choices = list("N.D." = "lav",
                                       "Genere" = "genere",
                                       "Contratto" = "ct",
                                       "Contratti (CTI)" = "ct_cti",
                                       "Classi di eta'" = "eta",
                                       "Nazionalita'" = "citt",
                                       "Titolo di studio" = "studio",
                                       "Settori" = "settori_7",
                                       "Grandi gruppi professionali" = "prof_8"), selected = "N.D.")

            )),
        
            box(title = "Tabella Pivot mensile", solidHeader = TRUE, status = 'primary', width = 5,
                
                tabPanel("",
                         
                         pivottablerOutput('pvt'),
                         downloadButton("d_pvt", "Download")
            
            )),

            box(title = "Tabella Pivot trimestrale", solidHeader = TRUE, status = 'primary', width = 5,
                
                tabPanel("",
                    
                         pivottablerOutput('pvt_t'),
                         downloadButton("d_pvt_t", "Download")
                    
            ))
        ))
    )
)

# ==============================================================================
# SERVER
# ------------------------------------------------------------------------------

server <- function(input, output, session) {

    # Reactive dataframe (mensile)
    
    # Regioni
    df_react_ts1 <- reactive({
        
        if (input$region_1 == "Totale") {
            assign("mydata_1", df_m_all[df_m_all$anno >= input$daterange_1[1] & 
                                        df_m_all$anno <= input$daterange_1[2]
                                        ,])
        } else if (input$region_1 != "Totale") {
            assign("mydata_1", df_m_all[df_m_all$regione == input$region_1 &
                                        df_m_all$anno >= input$daterange_1[1] & 
                                        df_m_all$anno <= input$daterange_1[2],])
        } 

        mydata_1
    })
    # Genere
    df_react_ts2 <- reactive({
        
        mydata <- df_react_ts1()
        
        if (input$genere_1 == "Totale") {
            assign("mydata_2", mydata)
        } else if (input$genere_1 != "Totale") {
            assign("mydata_2", mydata[mydata$genere == input$genere_1,])
        } 
        
        mydata_2
    })
    # Classi di eta'
    df_react_ts3 <- reactive({
        
        mydata <- df_react_ts2()
        
        if (input$eta_1 == "Totale") {
            assign("mydata_3", mydata)
        } else if (input$eta_1 != "Totale") {
            assign("mydata_3", mydata[mydata$eta == input$eta_1,])
        } 
        
        mydata_3
    })
    # Nazionalita'
    df_react_ts4 <- reactive({
      
      mydata <- df_react_ts3()
      
      if (input$cit_1 == "Totale") {
        assign("mydata_4", mydata)
      } else if (input$cit_1 != "Totale") {
        assign("mydata_4", mydata[mydata$citt == input$cit_1,])
      } 
      
      mydata_4
    })
    # Titolo di studio
    df_react_ts5 <- reactive({
        
        mydata <- df_react_ts4()
        
        if (input$studio_1 == "Totale") {
            assign("mydata_5", mydata)
        } else if (input$studio_1 != "Totale") {
            assign("mydata_5", mydata[mydata$studio == input$studio_1,])
        } 
        
        mydata_5
    })
    # Settori economici
    df_react_ts6 <- reactive({
        
        mydata <- df_react_ts5()
        
        if (input$settori_1 == "Totale") {
            assign("mydata_6", mydata)
        } else if (input$settori_1 != "Totale") {
            assign("mydata_6", mydata[mydata$settori_7 == input$settori_1,])
        } 
        
        mydata_6
    })
    # Grandi gruppi professionali
    df_react_ts <- reactive({
        
        mydata <- df_react_ts6()
        
        if (input$professioni_1 == "Totale") {
            assign("mydata_7", mydata)
        } else if (input$professioni_1 != "Totale") {
            assign("mydata_7", mydata[mydata$prof_8 == input$professioni_1,])
        } 
        
        mydata_7
    })
    
    # Reactive dataframe (trimestrale)
    
    # Regioni
    df_react_ts1t <- reactive({
        
        if (input$region_1 == "Totale") {
            assign("mydata_1t", df_t_all[df_t_all$anno >= input$daterange_1[1] & 
                                             df_t_all$anno <= input$daterange_1[2]
                                         ,])
        } else if (input$region_1 != "Totale") {
            assign("mydata_1t", df_t_all[df_t_all$regione == input$region_1 &
                                             df_t_all$anno >= input$daterange_1[1] & 
                                             df_t_all$anno <= input$daterange_1[2],])
        } 
        
        mydata_1t
    })
    # Genere
    df_react_ts2t <- reactive({
        
        mydata <- df_react_ts1t()
        
        if (input$genere_1 == "Totale") {
            assign("mydata_2t", mydata)
        } else if (input$genere_1 != "Totale") {
            assign("mydata_2t", mydata[mydata$genere == input$genere_1,])
        } 
        
        mydata_2t
    })
    # Classi di eta'
    df_react_ts3t <- reactive({
        
        mydata <- df_react_ts2t()
        
        if (input$eta_1 == "Totale") {
            assign("mydata_3t", mydata)
        } else if (input$eta_1 != "Totale") {
            assign("mydata_3t", mydata[mydata$eta == input$eta_1,])
        } 
        
        mydata_3t
    })
    # Nazionalita'
    df_react_ts4t <- reactive({
      
      mydata <- df_react_ts3t()
      
      if (input$cit_1 == "Totale") {
        assign("mydata_4t", mydata)
      } else if (input$cit_1 != "Totale") {
        assign("mydata_4t", mydata[mydata$citt == input$cit_1,])
      } 
      
      mydata_4t
    })
    # Titolo di studio
    df_react_ts5t <- reactive({
        
        mydata <- df_react_ts4t()
        
        if (input$studio_1 == "Totale") {
            assign("mydata_5t", mydata)
        } else if (input$studio_1 != "Totale") {
            assign("mydata_5t", mydata[mydata$studio == input$studio_1,])
        } 
        
        mydata_5t
    })
    # Settori economici
    df_react_ts6t <- reactive({
        
        mydata <- df_react_ts5t()
        
        if (input$settori_1 == "Totale") {
            assign("mydata_6t", mydata)
        } else if (input$settori_1 != "Totale") {
            assign("mydata_6t", mydata[mydata$settori_7 == input$settori_1,])
        } 
        
        mydata_6t
    })
    # Grandi gruppi professionali
    df_react_tst <- reactive({
        
        mydata <- df_react_ts6t()
        
        if (input$professioni_1 == "Totale") {
            assign("mydata_7t", mydata)
        } else if (input$professioni_1 != "Totale") {
            assign("mydata_7t", mydata[mydata$prof_8 == input$professioni_1,])
        } 
        
        mydata_7t
    })
    
    
    # Pivot table (mensile)
    output$pvt <- renderPivottabler({
        
        righe <- unique(c("anno", "trims", "mese", input$selectRows_1, input$selectRows_2))
        colonne <- unique(c(input$selectCols_1, input$selectCols_2))
        totali <- ifelse(length(colonne) == 1 & length(righe) <= 3, colonne, colonne[! colonne %in% c("lav")])
        
        qhpvt(df_react_ts(), 
              righe, 
              colonne, 
              "round(sum(lavoratori, na.rm = TRUE),0)",
              totals = totali
              )
        
    })
    # Download (.csv) dei dati
    output$d_pvt <- downloadHandler(
        filename = function() {
            paste("d_pvt.csv", sep = ";")
        },
        content = function(file) {
            write.csv(df_react_ts(), file, row.names = FALSE)
        }
    )
    
    # Pivot table (trimestrale)
    output$pvt_t <- renderPivottabler({
        
        righe <- unique(c("anno", "trims", input$selectRows_1, input$selectRows_2))
        righe <- righe[! righe %in% c("mese")]
        colonne <- unique(c(input$selectCols_1, input$selectCols_2))
        totali <- ifelse(length(colonne) == 1 & length(righe) <= 2, colonne, colonne[! colonne %in% c("lav")])
        
        qhpvt(df_react_tst(), 
              righe, 
              colonne, 
              "round(sum(lavoratori, na.rm = TRUE),0)",
              totals = totali
        )
        
    })
    # Download (.csv) dei dati
    output$d_pvt_t <- downloadHandler(
        filename = function() {
            paste("d_pvt_t.csv", sep = ";")
        },
        content = function(file) {
            write.csv(df_react_tst(), file, row.names = FALSE)
        }
    )
}

# ==============================================================================
# RUN APP
# ------------------------------------------------------------------------------

shinyApp(ui = ui, server = server)

# ==============================================================================
# DEPLOY
# ------------------------------------------------------------------------------

#rsconnect::deployApp(appName = "stock_pivot") 