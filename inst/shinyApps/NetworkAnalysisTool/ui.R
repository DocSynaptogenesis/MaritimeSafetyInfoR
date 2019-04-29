require(pacman)
p_load(shiny, readxl, visNetwork,
       igraph, data.table, scales,
       shinyjs, shinythemes, RColorBrewer,
       DT, V8, tidyverse,
       shinydashboard, dplyr)

jsResetCode <- "shinyjs.reset = function() {history.go(0)}"

shinyUI(fluidPage(
  theme = shinytheme("paper"),
  navbarPage("Network Explorer Version",
             tabPanel("Data Import",
                      br(),br(),#br(),
                      tags$head(
                        tags$style(HTML("
                                        @import url('//fonts.googleapis.com/css?family=Fjalla One|Cabin:400,700');
                                        h1 {
                                        font-family: 'Arial';
                                        text-align:center;
                                        font-weight: 250;
                                        line-height: 1.1;
                                        color: #33686f;
                                        }
                                        
                                        "))
                        ),
                      fluidRow(
                        br(),br(),br(),
                        shiny::column(width=8,
                                      offset = 2,
                                      h5("Instructions"),
                                      br(),
                                      p("First import a list of ships with MMSI and associated companies. Then import loitering table, which must include loitering ships (identified by MMSI) and loitering polygon."),
                                      br(),
                                      column(width=12,
                                             align = "center",
                                             fileInput("links1",
                                                       "Upload a list of MMSIs and companies",
                                                       #accept = c(".csv"),
                                                       buttonLabel = "Browse",
                                                       placeholder = "No file selected"),
                                             br(),
                                             fileInput("links2",
                                                       
                                                       "Upload a list of MMSIs and companies",
                                                       accept = c(".csv"),
                                                       buttonLabel = "Browse",
                                                       placeholder = "No file selected"),
                                             br(),
                                             useShinyjs(),
                                             extendShinyjs(text = jsResetCode),
                                             actionButton("reset_button", "Reset Application")
                                             )
                                    )
                      ),
                      mainPanel() #nothing goes here
                        ),
             tabPanel("Boats and Areas",
                      # sidebarLayout(
                      #   sidebarPanel(
                      #     width=0
                      #    ),
                      mainPanel(
                        tabsetPanel(
                          # tabPanel("Network", visNetworkOutput("network", height = "700px", width = "100%")
                          tabPanel("Network", bootstrapPage(
                            tags$head(tags$style("#network{height:85vh !important;}")), #renders the Network output as 85% of the screen
                            # absolutePanel(top = "30%", right = "20%", width = "20%", draggable = TRUE,
                            #               selectInput("measure1", "Choose a measure for sizing:", choices = c("None",
                            #                                                                                  "Degree")),
                            #               style = "opacity: 0.85; z-index: 1000;"
                            #               
                            # ),
                            visNetworkOutput("network")
                          )
                          ),
                          tabPanel("Data", DT::dataTableOutput("table"))
                        ),
                        width = 12
                      ) #mainPanel ends
                      # ) #sidebarLayout ends
             ),# tabPanel "Network" ends
             tabPanel("Coloitering Ships", 
                      # sidebarLayout(
                      #   sidebarPanel(
                      #     width=0
                      #   ),
                      mainPanel(
                        tabsetPanel(
                          #tabPanel("Network", visNetworkOutput("network2", height = "700px", width = "100%")
                          tabPanel("Network", bootstrapPage(
                            tags$head(tags$style("#network2{height:85vh !important;}")), #renders the Network output as 85% of the screen
                            absolutePanel(top = "30%", right = "20%", width = "20%", draggable = TRUE,
                                          selectInput("measure2", "Choose a measure for sizing:", choices = c("None",
                                                                                                              "Degree",
                                                                                                              "Betweenness")),
                                          style = "opacity: 0.85; z-index: 1000;"
                                          
                            ),
                            visNetworkOutput("network2")
                          )
                          ),
                          tabPanel("Data", DT::dataTableOutput("table2"))
                        ),
                        width = 12
                      ) #mainPanel ends
                      # ) #sidebarLayout ends
             ),# tabPanel "Coloitering Ships" ends
             tabPanel("Coorporates", 
                      # sidebarLayout(
                      #   sidebarPanel(
                      #     width=0
                      #   ),
                      mainPanel(
                        tabsetPanel(
                          #tabPanel("Network", visNetworkOutput("network3", height = "700px", width = "100%")
                          tabPanel("Network", bootstrapPage(
                            tags$head(tags$style("#network3{height:85vh !important;}")), #renders the Network output as 85% of the screen
                            absolutePanel(top = "30%", right = "20%", width = "20%", draggable = TRUE,
                                          selectInput("measure3", "Choose a measure for sizing:", choices = c("None",
                                                                                                              "Degree",
                                                                                                              "Betweenness")),
                                          style = "opacity: 0.85; z-index: 1000;"
                                          
                            ),
                            visNetworkOutput("network3")
                          )
                          ),
                          # tabPanel("Coorporates2", visNetworkOutput("network4", height = "700px", width = "100%")), #This network needs to be discussed 10/09
                          tabPanel("Data", DT::dataTableOutput("table3"))
                        ),
                        width = 12
                      ) #mainPanel ends
                      # ) #sidebarLayout ends
             ) # tabPanel "Coloitering Ships" ends
             # ,
             # tabPanel("Downloads")
                        ) # navbarPage ends
             )
  )
