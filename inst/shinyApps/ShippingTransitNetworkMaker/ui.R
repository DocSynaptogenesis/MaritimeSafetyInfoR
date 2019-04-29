packages <- c("shiny", "DT", "shinythemes",
              "leaflet", "visNetwork", "shinyBS",
              "chorddiag")
lapply(packages, require, character.only=TRUE)


# Define UI for application that draws a histogram
shinyUI(navbarPage("Shipping Transit Network Maker", theme = shinytheme("flatly"),
                   tabPanel("Load Data",
                            sidebarLayout(
                              sidebarPanel(
                                fileInput("file1", "Choose CSV File(s) for tracks",
                                          multiple = TRUE,
                                          accept = c(
                                            "text/csv",
                                            "text/comma-separated-values,text/plain",
                                            ".csv")
                                ),
                                checkboxInput(inputId = "seavision_data", label = "Seavision Data", value = TRUE),
                                selectInput(inputId = "mmsi_col", label = "Select MMSI Column", choices = NULL),
                                selectInput(inputId = "td_col", label = "Select Time-Date Column", choices = NULL),
                                selectInput(inputId = "sog_col", label = "Select SOG Column", choices = NULL),
                                selectInput(inputId = "lat_col", label = "Select Lat Column", choices = NULL),
                                selectInput(inputId = "long_col", label = "Select Long Column", choices = NULL)
                              ),
                              mainPanel(
                                DT::dataTableOutput("ship_table")
                              )
                            )
                   ),
                   tabPanel("Step 1: Create Object",
                            sidebarLayout(
                              sidebarPanel(
                                h4("Choose initial values for cutoff speed how many neighbors (k) to initally connect points to."),
                                sliderInput("loitering_cutoff", "Choose loitering cutoff speed:",
                                            min = 0, max = 5, value = 1.0, step = 0.1
                                ),
                                sliderInput("k_value", "Choose value for K:",
                                            min = 10, max = 2000, value = 500, step = 10),
                                h4("Once values above are chosen, click 'Create Object' and wait for results in main panel."),
                                bsButton("create_itin", "Create Object", style="success"),
                                HTML("<br><br>"),
                                checkboxInput("advanced_options", "Advanced Options", value = FALSE, width = NULL),
                                conditionalPanel(
                                  condition = "input.advanced_options == true",
                                  textInput("proj_info", "Projection Settings", 
                                            value = "+proj=aeqd +lat_0=6 +lon_0=149.8", 
                                            width = NULL, placeholder = NULL)
                                ),
                                HTML("<br>"),
                                uiOutput("downloadLXY_button")
                              ),
                              mainPanel(
                                #DT::dataTableOutput("itin_1")
                                verbatimTextOutput("sum_overall_tlocoh")
                              )
                            )),
                   tabPanel("Step 2: Create Isopleths",
                            sidebarLayout(
                              sidebarPanel(
                                h4("Choose method and values for creating isopleths:"),
                                radioButtons("iso_dist_type", "Choose method:", choices = list("R (Radius)" = "r",
                                                                                                           "K (nearest neighbors)" = "k",
                                                                                                           "A (adaptive/cumlative distance)" = "a"),
                                             selected = "a"),
                                numericInput("iso_num_input", "Enter numeric value for calculating isopleths (distance or number of neighbors):", 5000, min = 0, max = NA, step = NA,
                                             width = NULL),
                                h4("Choose which sets of isopleths to create:"),
                                checkboxInput("iso_99", "99% Isopleth", value = TRUE, width = NULL),
                                checkboxInput("iso_95", "95% Isopleth", value = TRUE, width = NULL),
                                checkboxInput("iso_75", "75% Isopleth", value = TRUE, width = NULL),
                                checkboxInput("iso_50", "50% Isopleth", value = TRUE, width = NULL),
                                checkboxInput("iso_10", "10% Isopleth", value = TRUE, width = NULL),
                                #HTML("<br>"),
                                h4("When ready click below and wait for results in summaries."),
                                bsButton("create_isos", "Create Isopleths", style = "success"),
                                HTML("<br>"),
                                uiOutput("download_isos"),
                                uiOutput("download_isos_ui")
                                
                              ),
                              mainPanel(
                                h3("Summaries:"),
                                column(2,
                                       h5(textOutput("isos_99_count")),
                                       h5(textOutput("isos_95_count")),
                                       h5(textOutput("isos_75_count")),
                                       h5(textOutput("isos_50_count")),
                                       h5(textOutput("isos_10_count"))
                                ),
                                column(6,
                                       verbatimTextOutput("isos_sum_out")
                                )
                              )
                            )),
                   tabPanel("Step 3: Generate Maps",
                            sidebarLayout(
                              sidebarPanel(
                                h3("Map Settings:"),
                                checkboxInput("anti_mer", "Wrap across antimeridian", value = FALSE),
                                HTML("<br>"),
                                bsButton("gen_map", "Generate Map", style = "success"),
                                HTML("<br>"),
                                HTML("<br>"),
                                uiOutput("map_as_rds")
                                
                              ),
                              mainPanel(
                                leafletOutput("init_iso_map")
                              ))
                   ),
                   tabPanel("Step 4: Label Isopleths",
                            sidebarLayout(
                              sidebarPanel(
                                selectInput("isos_name_choice", "Choose which Isopleth Level to Name", choices = list(
                                  "99% Isopleth" = "iso_99",
                                  "95% Isopleth" = "iso_95",
                                  "75% Isopleth" = "iso_75",
                                  "50% Isopleth" = "iso_50",
                                  "10% Isopleth" = "iso_10"
                                )),
                                HTML("<br>"),
                                h4("Label Isopleths"),
                                textOutput("iso_index"),
                                h5("Change label and classify location"),
                                textOutput("iso_name"),
                                textInput("new_iso_name", "New Name:", value = "", width = NULL, placeholder = NULL),
                                selectInput("area_type", "Area Type", choices = c("Port", "Anchorage", 
                                                                                  "Loitering Area", "Unknown"),
                                            selected = "Unknown"),
                                selectInput("keep_or_not", "Keep or Remove", choices = c("Keep", "Remove")),
                                bsButton("previous_isos", "Previous", style = "default", icon = icon("arrow-circle-left")),
                                bsButton("next_isos", "Next", style = "default", icon = icon("arrow-circle-right"))
                              ),
                              mainPanel(
                                tabsetPanel(type = "tabs",
                                            tabPanel("Map of Isopleth",
                                            leafletOutput("indiv_iso_map")
                                            ),
                                            tabPanel("Table",
                                                     #h3("Argh"))
                                                     DT::dataTableOutput("isos_choice_dt"))
                                            )
                              
                              )
                            )),
                   tabPanel("Step 5: Generate Graphs",
                            sidebarLayout(
                              sidebarPanel(
                                h4("First merge original ship data with Isopleths"),
                                bsButton("iso_merge", "Merge Ship Data with Isos", style = "success"),
                                h4("After table is created, click button below to create networks"),
                                bsButton("gen_nets", "Generate Networks", style = "danger", disabled = TRUE),
                                h4("After graph is created, click button below to allow you to see it on the map"),
                                bsButton("vis_net_map", "Visualize on Map", style = "danger", disabled = TRUE),
                                HTML("<br>"),
                                HTML("<br>"),
                                uiOutput("download_ui_nl"),
                                HTML("<br>"),
                                uiOutput("download_ui_el")
                              ),
                              mainPanel(
                                tabsetPanel(id = "step_5_tabs",
                                            tabPanel("Table", value = "table", height = "800px",
                                                     DT::dataTableOutput("ship_iso_table")),
                                            tabPanel("Network", height = "800px", value = "network",
                                                     #h3("Argh")),
                                                     visNetworkOutput("network", height = "800px")),
                                            tabPanel("Chord Diagram",height = "800px", value = "chord_d",
                                                     #h3("Argh")),
                                                     #verbatimTextOutput("test2")),
                                                     chorddiagOutput("chord_d")),
                                                     #DT::dataTableOutput("chord_d")),
                                            tabPanel("Map", height = "800px", value = "map",
                                                     leafletOutput("network_map", height = "800px")))
                              )
                            ))
                                
                                
                            
))
