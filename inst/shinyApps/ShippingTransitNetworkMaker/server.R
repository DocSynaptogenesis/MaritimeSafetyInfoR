options(shiny.maxRequestSize=100*1024^4) 

packages <- c("shiny", "DT", "shinythemes",
              "leaflet", "visNetwork", "shinyBS",
              "chorddiag", "anytime", "sp",
              "maptools", "rgdal", "rgeos",
              "tlocoh", "igraph", "leaflet",
              "rmapshaper")
lapply(packages, require, character.only=TRUE)

shinyServer(function(input, output, session) {
  
  reac_vals <- reactiveValues(counter = 1)
  
  mmsi_file <- reactive({
    
    inFile <- input$file1
    if (is.null(inFile))
      return(NULL)
    
    numfiles = nrow(inFile)
    
    for(f in 1:numfiles){
      if(input$seavision_data == TRUE){
        csv_out <- read.csv2(input$file1[[f, 'datapath']], header = TRUE, stringsAsFactors = FALSE, row.names = NULL)
        orig_col <- colnames(csv_out)
        csv_out <- csv_out[,1:8]
        colnames(csv_out) <- orig_col[2:9]
      }  else {
        csv_out <- read.csv2(input$file1[[f, 'datapath']], header = TRUE, stringsAsFactors = FALSE)
      }
      
      if(f == 1){
        csv_final <- csv_out
      } else(
        csv_final <- rbind(csv_final, csv_out)
      )
      
    }
      
    # if(input$seavision_data == TRUE){
    #   csv_out <- read.csv(inFile$datapath, header = TRUE, stringsAsFactors = FALSE, row.names = NULL)
    #   orig_col <- colnames(csv_out)
    #   csv_out <- csv_out[,1:8]
    #   colnames(csv_out) <- orig_col[2:9]
    # }  else {
    #   csv_out <- read.csv(inFile$datapath, header = TRUE, stringsAsFactors = FALSE)
    # }
    # 
    csv_final
    
  })
   
  choices_cols <- reactive({
    dataset1 <- mmsi_file()
    choices_cols <- colnames(dataset1)
    choices_cols
  })
  
  observe({
    updateSelectInput(session = session, inputId = "mmsi_col", choices = choices_cols())
    updateSelectInput(session = session, inputId = "td_col", choices = choices_cols())
    updateSelectInput(session = session, inputId = "sog_col", choices = choices_cols())
    updateSelectInput(session = session, inputId = "lat_col", choices = choices_cols())
    updateSelectInput(session = session, inputId = "long_col", choices = choices_cols())
    
  })
  
  output$ship_table <- renderDataTable({
    req(input$file1)
    
    dataset1 <- mmsi_file()
    datatable(dataset1)
    
  })
  
  output$test1 <- renderPrint({
    text_test <- stopped_lxy()
    sum_out <- summary(text_test)
    sum_out
  })
  
  ### Step 1
  
  observeEvent(input$create_itin, {
    req(input$file1)
    ais_data <- mmsi_file()
    if (is.null(ais_data))
      return(NULL)
    
    mmsi_col <- input$mmsi_col
    td_col <- input$td_col
    sog_col <- input$sog_col
    lat_col <- input$lat_col
    long_col <- input$long_col
    sog_cutoff <- input$loitering_cutoff
    
    proj_input_info <- input$proj_info
    k_value <- input$k_value
    
    ais_data$new_DateTime <- anytime(ais_data[,td_col])
    ais_data$new_lat <- as.numeric(ais_data[,lat_col])
    ais_data$new_long <- as.numeric(ais_data[,long_col])
    ais_data$new_mmsi <- as.integer(ais_data[,mmsi_col])
    ais_data$new_sog <- as.numeric(ais_data[,sog_col])
    
    vessel_data = ais_data[order(ais_data$new_mmsi, ais_data$new_DateTime),]
    vessel_stopped=vessel_data[which(as.numeric(vessel_data$new_sog)<= sog_cutoff),]
  
    vessel_stopped
    
    ship_data <- vessel_stopped[,c("new_mmsi", "new_long", "new_lat", "new_DateTime")]
    
    # Save ship data (without being overwritten as reactive value)
    reac_vals$ship_data <- ship_data
    
    ship_data$new_mmsi <- 17171717
    ship_data$new_DateTime <- "2001-01-01 09:00:00"
    ship_data$new_DateTime = as.POSIXct(strptime(as.character(ship_data$new_DateTime),"%Y-%m-%d %H:%M:%S", tz="GMT"))
    
    # Remove duplicates:
    ship_data <- unique(ship_data)
    colnames(ship_data) <- c("mmsi", "lon", "lat", "DT_1")
    
    # Convert coordinates from Long/Lat to UTM:
    coord.dec = SpatialPoints(cbind(as.numeric(ship_data$lon), as.numeric(ship_data$lat)), proj4string = CRS("+proj=longlat"))
    # Transform coordinates to UTM using Choose Projection http://projectionwizard.org/#
    coord.UTM <- spTransform(coord.dec, CRS(proj_input_info))
    # Create initial lxy object
    # Choose Projection http://projectionwizard.org/#
    stopped.lxy <- xyt.lxy(xy=as.data.frame(coord.UTM), dt=ship_data$DT_1, id="mmsi", proj4string=CRS(proj_input_info),dup.dt.check=FALSE)
    updateButton(session, "create_itin", style = "default")

    stopped <- lxy.nn.add(stopped.lxy, s=0, k=k_value)
    
    reac_vals$overall_tlocoh <- stopped
    
  })
  
  output$downloadLXY_button <- renderUI({
    if(!is.null(reac_vals$overall_tlocoh)) {
      tagList(
        h4("Download t-locoh object"),
        downloadButton("downloadLXY", "Download")
      )
    }
  })
  
  output$sum_overall_tlocoh <- renderPrint({
    reac_vals$overall_tlocoh
    text_test <- reac_vals$overall_tlocoh
    if(is.null(text_test))
      return(NULL)
    
    sum_out <- summary(text_test)
    sum_out
  })
  
  
  
  #### Step 2 #####
  
  observeEvent(input$create_isos,{
    init_tlocoh_obj <- reac_vals$overall_tlocoh
    if(is.null(init_tlocoh_obj))
      return(NULL)
    
    out_tlocoh_obj <- list()
    
    iso_cut <- input$iso_num_input
    iso_type <- input$iso_dist_type[[1]]
    
    iso_choices <- numeric()
    temp_iso_counter <- 1
    
    if(input$iso_10 == TRUE){
      iso_choices[temp_iso_counter] <- 0.10
      temp_iso_counter <- temp_iso_counter + 1
    }
    
    if(input$iso_50 == TRUE){
      iso_choices[temp_iso_counter] <- 0.50
      temp_iso_counter <- temp_iso_counter + 1
    }
    
    if(input$iso_75 == TRUE){
      iso_choices[temp_iso_counter] <- 0.75
      temp_iso_counter <- temp_iso_counter + 1
    }
    
    if(input$iso_95 == TRUE){
      iso_choices[temp_iso_counter] <- 0.95
      temp_iso_counter <- temp_iso_counter + 1
    }
    
    if(input$iso_99 == TRUE){
      iso_choices[temp_iso_counter] <- 0.99
      temp_iso_counter <- temp_iso_counter + 1
    }
    
    if(iso_type == "a"){
      invisible(capture.output(itinerary_data_combined <- lxy.lhs(init_tlocoh_obj, a = iso_cut, s=0,iso.add=T, iso.levels = iso_choices, status = FALSE)))
      #itinerary_data_combined <- lxy.lhs(init_tlocoh_obj, a = iso_cut, s=0,iso.add=T, iso.levels = iso_choices)
      
    } else if (iso_type == "r") {
      invisible(capture.output(itinerary_data_combined <- lxy.lhs(init_tlocoh_obj, r = iso_cut, s=0,iso.add=T, iso.levels = iso_choices, status = FALSE)))
    } else if(iso_type == "k"){
      invisible(capture.output(itinerary_data_combined <- lxy.lhs(init_tlocoh_obj, k = iso_cut, s=0,iso.add=T, iso.levels = iso_choices, status = FALSE)))
    }
    
    if(input$iso_99 == TRUE){
      polys_comb <- itinerary_data_combined[[1]]$isos[[1]]$polys
      polys_iso <- subset(polys_comb, polys_comb$iso.level == 0.99)
      if(nrow(polys_iso) > 0){
        temp_polys <- disaggregate(polys_iso)
        temp_polys <- spTransform(temp_polys, CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs"))
        temp_count <- nrow(temp_polys)
        reac_vals$iso_99$count <- temp_count
        reac_vals$iso_99$polys <- temp_polys
      } else{
        reac_vals$iso_99$warning <- "Unable to create for this value"
      }
    }
    if(input$iso_95 == TRUE){
      polys_comb <- itinerary_data_combined[[1]]$isos[[1]]$polys
      polys_iso <- subset(polys_comb, polys_comb$iso.level == 0.95)
      if(nrow(polys_iso) > 0){
        temp_polys <- disaggregate(polys_iso)
        temp_polys <- spTransform(temp_polys, CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs"))
        temp_count <- nrow(temp_polys)
        reac_vals$iso_95$count <- temp_count
        reac_vals$iso_95$polys <- temp_polys
      } else {
        reac_vals$iso_95$warning <- "Unable to create for this value"
      }
    }
    if(input$iso_75 == TRUE){
      polys_comb <- itinerary_data_combined[[1]]$isos[[1]]$polys
      polys_iso <- subset(polys_comb, polys_comb$iso.level == 0.75)
      if(nrow(polys_iso) > 0){
        temp_polys <- disaggregate(polys_iso)
        temp_polys <- spTransform(temp_polys, CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs"))
        temp_count <- nrow(temp_polys)
        reac_vals$iso_75$count <- temp_count
        reac_vals$iso_75$polys <- temp_polys
      } else {
        reac_vals$iso_75$warning <- "Unable to create for this value"
      }
    }
    if(input$iso_50 == TRUE){
      polys_comb <- itinerary_data_combined[[1]]$isos[[1]]$polys
      polys_iso <- subset(polys_comb, polys_comb$iso.level == 0.50)
      if(nrow(polys_iso) > 0){
        temp_polys <- disaggregate(polys_iso)
        temp_polys <- spTransform(temp_polys, CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs"))
        temp_count <- nrow(temp_polys)
        reac_vals$iso_50$count <- temp_count
        reac_vals$iso_50$polys <- temp_polys
      } else {
        reac_vals$iso_50$warning <- "Unable to create for this value"
      }
    }
    if(input$iso_10 == TRUE){
      polys_comb <- itinerary_data_combined[[1]]$isos[[1]]$polys
      polys_iso <- subset(polys_comb, polys_comb$iso.level == 0.10)
      if(nrow(polys_iso) > 0){
        temp_polys <- disaggregate(polys_iso)
        temp_polys <- spTransform(temp_polys, CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs"))
        temp_count <- nrow(temp_polys)
        reac_vals$iso_10$count <- temp_count
        reac_vals$iso_10$polys <- temp_polys
      } else {
        reac_vals$iso_10$warning <- "Unable to create for this value"
      }
    }
    updateButton(session, "create_isos", style = "default")
    
    reac_vals$out_tlocoh_obj <- itinerary_data_combined
    
  })
  
  output$download_isos <- renderUI({
    if(!is.null(reac_vals$out_tlocoh_obj)) {
      h4("Download t-locoh object")
    }
  })
  
  
  output$isos_99_count <- renderText({
    temp_count <- reac_vals$iso_99$count
    if(is.null(temp_count))
      return("Isos 99:")
    
    paste("Isos 99: ", temp_count, sep = "")
    
  })
  
  output$download_isos_ui <- renderUI({
    if(!is.null(reac_vals$out_tlocoh_obj)) {
      downloadButton("download_iso_data", "Download Isopleths")
    }
  })
  
  output$isos_95_count <- renderText({
    temp_count <- reac_vals$iso_95$count
    if(is.null(temp_count))
      return("Isos 95:")
    
    paste("Isos 95: ", temp_count, sep = "")
  })
  
  output$isos_75_count <- renderText({
    temp_count <- reac_vals$iso_75$count
    if(is.null(temp_count))
      return("Isos 75:")
    
    paste("Isos 75: ", temp_count, sep = "")
  })
  
  output$isos_50_count <- renderText({
    temp_count <- reac_vals$iso_50$count
    if(is.null(temp_count))
      return("Isos 50:")
    
    paste("Isos 50: ", temp_count, sep = "")
  })
  
  output$isos_10_count <- renderText({
    temp_count <- reac_vals$iso_10$count
    if(is.null(temp_count))
      return("Isos 10:")
    
    paste("Isos 10: ", temp_count, sep = "")
  })
  
  output$isos_sum_out <- renderPrint({
    
    if(is.null(reac_vals$out_tlocoh_obj))
      return(NULL)
    temp_out <- summary(reac_vals$out_tlocoh_obj)
    sum_out <- summary(reac_vals$out_tlocoh_obj)
    sum_out
    
  })
  
  #### Step 3 ####
  
  output$init_iso_map <- renderLeaflet({
    leaflet("init_iso_map") %>%
      setView(lng = 0, lat = 0, zoom = 2) %>%
      addProviderTiles(providers$CartoDB.Positron, group = "Grey") %>%
      addProviderTiles(providers$OpenStreetMap.Mapnik, group = "OSM") %>%
      addProviderTiles(providers$Esri.WorldImagery, group = "Imagery") %>%
      addProviderTiles(providers$Esri.OceanBasemap, group = "Ocean") %>%
      
      addLayersControl(
        baseGroups = c("Grey", "OSM", "Imagery", "Ocean"),
        options = layersControlOptions(collapsed = FALSE)
      )
    
    
  })
  
  observeEvent(input$gen_map, {
    out_locohs <- reac_vals$out_tlocoh_obj
    
    if(is.null(out_locohs))
      return(NULL)
    
    if(!is.null(reac_vals$iso_99$polys)){
      
      polys_test5 <- reac_vals$iso_99$polys
      
      l_df5 <-as.data.frame(1:reac_vals$iso_99$count)
      colnames(l_df5) <- c("label")
      l_df5$labels2 <- paste("id_iso_99_", l_df5$label, sep = "")
      l_df5$id2 <- paste("id_iso_99_", l_df5$label, sep = "")
      
      polys_test5$labels2 <- l_df5$labels2
      polys_test5$id2 <- l_df5$id2
      
      if(input$anti_mer == TRUE){
        isos_ll_99_test <- spTransform(polys_test5, CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs +lon_wrap=180"))
        
      } else {
        isos_ll_99_test <- spTransform(polys_test5, CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs"))
      }
      
      trueCentroids = gCentroid(isos_ll_99_test,byid=TRUE)
      cent_2 <- as.data.frame(trueCentroids)
      l_df5$long <- cent_2$x
      l_df5$lat <- cent_2$y
      l_df5$Location_Type <- "Unknown"
      l_df5$Keep <- "Keep"
      
      reac_vals$iso_99$polys2 <- isos_ll_99_test
      reac_vals$iso_99$l_df <- l_df5
      
      
    }
    if(!is.null(reac_vals$iso_95$polys)){
      
      polys_test4 <- reac_vals$iso_95$polys
      
      l_df4 <-as.data.frame(1:reac_vals$iso_95$count)
      colnames(l_df4) <- c("label")
      l_df4$labels2 <- paste("id_iso_95_", l_df4$label, sep = "")
      l_df4$id2 <- paste("id_iso_95_", l_df4$label, sep = "")
      
      polys_test4$labels2 <- l_df4$labels2
      polys_test4$id2 <- l_df4$id2
      
      if(input$anti_mer == TRUE){
        isos_ll_95_test <- spTransform(polys_test4, CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs +lon_wrap=180"))
        
      } else {
        isos_ll_95_test <- spTransform(polys_test4, CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs"))
      }
      
      trueCentroids = gCentroid(isos_ll_95_test,byid=TRUE)
      cent_2 <- as.data.frame(trueCentroids)
      l_df4$long <- cent_2$x
      l_df4$lat <- cent_2$y
      l_df4$Location_Type <- "Unknown"
      l_df4$Keep <- "Keep"
      
      reac_vals$iso_95$polys2 <- isos_ll_95_test
      reac_vals$iso_95$l_df <- l_df4
    }
    
    if(!is.null(reac_vals$iso_75$polys)){
      polys_test3 <- reac_vals$iso_75$polys
      
      l_df3 <-as.data.frame(1:reac_vals$iso_75$count)
      colnames(l_df3) <- c("label")
      l_df3$labels2 <- paste("id_iso_75_", l_df3$label, sep = "")
      l_df3$id2 <- paste("id_iso_75_", l_df3$label, sep = "")
      
      polys_test3$labels2 <- l_df3$labels2
      polys_test3$id2 <- l_df3$id2
      
      if(input$anti_mer == TRUE){
        isos_ll_75_test <- spTransform(polys_test3, CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs +lon_wrap=180"))
        
      } else {
        isos_ll_75_test <- spTransform(polys_test3, CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs"))
      }
      
      trueCentroids = gCentroid(isos_ll_75_test,byid=TRUE)
      cent_2 <- as.data.frame(trueCentroids)
      l_df3$long <- cent_2$x
      l_df3$lat <- cent_2$y
      l_df3$Location_Type <- "Unknown"
      l_df3$Keep <- "Keep"
      
      reac_vals$iso_75$polys2 <- isos_ll_75_test
      reac_vals$iso_75$l_df <- l_df3
    }
    
    if(!is.null(reac_vals$iso_50$polys)){
      polys_test2 <- reac_vals$iso_50$polys
      
      l_df2 <-as.data.frame(1:reac_vals$iso_50$count)
      colnames(l_df2) <- c("label")
      l_df2$labels2 <- paste("id_iso_50_", l_df2$label, sep = "")
      l_df2$id2 <- paste("id_iso_50_", l_df2$label, sep = "")
      
      polys_test2$labels2 <- l_df2$labels2
      polys_test2$id2 <- l_df2$id2
      
      
      if(input$anti_mer == TRUE){
        isos_ll_50_test <- spTransform(polys_test2, CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs +lon_wrap=180"))
        
      } else {
        isos_ll_50_test <- spTransform(polys_test2, CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs"))
      }
      
      trueCentroids = gCentroid(isos_ll_50_test,byid=TRUE)
      cent_2 <- as.data.frame(trueCentroids)
      l_df2$long <- cent_2$x
      l_df2$lat <- cent_2$y
      l_df2$Location_Type <- "Unknown"
      l_df2$Keep <- "Keep"
      
      reac_vals$iso_50$polys2 <- isos_ll_50_test
      reac_vals$iso_50$l_df <- l_df2
    }
    
    if(!is.null(reac_vals$iso_10$polys)){
      polys_test1 <- reac_vals$iso_10$polys
      l_df1 <-as.data.frame(1:reac_vals$iso_10$count)
      colnames(l_df1) <- c("label")
      l_df1$labels2 <- paste("id_iso_10_", l_df1$label, sep = "")
      l_df1$id2 <- paste("id_iso_10_", l_df1$label, sep = "")
      
      polys_test1$labels2 <- l_df1$labels2
      polys_test1$id2 <- l_df1$id2
      
      
      if(input$anti_mer == TRUE){
        isos_ll_10_test <- spTransform(polys_test1, CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +lon_wrap=180"))
        
      } else {
        isos_ll_10_test <- spTransform(polys_test1, CRS("+proj=longlat +ellps=WGS84 +datum=WGS84"))
      }
      
      trueCentroids = gCentroid(isos_ll_10_test,byid=TRUE)
      cent_2 <- as.data.frame(trueCentroids)
      l_df1$long <- cent_2$x
      l_df1$lat <- cent_2$y
      l_df1$Location_Type <- "Unknown"
      l_df1$Keep <- "Keep"
      
      reac_vals$iso_10$polys2 <- isos_ll_10_test
      reac_vals$iso_10$l_df <- l_df1
    }
    reac_vals$mapped_isos <- 1
    updateButton(session, "gen_map",style = "default")
  })
  

  observeEvent(input$gen_map,{
    
    list_counter <- 1
    group_list <- list()
    
    if(!is.null(reac_vals$iso_99$polys2)){
      iso_99_polys <- reac_vals$iso_99$polys2
      
      
      leafletProxy("init_iso_map") %>%
        addPolygons(data = iso_99_polys, color = "#F43E3E", fillOpacity = 0.1, smoothFactor = 0.5, group = "Iso 99")
      
      group_list[list_counter] <- "Iso 99"
      list_counter <- list_counter + 1
    }
    
    if(!is.null(reac_vals$iso_95$polys2)){
      iso_95_polys <- reac_vals$iso_95$polys2
      
      
      leafletProxy("init_iso_map") %>%
        addPolygons(data = iso_95_polys, color = "#F56847", fillOpacity = 0.1, smoothFactor = 0.5, group = "Iso 95")
      
      group_list[list_counter] <- "Iso 95"
      list_counter <- list_counter + 1
    }
    if(!is.null(reac_vals$iso_75$polys2)){
      iso_75_polys <- reac_vals$iso_75$polys2
      
      
      leafletProxy("init_iso_map") %>%
        addPolygons(data = iso_75_polys, color = "#F7A156", fillOpacity = 0.1, smoothFactor = 0.5, group = "Iso 75")
      
      group_list[list_counter] <- "Iso 75"
      list_counter <- list_counter + 1
    }
    
    if(!is.null(reac_vals$iso_50$polys2)){
      iso_50_polys <- reac_vals$iso_50$polys2
      
      
      leafletProxy("init_iso_map") %>%
        addPolygons(data = iso_50_polys, color = "#F8C360", fillOpacity = 0.1, smoothFactor = 0.5, group = "Iso 50")
      
      group_list[list_counter] <- "Iso 50"
      list_counter <- list_counter + 1
    }
    
    if(!is.null(reac_vals$iso_10$polys2)){
      iso_10_polys <- reac_vals$iso_10$polys2
      
      
      leafletProxy("init_iso_map") %>%
        addPolygons(data = iso_10_polys, color = "#FAF170", fillOpacity = 0.1, smoothFactor = 0.5, group = "Iso 10")
      
      group_list[list_counter] <- "Iso 10"
      list_counter <- list_counter + 1
    }
    
    # Temp Save 
    #saveRDS(reac_vals, file = "temp_reac_vals.rds")
    
    leafletProxy("init_iso_map") %>%
      clearControls() %>%
      addLayersControl(
        baseGroups = c("Grey", "OSM", "Imagery", "Ocean"),
        overlayGroups = group_list,
        options = layersControlOptions(collapsed = FALSE)
      )
  })
  
  #### Step 4 Label Maps ####
  
  # Choose which Isopleth group to label
  working_isos_index <- reactive({
    isos_choice <- input$isos_name_choice
    if(!is.element(input$isos_name_choice, names(reac_vals)))
       return(NULL)
       
       
    out_index <- isos_choice
    out_index
  })
  
  #### Reactive Values for Chosen Iso
  
  
  
  # Have person enter in new name
  output$iso_name <- renderText({
    reac_counter <- reac_vals$counter
    iso_choice <- working_isos_index()
    poly_name <- reac_vals[[iso_choice]]$l_df[reac_counter, c("labels2")]
    out_name <- paste0("Original Name: ", poly_name)
    out_name
    
  })
  
  
  # Gives which one you are looking at
  output$iso_index <- renderText({
    iso_num <- reac_vals$counter
    iso_choice <- working_isos_index()
    n_isos_count <- nrow(reac_vals[[iso_choice]]$l_df)
    out_text <- paste0(iso_num, " of ", n_isos_count, " polygons")
    out_text
  })
  
  # Goes back one
  observeEvent(input$previous_isos, {
    iso_choice <- working_isos_index()
    n_isos_count <- nrow(reac_vals[[iso_choice]]$l_df)
    new_input <- input$new_iso_name
    temp_counter <- reac_vals$counter
    new_area_type <- input$area_type
    new_keep <- input$keep_or_not
    if(new_input != ""){
      reac_vals[[iso_choice]]$l_df[temp_counter, c("labels2")] <- new_input
      updateTextInput(session, "new_iso_name", value = "")
    }
    reac_vals[[iso_choice]]$l_df[temp_counter, c("Keep")] <- new_keep
    reac_vals[[iso_choice]]$l_df[temp_counter, c("Location_Type")] <- new_area_type
    
    if(reac_vals$counter == 1){
      reac_vals$counter <- n_isos_count
      old_location_type <- reac_vals[[iso_choice]]$l_df[reac_vals$counter, c("Location_Type")]
      old_keep <- reac_vals[[iso_choice]]$l_df[reac_vals$counter, c("Keep")]
      updateSelectInput(session, "area_type", selected = old_location_type)
      updateSelectInput(session, "keep_or_not", selected = old_keep)
    } else {
      reac_vals$counter <- reac_vals$counter - 1
      old_location_type <- reac_vals[[iso_choice]]$l_df[reac_vals$counter, c("Location_Type")]
      updateSelectInput(session, "area_type", selected = old_location_type)
      old_keep <- reac_vals[[iso_choice]]$l_df[reac_vals$counter, c("Keep")]
      updateSelectInput(session, "keep_or_not", selected = old_keep)
    }
  })
  
  # Goes forward one
  observeEvent(input$next_isos, {
    ## Choose wether to update name
    iso_choice <- working_isos_index()
    new_input <- input$new_iso_name
    temp_counter <- reac_vals$counter
    new_area_type <- input$area_type
    new_keep <- input$keep_or_not
    if(new_input != ""){
      reac_vals[[iso_choice]]$l_df[temp_counter, c("labels2")] <- new_input
      updateTextInput(session, "new_iso_name", value = "")
    }
    
    reac_vals[[iso_choice]]$l_df[temp_counter, c("Location_Type")] <- new_area_type
    reac_vals[[iso_choice]]$l_df[temp_counter, c("Keep")] <- new_keep
    n_isos_count <- nrow(reac_vals[[iso_choice]]$l_df)
    
    if(reac_vals$counter == n_isos_count){
      reac_vals$counter <- 1
      old_location_type <- reac_vals[[iso_choice]]$l_df[reac_vals$counter, c("Location_Type")]
      updateSelectInput(session, "area_type", selected = old_location_type)
      old_keep <- reac_vals[[iso_choice]]$l_df[reac_vals$counter, c("Keep")]
      updateSelectInput(session, "keep_or_not", selected = old_keep)
    } else {
      reac_vals$counter <- reac_vals$counter + 1
      old_location_type <- reac_vals[[iso_choice]]$l_df[reac_vals$counter, c("Location_Type")]
      updateSelectInput(session, "area_type", selected = old_location_type)
      old_keep <- reac_vals[[iso_choice]]$l_df[reac_vals$counter, c("Keep")]
      updateSelectInput(session, "keep_or_not", selected = old_keep)
    }
  })
  
  
  
  # Gives Table of Iso's with centroids
  output$isos_choice_dt <- renderDataTable({
    iso_choice <- working_isos_index()
    dt_named_isos <- reac_vals[[iso_choice]]
    
    if(is.null(dt_named_isos))
      return(NULL)
    
    datatable(dt_named_isos$l_df)
  })
  
  #### Map for Each Iso ###
  
  output$indiv_iso_map <- renderLeaflet({
    iso_choice <- working_isos_index()
    temp1 <- reac_vals[[iso_choice]]
    temp_counter <- reac_vals$counter
    temp_sub <- temp1$l_df[temp_counter,]
    temp_id <- temp_sub[1,c("id2")]
    temp_name <- temp_sub[1,c("labels2")]
    temp_poly <- subset(temp1$polys2, temp1$polys2$id2 == temp_id)
    other_polys <- subset(temp1$polys2, temp1$polys2$id2 != temp_id)
    other_names <- subset(temp1$l_df, temp1$l_df$labels2 != temp_name)
    temp_long <- temp1$l_df[temp_counter,c("long")]
    temp_lat <- temp1$l_df[temp_counter,c("lat")]
    
    if(is.null(temp_poly)){
      m <- leaflet("indiv_iso_map") %>%
        setView(lng = 0, lat = 0, zoom = 2) %>%
        addProviderTiles(providers$CartoDB.Positron, group = "Grey") %>%
        addProviderTiles(providers$OpenStreetMap.Mapnik, group = "OSM") %>%
        addProviderTiles(providers$Esri.WorldImagery, group = "Imagery") %>%
        addProviderTiles(providers$Esri.OceanBasemap, group = "Ocean") %>%
        addProviderTiles(providers$OpenInfraMap.Telecom, group = "Telecom") %>%
        addProviderTiles(providers$OpenInfraMap.Petroleum, group = "Petroleum") %>%
        
        addLayersControl(
          baseGroups = c("Grey", "OSM", "Imagery", "Ocean"),
          overlayGroups = c("Target Iso Overlay", "Other Isos Overlay", "Telecom", "Petroleum"),
          options = layersControlOptions(collapsed = TRUE)
        )
      
    } else {
      m <- leaflet("indiv_iso_map") %>%
        setView(lng = temp_long, lat = temp_lat, zoom = 10) %>%
        addProviderTiles(providers$CartoDB.Positron, group = "Grey") %>%
        addProviderTiles(providers$OpenStreetMap.Mapnik, group = "OSM") %>%
        addProviderTiles(providers$Esri.WorldImagery, group = "Imagery") %>%
        addProviderTiles(providers$Esri.OceanBasemap, group = "Ocean") %>%
        addProviderTiles(providers$OpenInfraMap.Telecom, group = "Telecom") %>%
        addProviderTiles(providers$OpenInfraMap.Petroleum, group = "Petroleum") %>%
        addPolygons(data = other_polys, color = "#008000", fillOpacity = 0.3, smoothFactor = 0.5, popup = other_names$labels2, group = "Other Isos Overlay") %>%
        addPolygons(data = temp_poly, color = "#F43E3E", fillOpacity = 0.7, smoothFactor = 0.5, popup = temp_name, group = "Target Iso Overlay") %>%
        
        addLayersControl(
          baseGroups = c("Grey", "OSM", "Imagery", "Ocean"),
          overlayGroups = c("Target Iso Overlay", "Other Isos Overlay", "Telecom", "Petroleum"),
          options = layersControlOptions(collapsed = TRUE)
        )
      
    }
    
    m
  })
  
  # For Map
  
  observeEvent(input$gen_map, {
    iso_choice <- working_isos_index()
    temp1 <- reac_vals[[iso_choice]]
    
    if(is.null(temp1))
      return(NULL)
    
    temp_counter <- reac_vals$counter
    temp_sub <- temp1$l_df[temp_counter,]
    temp_id <- temp_sub[1,c("id2")]
    temp_name <- temp_sub[1,c("labels2")]
    temp_poly <- subset(temp1$polys2, temp1$polys2$id2 == temp_id)
    other_polys <- subset(temp1$polys2, temp1$polys2$id2 != temp_id)
    other_names <- subset(temp1$l_df, temp1$l_df$labels2 != temp_name)
    temp_long <- temp1$l_df[temp_counter,c("long")]
    temp_lat <- temp1$l_df[temp_counter,c("lat")]
    ####
    
    
    if(is.null(temp_poly))
      return(NULL)
    
    proxy <- leafletProxy("indiv_iso_map")
    
    proxy %>%
      clearGroup("Target Iso Overlay") %>%
      clearGroup("Other Isos Overlay") %>%
      setView(lng = temp_long, lat = temp_lat, zoom = 10) %>%
      addPolygons(data = other_polys, color = "#008000", fillOpacity = 0.3, smoothFactor = 0.5, popup = other_names$labels2, group = "Other Isos Overlay") %>%
      addPolygons(data = temp_poly, color = "#F43E3E", fillOpacity = 0.7, smoothFactor = 0.5, popup = temp_name, group = "Target Iso Overlay")
    
  })
  
  observeEvent(input$isos_name_choice, {
    iso_choice <- input$isos_name_choice
    temp1 <- reac_vals[[iso_choice]]
    temp_counter <- reac_vals$counter
    temp_sub <- temp1$l_df[temp_counter,]
    temp_id <- temp_sub[1,c("id2")]
    temp_name <- temp_sub[1,c("labels2")]
    temp_poly <- subset(temp1$polys2, temp1$polys2$id2 == temp_id)
    other_polys <- subset(temp1$polys2, temp1$polys2$id2 != temp_id)
    other_names <- subset(temp1$l_df, temp1$l_df$labels2 != temp_name)
    temp_long <- temp1$l_df[temp_counter,c("long")]
    temp_lat <- temp1$l_df[temp_counter,c("lat")]
    
    
    if(is.null(temp_poly))
      return(NULL)
    
    proxy <- leafletProxy("indiv_iso_map")
    proxy %>%
      clearGroup("Target Iso Overlay") %>%
      clearGroup("Other Isos Overlay") %>%
      setView(lng = temp_long, lat = temp_lat, zoom = 10) %>%
      addPolygons(data = other_polys, color = "#008000", fillOpacity = 0.3, smoothFactor = 0.5, popup = other_names$labels2, group = "Other Isos Overlay") %>%
      addPolygons(data = temp_poly, color = "#F43E3E", fillOpacity = 0.7, smoothFactor = 0.5, popup = temp_name, group = "Target Iso Overlay")
    
  })
  
  # Map change for next isos
  observeEvent(input$next_isos, {
    iso_choice <- working_isos_index()
    temp1 <- reac_vals[[iso_choice]]
    temp_counter <- reac_vals$counter
    temp_sub <- temp1$l_df[temp_counter,]
    temp_id <- temp_sub[1,c("id2")]
    temp_name <- temp_sub[1,c("labels2")]
    temp_poly <- subset(temp1$polys2, temp1$polys2$id2 == temp_id)
    other_polys <- subset(temp1$polys2, temp1$polys2$id2 != temp_id)
    other_names <- subset(temp1$l_df, temp1$l_df$labels2 != temp_name)
    temp_long <- temp1$l_df[temp_counter,c("long")]
    temp_lat <- temp1$l_df[temp_counter,c("lat")]
    
    if(is.null(temp_poly))
      return(NULL)
    
    proxy <- leafletProxy("indiv_iso_map")
    
    proxy %>%
      clearGroup("Target Iso Overlay") %>%
      clearGroup("Other Isos Overlay") %>%
      setView(lng = temp_long, lat = temp_lat, zoom = 10) %>%
      addPolygons(data = other_polys, color = "#008000", fillOpacity = 0.3, smoothFactor = 0.5, popup = other_names$labels2, group = "Other Isos Overlay") %>%
      addPolygons(data = temp_poly, color = "#F43E3E", fillOpacity = 0.7, smoothFactor = 0.5, popup = temp_name, group = "Target Iso Overlay")
    
  })
  
  # Map change for previous isos
  observeEvent(input$previous_isos, {
    iso_choice <- working_isos_index()
    temp1 <- reac_vals[[iso_choice]]
    temp_counter <- reac_vals$counter
    temp_sub <- temp1$l_df[temp_counter,]
    temp_id <- temp_sub[1,c("id2")]
    temp_name <- temp_sub[1,c("labels2")]
    temp_poly <- subset(temp1$polys2, temp1$polys2$id2 == temp_id)
    other_polys <- subset(temp1$polys2, temp1$polys2$id2 != temp_id)
    other_names <- subset(temp1$l_df, temp1$l_df$labels2 != temp_name)
    temp_long <- temp1$l_df[temp_counter,c("long")]
    temp_lat <- temp1$l_df[temp_counter,c("lat")]
    
    if(is.null(temp_poly))
      return(NULL)
    
    proxy <- leafletProxy("indiv_iso_map")
    
    proxy %>%
      clearGroup("Target Iso Overlay") %>%
      clearGroup("Other Isos Overlay") %>%
      setView(lng = temp_long, lat = temp_lat, zoom = 10) %>%
      addPolygons(data = other_polys, color = "#008000", fillOpacity = 0.4, smoothFactor = 0.5, popup = other_names$labels2, group = "Other Isos Overlay") %>%
      addPolygons(data = temp_poly, color = "#F43E3E", fillOpacity = 0.7, smoothFactor = 0.5, popup = temp_name, group = "Target Iso Overlay")
    
  })
  
  
  # For Table
  
  observeEvent(input$iso_merge, {
    
    # "new_mmsi", "new_long", "new_lat", "new_DateTime"
    iso_choice <- working_isos_index()
    
    stopped_data2 <- reac_vals$ship_data
    polys_1 <- reac_vals[[iso_choice]]$polys2
    l_df1 <- reac_vals[[iso_choice]]$l_df
    
    
    #saveRDS(polys_1, file = "polys_1.rds")
    #saveRDS(stopped_data2, file = "stopped_data2.rds")
    #saveRDS(l_df1, file = "l_df1.rds")
    
    new_proj_input_info <- polys_1@proj4string
    
    Test_Point = stopped_data2[,c("new_long","new_lat")]
    Test_Point_sp = SpatialPoints(Test_Point, proj4string=CRS("+proj=longlat +ellps=WGS84 +datum=WGS84"))
    Test_Point_sp_tf = spTransform(Test_Point_sp, new_proj_input_info)
    
    temp_int2 <- over(Test_Point_sp_tf, polys_1)
    
    stopped_data2$id2 <- temp_int2$id2
    stopped_data2$id2[which(is.na(stopped_data2$id2))] <- "unknown"
    
    stopped_data_unique <- stopped_data2[,c("new_mmsi", "id2")]
    stopped_data_unique <- unique(stopped_data_unique)
    
    for(p in 1:nrow(l_df1)){
      temp_poi <- l_df1[p, c("id2")]
      sub_ship <- subset(stopped_data_unique, stopped_data_unique$id2 == temp_poi)
      temp_ship_count <- nrow(sub_ship)
      l_df1[p, c("ship_count")] <- temp_ship_count
      
      if(temp_ship_count < 20){
        temp_new_name_begin <- paste("<b>Polyname: </b>", temp_poi, "<br>",
                                     "<b>Ship Count: </b>", temp_ship_count, "<br>",
                                     "<b>Ships: </b><br>", sep = "")
        ships_conc <- paste(sub_ship$new_mmsi, collapse = "<br>")
        complete_new_name <- paste(temp_new_name_begin, ships_conc, sep = "")
        l_df1[p,"map_ship_name"] <- complete_new_name
      } else {
        temp_new_name <- paste("<b>Polyname: </b>", temp_poi, "<br>",
                               "<b>Ship Count: </b>", temp_ship_count, sep = "")
        l_df1[p,"map_ship_name"] <- temp_new_name
        
      }
    }
    
    reac_vals[[iso_choice]]$l_df_merge <- l_df1
    
    updateButton(session, "iso_merge", style = "default")
    updateButton(session, "gen_nets", style = "success", disabled = FALSE)
    
    reac_vals$ship_poly_df <- stopped_data2
    
  })
  
  output$ship_iso_table <- renderDataTable({
    
    datatable(reac_vals$ship_poly_df)
    
  })
  
  # Gen Networks
  
  observeEvent(input$gen_nets, {
    ship_poly_df <- reac_vals$ship_poly_df
    unique_mmsis <- unique(ship_poly_df$new_mmsi)
    for(ship in 1:length(unique_mmsis)){
      temp_mmsi <- unique_mmsis[ship]
      sub_ship_df <- subset(ship_poly_df, ship_poly_df$new_mmsi == temp_mmsi)
      sub_ship_df <- sub_ship_df[order(sub_ship_df$new_DateTime), ]
      sub_ship_df <- subset(sub_ship_df, sub_ship_df$id2 != "unknown")
      if(nrow(sub_ship_df) > 1){
        for(l in 2:nrow(sub_ship_df)){
          lm1 <- l - 1
          orig_l <- sub_ship_df[lm1, c("id2")]
          next_l <- sub_ship_df[l, c("id2")]
          if(orig_l != next_l){
            temp_el <- cbind(orig_l, next_l, temp_mmsi)
            if (!exists("out_el")){
              out_el <- temp_el
            } else{
              out_el <- rbind(out_el, temp_el)
            }
          }
        }
      }
    }
    
    reac_vals$edge_list_mmsi <- out_el
    
    ### Create Graph ###
    out_el_df <- as.data.frame(out_el, stringsAsFactors = FALSE)
    out_el_count <- dplyr::count_(out_el_df, c("orig_l", "next_l"))
    temp_m <- as.matrix(out_el_count)
    g1 <- graph_from_edgelist(temp_m[,1:2], directed = TRUE)
    E(g1)$weight <- temp_m[,3]
    V(g1)$betw <- betweenness(g1, v=V(g1), directed = TRUE, normalized = TRUE, weights = NULL)
    V(g1)$total_deg <- degree(g1, v=V(g1), mode = c("total"))
    
    g1_vis <- toVisNetworkData(g1, idToLabel = TRUE)
    
    iso_choice <- working_isos_index()
    
    nodes.sub <- g1_vis$nodes
    links.sub <- g1_vis$edges
    node_info <- reac_vals[[iso_choice]]$l_df_merge
    node_info <- node_info[,c("labels2","id2", "Location_Type", "long", "lat", "map_ship_name", "ship_count")]
    nodes.sub2 <- merge(nodes.sub, node_info, by.x = "id", by.y = "id2")
    nodes.sub2$label <- nodes.sub2$labels2
    nodes.sub2$color.border <- "black"
    nodes.sub2$color.background <- ifelse(nodes.sub2$Location_Type == "Port", "steelblue", "lightgrey")
    nodes.sub2$color.background <- ifelse(nodes.sub2$Location_Type == "Anchorage", "tomato", nodes.sub2$color.background)
    nodes.sub2$color.background <- ifelse(nodes.sub2$Location_Type == "Loitering Area", "grey", nodes.sub2$color.background)
    nodes.sub2$shadow <- TRUE
    nodes.sub2$size <- scales::rescale(nodes.sub2$betw, to= c(10,25))
    nodes.sub2$title <- nodes.sub2$map_ship_name
    nodes.sub2$map_size <- scales::rescale(nodes.sub2$betw, to = c(2,5))
    
    
    ### Edges ###
    links.sub$shadow <- TRUE
    links.sub$arrows <- "to"
    links.sub$weight <- as.integer(links.sub$weight)
    links.sub$width <- scales::rescale(links.sub$weight, to=c(1,4))
    
    reac_vals$links.sub <- links.sub
    reac_vals$nodes.sub2 <- nodes.sub2
    
    
    updateButton(session, "gen_nets", style = "default")
    updateButton(session, "vis_net_map", style = "success")
    
    saveRDS(reac_vals, file = "temp_reac_vals.rds")
    
    
    reac_vals$g1 <- g1
    
  })
  
  
  ### Only allow Map button to work when on Map tab
  observe({
    input$step_5_tabs
    if(input$step_5_tabs == "map" & !is.null(reac_vals$links.sub)){
      updateButton(session, "vis_net_map", disabled = FALSE)
    } else {
      updateButton(session, "vis_net_map", disabled = TRUE)
    }
    
  })
  
  
  # Visualize Network
  
  output$network <- renderVisNetwork({
    
    links.sub <- reac_vals$links.sub
    nodes.sub2 <- reac_vals$nodes.sub2
    
    if(is.null(links.sub)|is.null(nodes.sub2))
      return(NULL)
    
    
    visNetwork(nodes.sub2, links.sub, height = "1000px") %>%
      visOptions(nodesIdSelection = TRUE, highlightNearest = TRUE) %>%
      visIgraphLayout(layout = "layout_nicely")
  })
  
  ## Chord Diagram
  
  output$chord_d <- renderChorddiag({
    links.sub <- reac_vals$links.sub
    nodes.sub2 <- reac_vals$nodes.sub2
    
    # Convert new names to ids 
    nodes_conv <- nodes.sub2[,c("id", "title")]
    links.sub_merged1 <- merge(links.sub, nodes_conv, by.x = "from", by.y = "id")
    links.sub_merged1$new_from <- links.sub_merged1$title
    links.sub_merged1$title <- NULL
    links.sub_merged1 <- merge(links.sub_merged1, nodes_conv, by.x = "to", by.y = "id")
    links.sub_merged1$new_to <- links.sub_merged1$title
    
    datatable(links.sub_merged1)
    
    new_df <- links.sub_merged1[,c("new_from", "new_to", "weight")]
    new_m <- as.matrix(new_df)
    g1 <- graph_from_edgelist(new_m[,1:2], directed = TRUE)
    E(g1)$weight <- new_m[,3]

    #### Chord Diagram

    out_mat <- as_adjacency_matrix(g1, type = c("both"), attr = "weight",
                                   edges = FALSE, names = TRUE, sparse = FALSE)

    out_mat <- as.matrix(out_mat)

    for(m in 1:nrow(out_mat)) {
      for(l in 1:nrow(out_mat)){
        out_mat[m,l] <- ifelse(out_mat[m,l] == "", 0, as.numeric(out_mat[m,l]))
      }
    }

    mode(out_mat) <- "numeric"



    #library(chorddiag)

    cd <-chorddiag(out_mat, groupnamePadding = 20,
                   showTicks = FALSE, groupnameFontsize = 12, margin = 120)
    cd
    
  })
  
  
  # Visualize Map
  
  output$network_map <- renderLeaflet({
    leaflet("network_map") %>%
      setView(lng = 0, lat = 0, zoom = 2) %>%
      addProviderTiles(providers$CartoDB.Positron, group = "Grey") %>%
      addProviderTiles(providers$OpenStreetMap.Mapnik, group = "OSM") %>%
      addProviderTiles(providers$Esri.WorldImagery, group = "Imagery") %>%
      addProviderTiles(providers$Esri.OceanBasemap, group = "Ocean") %>%
      
      addLayersControl(
        baseGroups = c("Grey", "OSM", "Imagery", "Ocean"),
        overlayGroups = c("Iso Overlay", "Network Overlay"),
        options = layersControlOptions(collapsed = FALSE)
      )
    
    
  })
  
  # Have map created right away
  outputOptions(output, "network_map", suspendWhenHidden = FALSE)
  
  observeEvent(input$vis_net_map,{
    iso_choice <- working_isos_index()
    
    polys_1 <- reac_vals[[iso_choice]]$polys2
    l_df1 <- reac_vals[[iso_choice]]$l_df_merge
    
    links.sub <- reac_vals$links.sub
    nodes.sub2 <- reac_vals$nodes.sub2
    
    if(is.null(polys_1)|is.null(l_df1)|is.null(links.sub)|is.null(nodes.sub2))
      return(NULL)
    
    proxy <- leafletProxy("network_map")
    
    proxy %>%
      clearGroup("Iso Overlay") %>%
      clearGroup("Network Overlay") %>%
      addPolygons(data = polys_1, color = "#F43E3E", fillOpacity = 0.7, smoothFactor = 0.5, popup = l_df1$map_ship_name, group = "Iso Overlay") %>%
      addCircleMarkers(nodes.sub2, lng = nodes.sub2$long, lat = nodes.sub2$lat, radius = nodes.sub2$map_size, color = "#B30000",
                       popup = nodes.sub2$title, group = "Network Overlay")
    
    for(e in 1:nrow(links.sub)){
      
      start_n <- links.sub[e,c("from")]
      end_n <- links.sub[e, c("to")]
      start_ll <-subset(nodes.sub2, nodes.sub2$id == start_n)
      end_ll <- subset(nodes.sub2, nodes.sub2$id == end_n)
      pl_df <- rbind(start_ll, end_ll)
      #pl_df$long <- ifelse(pl_df$long < 0, pl_df$long + 360, pl_df$long)
      addPolylines(proxy, data = pl_df, lng = pl_df$long, lat = pl_df$lat, color = "#B30000",
                   opacity = 0.2, weight = 2, group = "Network Overlay")
      
    }
    
  })
  
  
  ### Download Data ###
  
  output$downloadLXY <- downloadHandler(
    filename = function() {
      temp_time <- as.POSIXlt(Sys.time())
      temp_time_str <-format(temp_time, "%d%b%y_%H%M", tz = "")
      paste(temp_time_str,"_tlocoh_lxy", ".rds", sep = "")
    },
    content = function(file) {
      saveRDS(stopped_lxy(), file)
    })
  
  
  output$download_iso_99 <- downloadHandler(
    filename = function() {
      temp_time <- as.POSIXlt(Sys.time())
      temp_time_str <-format(temp_time, "%d%b%y_%H%M", tz = "")
      paste(temp_time_str,"_iso_99_tlocoh", ".rds", sep = "")
    },
    content = function(file) {
      saveRDS(reac_vals$out_tlocoh_obj$iso_99, file)
    }
  )
  
  
  output$download_iso_95 <- downloadHandler(
    filename = function() {
      temp_time <- as.POSIXlt(Sys.time())
      temp_time_str <-format(temp_time, "%d%b%y_%H%M", tz = "")
      paste(temp_time_str,"_iso_95_tlocoh", ".rds", sep = "")
    },
    content = function(file) {
      saveRDS(reac_vals$out_tlocoh_obj$iso_95, file)
    }
  )
  
  output$download_iso_75 <- downloadHandler(
    filename = function() {
      temp_time <- as.POSIXlt(Sys.time())
      temp_time_str <-format(temp_time, "%d%b%y_%H%M", tz = "")
      paste(temp_time_str,"_iso_75_tlocoh", ".rds", sep = "")
    },
    content = function(file) {
      saveRDS(reac_vals$out_tlocoh_obj$iso_75, file)
    }
  )
  
  output$download_iso_50 <- downloadHandler(
    filename = function() {
      temp_time <- as.POSIXlt(Sys.time())
      temp_time_str <-format(temp_time, "%d%b%y_%H%M", tz = "")
      paste(temp_time_str,"_iso_50_tlocoh", ".rds", sep = "")
    },
    content = function(file) {
      saveRDS(reac_vals$out_tlocoh_obj$iso_50, file)
    }
  )
  output$download_iso_10 <- downloadHandler(
    filename = function() {
      temp_time <- as.POSIXlt(Sys.time())
      temp_time_str <-format(temp_time, "%d%b%y_%H%M", tz = "")
      paste(temp_time_str,"_iso_10_tlocoh", ".rds", sep = "")
    },
    content = function(file) {
      saveRDS(reac_vals$out_tlocoh_obj$iso_10, file)
    }
  )
  
  output$map_as_rds <- renderUI({
    if(input$gen_map > 0) {
      downloadButton("download_map_as_rds", "Download Map R File")
    }
  })
  
  output$download_map_as_rds <- downloadHandler(
    filename = function() {
      temp_time <- as.POSIXlt(Sys.time())
      temp_time_str <-format(temp_time, "%d%b%y_%H%M", tz = "")
      paste(temp_time_str,"_mapped_isos", ".rds", sep = "")
    },
    content = function(file) {
      proxy <- leafletProxy("indiv_iso_map")
      write.csv(proxy, file, row.names = FALSE)
    }
  )

  
  output$download_ui_nl <- renderUI({
    if(!is.null(reac_vals$nodes.sub2)) {
      downloadButton("download_nl", "Download Nodelist")
    }
  })
  
  output$download_nl <- downloadHandler(
    filename = function() {
      temp_time <- as.POSIXlt(Sys.time())
      temp_time_str <-format(temp_time, "%d%b%y_%H%M", tz = "")
      paste(temp_time_str,"_nodelist", ".csv", sep = "")
    },
    content = function(file) {
      write.csv(reac_vals$nodes.sub2, file, row.names = FALSE)
    }
  )
    
    
  output$download_ui_el <- renderUI({
    if(!is.null(reac_vals$links.sub)) {
      downloadButton("download_el", "Download Edgelist")
    }
  })
  
  output$download_el <- downloadHandler(
    filename = function() {
      temp_time <- as.POSIXlt(Sys.time())
      temp_time_str <-format(temp_time, "%d%b%y_%H%M", tz = "")
      paste(temp_time_str,"_edgelist", ".csv", sep = "")
    },
    content = function(file) {
      write.csv(reac_vals$links.sub, file, row.names = FALSE)
    }
  )

  #### Test Stuff ####
  output$test_table <- renderDataTable({
    
    
    datatable(reac_vals$edge_list_mmsi)
    
  })
  
  output$test2 <- renderText({
    input$step_5_tabs == "chord_d"
    
  })
  
})
