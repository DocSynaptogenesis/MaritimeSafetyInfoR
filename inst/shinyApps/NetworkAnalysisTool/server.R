shinyServer(function(input, output, session){
  ### TAB 1 ###
  #Function to bring in an edgelist with attributes for boats:
  getlinks1 <- eventReactive(input$links1, {
    data.frame(read_excel(input$links1$datapath, 1))
  })
  
  #Function to bring in table of MMSI and polygons:
  getlinks2 <- eventReactive(input$links2, {
    data.frame(fread(input$links2$datapath))
  })
  
  #Getting data in right format:
  get_data <- reactive({
    shipsAndOwners <- getlinks1()
    shipsAndPolygons <- getlinks2()
    
    #Clean and Stucture Data:
    shipsAndOwners$Source.MMSI <- as.character(shipsAndOwners$Source.MMSI)
    shipsAndPolygons$mmsi <- as.character(shipsAndPolygons$mmsi)
    
    df <- shipsAndOwners %>%
      left_join(shipsAndPolygons, by=c("Source.MMSI"="mmsi"))
    
    # Create MMSI to polygon network:
    el_mmsiXpoli <- df %>%
      dplyr::select(Source.MMSI, poly1) %>%
      na.omit()
    nl_mmsixpoli <- data.frame(id = as.character(c(el_mmsiXpoli$Source.MMSI, el_mmsiXpoli$poly1))) %>%
      unique() %>%
      left_join(shipsAndOwners, by=c("id"="Source.MMSI"))
      
    g_mmsiXpoli <- igraph::graph_from_data_frame(el_mmsiXpoli, 
                                                 directed = FALSE,
                                                 vertices = nl_mmsixpoli)
    
    # Edit MMSI to polygon graph:
    V(g_mmsiXpoli)$type <- bipartite.mapping(g_mmsiXpoli)$type
    V(g_mmsiXpoli)$shape <- ifelse(V(g_mmsiXpoli)$type==F, "dot", "diamond")
    V(g_mmsiXpoli)$entity <- ifelse(V(g_mmsiXpoli)$type==F, "ship", "polygon")
    V(g_mmsiXpoli)$color.background <- ifelse(V(g_mmsiXpoli)$type==F, "#2b83ba", "#d7191c")
    V(g_mmsiXpoli)$borderWidth <- 2
    V(g_mmsiXpoli)$color.highlight.background <- "orange"
    V(g_mmsiXpoli)$color.highlight.border <- "darkred"
    V(g_mmsiXpoli)$color.border <- "black"
    V(g_mmsiXpoli)$size <- ifelse(V(g_mmsiXpoli)$type==F, 10, 20)
    V(g_mmsiXpoli)$id <- V(g_mmsiXpoli)$name
    E(g_mmsiXpoli)$color <- "black"
    V(g_mmsiXpoli)$title <- ifelse(V(g_mmsiXpoli)$entity=="ship",
                                   paste0("<b>MMSI: </b>", V(g_mmsiXpoli)$name,
                                          "<br>","<b>Vessel Name: </b>", V(g_mmsiXpoli)$Source.Name,
                                          "<br>","<b>Call.Sign: </b>", V(g_mmsiXpoli)$Call.Sign,
                                          "<br>","<b>Vessel Type: </b>", V(g_mmsiXpoli)$Type,
                                          "<br>","<b>Owner: </b>", V(g_mmsiXpoli)$Registration.Owner,
                                          "<br>","<b>Operator Parent: </b>", V(g_mmsiXpoli)$Operator.Parent, sep=""),
                                   paste0("<b>Polygon ID: </b>", V(g_mmsiXpoli)$name, sep=""))
    V(g_mmsiXpoli)$label <- ifelse(V(g_mmsiXpoli)$entity=="ship",V(g_mmsiXpoli)$`Source.Name`, NA)
    
    # Create and edit ship to ship network:
    g_sxs <- bipartite_projection(g_mmsiXpoli, multiplicity = TRUE)$proj1
    V(g_sxs)$label <- V(g_sxs)$Source.Name
    V(g_sxs)$color.background <- "#2b83ba"
    V(g_sxs)$borderWidth <- 2
    V(g_sxs)$color.border <- "black"
    V(g_sxs)$color.highlight.background <- "orange"
    V(g_sxs)$color.highlight.border <- "darkred"
    V(g_sxs)$Degree <- degree(g_sxs, mode="total")
    V(g_sxs)$Betweenness <- betweenness(g_sxs, directed = FALSE)
    E(g_sxs)$color <- "black"
    E(g_sxs)$width <- scales::rescale(E(g_sxs)$weight, to=c(1,10))
    
    # Create and edit company to company network:
    g_cxc <- igraph::get.data.frame(g_sxs, "edges") %>%
      left_join(df %>%
                  select(Source.MMSI, Registration.Owner) %>%
                  rename(from_c = Registration.Owner),
                by=c("from"="Source.MMSI")) %>%
      left_join(df %>%
                  select(Source.MMSI, Registration.Owner) %>%
                  rename(to_c = Registration.Owner),
                by=c("to"="Source.MMSI")) %>%
      dplyr::select(-c("from", "to")) %>%
      dplyr::rename(from=from_c, to=to_c) %>%
      dplyr::select(from, to, everything()) %>%
      dplyr::mutate(relationship="coloitering",
                    color = "4575b4") %>%
      rbind(shipsAndOwners %>%
              select(Registration.Owner, Operator.Parent) %>%
              na.omit() %>%
              rename(from=Registration.Owner, to=Operator.Parent) %>%
              mutate(relationship = "business",
                     color = "#d73027",
                     weight = 1,
                     width = 1)) %>%
      igraph::graph_from_data_frame()
    
    V(g_cxc)$color.background <- "#2b83ba"
    V(g_cxc)$borderWidth <- 2
    V(g_cxc)$color.border <- "black"
    V(g_cxc)$color.highlight.background <- "orange"
    V(g_cxc)$color.highlight.border <- "darkred"
    E(g_cxc)$width <- scales::rescale(E(g_cxc)$weight, to=c(1,10))
    V(g_cxc)$id <- V(g_cxc)$name
    V(g_cxc)$Degree <- degree(g_cxc, mode="total")
    V(g_cxc)$Betweenness <- betweenness(g_cxc, directed = FALSE)
    V(g_cxc)$title <- paste0("<b>Name: </b>", V(g_cxc)$name, "<br>",
                           "<b>Degree: </b>", V(g_cxc)$Degree, "<br>",
                           "<b>Betweenness: </b>", round(V(g_cxc)$Betweenness, digits = 2), "<br>")
      
    listed_data <- list("g_mmsiXpoli" = g_mmsiXpoli,
                        "g_sxs" = g_sxs,
                        "g_cxc" = g_cxc)
    return(listed_data)
  })
  
  #Reset the app:
  observeEvent(input$reset_button, {js$reset()})
  
  ### TAB 2 ###
  #Network in the first mainPanel tab of the network tab:
  output$network <-  renderVisNetwork({
    listed_data <- get_data()
    g_mmsiXpoli <- listed_data$g_mmsiXpoli
    visNetwork::visIgraph(g_mmsiXpoli)%>%
      visNodes(shadow=TRUE)%>%
      visEdges(color=list(color="slategrey", highlight="black"))%>%
      visOptions(highlightNearest = TRUE, nodesIdSelection=TRUE)%>%
      visInteraction(navigationButtons = TRUE, keyboard=TRUE) %>%
      visLegend(addNodes=list(
        list(label="Ship", shape="dot", size=15, color="#2b83ba"),
        list(label="Loitering Polygon", shape="diamond", size=15, color="#d7191c")
      ), useGroups=FALSE, position="right")
  }) # End of renderVisNetwork({})
  
  output$table <- renderDataTable({
    listed_data <- get_data()
    g_mmsiXpoli <- listed_data$g_mmsiXpoli
    g_mmsiXpoli %>%
      igraph::get.data.frame("vertices") %>%
      dplyr::select(name, Source.IMO, Call.Sign, Type, Source.Name,
                    Registration.Owner, Operator.Parent, entity) %>%
      dplyr::filter(entity == "ship") %>%
      DT::datatable(class = "cell-border stripe", rownames=FALSE,
                    options = list(lengthChange = TRUE,
                                   searching = TRUE,
                                   bInfo=TRUE,
                                   bPaginate=TRUE,
                                   bFilter=TRUE,
                                   ordering=TRUE))
    
  }) #End of renderDataTable({})
  
  # ### TAB 3 ### 
  # #Network in the secont mainPanel tab of the network tab:
  output$network2 <-  renderVisNetwork({
    listed_data <- get_data()
    g_sxs <- listed_data$g_sxs
    visNetwork::visIgraph(g_sxs) %>%
      visIgraphLayout(layout="layout_with_kk")%>%
      visOptions(highlightNearest = TRUE, nodesIdSelection=TRUE)%>%
      visInteraction(navigationButtons = TRUE, keyboard=TRUE)
  }) # End of renderVisNetwork({})
  
  observeEvent(input$measure2, {
    listed_data <- get_data()
    g_sxs <- listed_data$g_sxs
    
    getmeasure <- reactive({
      if ("None" %in% input$measure2) return(10)
      if ("Degree" %in% input$measure2) return(V(g_sxs)$Degree)
      if ("Betweenness" %in% input$measure2) return(V(g_sxs)$Betweenness)
    })
    
    V(g_sxs)$sizer <- getmeasure()
    V(g_sxs)$size <- scales::rescale(V(g_sxs)$sizer, to=c(10,30))
    visNetworkProxy("network2")%>%
      visUpdateNodes(igraph::get.data.frame(g_sxs, "vertices"))
  
    })# End of observeEvent({})
  output$table2 <- renderDataTable({
    listed_data <- get_data()
    listed_data$g_sxs %>%
      igraph::get.data.frame("vertices") %>%
      dplyr::select(name, `Source.IMO`, Type, `Registration.Owner`, `Operator.Parent`, Degree, Betweenness) %>%
      DT::datatable(class = "cell-border stripe", rownames=FALSE,
                    options = list(lengthChange = TRUE,
                                   searching = TRUE,
                                   bInfo=TRUE,
                                   bPaginate=TRUE,
                                   bFilter=TRUE,
                                   ordering=TRUE))
  }) #End of renderDataTable({})

  
  # ### TAB 4 ###
  output$network3 <-  renderVisNetwork({
    ledges <- data.frame(color = c("#d73027", "#4575b4"),
                         label = c("Business", "Co-loitering"), arrows =c("to", "to"))
    listed_data <- get_data()
    g_cxc <- listed_data$g_cxc
    # Create the network:
    visNetwork::visIgraph(g_cxc) %>%
      visIgraphLayout(layout="layout_with_kk")%>%
      visNodes(shadow=TRUE)%>%
      visEdges(arrows="")%>%
      visOptions(highlightNearest = TRUE)%>%
      visInteraction(navigationButtons = TRUE, keyboard=TRUE) %>%
      visLegend(addEdges = ledges, useGroups=FALSE, position="right")
  })# End of renderVisNetwork({})
  # 
  observeEvent(input$measure3, {
    listed_data <- get_data()
    g_cxc <- listed_data$g_cxc
    
    getmeasure2 <- reactive({
      if ("None" %in% input$measure3) return(10)
      if ("Degree" %in% input$measure3) return(V(g_cxc)$Degree)
      if ("Betweenness" %in% input$measure3) return(V(g_cxc)$Betweenness)
    })
    
    V(g_cxc)$sizer <- getmeasure2()
    V(g_cxc)$size <- scales::rescale(V(g_cxc)$sizer, to=c(10,30))
    visNetworkProxy("network3")%>%
      visUpdateNodes(igraph::get.data.frame(g_cxc, "vertices"))
  })# End of observeEvent({})
  # 
  output$table3 <- renderDataTable({
    listed_data <- get_data()
    listed_data$g_cxc %>%
      igraph::get.data.frame("vertices") %>%
      dplyr::select(name, Degree, Betweenness) %>%
      dplyr::rename(Organization=name) %>%
      DT::datatable(class = "cell-border stripe", rownames=FALSE,
                    options = list(lengthChange = TRUE,
                                   searching = TRUE,
                                   bInfo=TRUE,
                                   bPaginate=TRUE,
                                   bFilter=TRUE,
                                   ordering=TRUE))
  })
}) # End of Server
