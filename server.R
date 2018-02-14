#
# This is the server logic of a Shiny web application. Y
#
#


# Define server logic 
shinyServer(function(input, output, session) {
  ## Home tab
  # output$map_tz <- ({
  #   
  # })
  
  ## Dashboard tab
  # sample
  output$sample <- renderInfoBox({
    infoBox(format(nrow(finScope.raw), big.mark = ",", scientific=FALSE))
  })
  
  # population represented
  output$population <- renderInfoBox({
    infoBox(format(round(sum(finScope.raw$Final_weight),0), big.mark = ",", scientific=FALSE))
  })
  
  # pie chart of location
  output$loc <- renderHighchart({
    highchart() %>% 
      hc_chart(type = "pie") %>%#, options3d = list(enabled = TRUE, alpha = 70, beta = 0)) %>% 
      hc_plotOptions(pie = list(depth = 70)) %>% 
      hc_add_series_labels_values(dataLoc$Cluster, round(dataLoc$percent,2),
                                  size='80%', dataLabels = list(distance = -40, format = '{point.y}%')) %>%
      hc_tooltip(valueDecimals = 2,
                 pointFormat = "Percent: {point.y}%")
  })
  
  # pie chart of gender split
  output$gender <- renderHighchart({
    highchart() %>% 
      hc_chart(type = "pie") %>%#, options3d = list(enabled = TRUE, alpha = 70, beta = 0)) %>% 
      hc_plotOptions(pie = list(depth = 70)) %>% 
      hc_add_series_labels_values(dataGen$gender, round(dataGen$percent,2),
                                  size='80%', innerSize = "60%", dataLabels = list(distance = -40, format = '{point.y}%')) %>%
      hc_tooltip(valueDecimals = 2,
                 pointFormat = "Percent: {point.y}%")
  })
  
  ### BANKED VS UNBANKED TAB
  # fit the propensity score model
  createSegments = reactive({
    finScope.raw %>%
      mutate(banked_pr_score = predict(glm(banked ~ .,family = binomial(), data = finScope.raw[,c("banked", input$model_vars)]), type = "response")) %>%
      mutate(banked_segments = if_else(banked==1, 1,
                                       if_else(banked_pr_score>input$cut_off[2], 2,
                                               if_else(banked_pr_score >input$cut_off[1] & banked_pr_score<=input$cut_off[2], 3, 4)))) %>%
      mutate(banked_segments = factor(banked_segments, levels = 1:4, labels = c("Banked", "Bankable", "Development", "Unbankable")))
  })
  
  # histogram of propensity scores
  output$unbanked_hist <- renderHighchart({
    data <- createSegments()
    hchart(data$banked_pr_score[data$banked==0], name = "Propensity score (Similarity to banked people)") %>%
      hc_xAxis(plotLines = list(
        list(color = "#FF0000", width = 1, value = input$cut_off[1]),
        list(color = "#FF0000", width = 1, value = input$cut_off[2]))) %>%
      hc_exporting(enabled = TRUE)
  })
  
  # plot the distribution of segments and gender
  output$banked_segments <- renderHighchart({
    data <- createSegments()
    temp.df <- as.data.frame(data %>% 
                               group_by_(.dots=c(input$graph_by)) %>%
                               summarise(sample_count = n(),
                                         pop_count = sum(Final_weight)) %>%
                               mutate(percent = round((pop_count*100/sum(pop_count)),2)) %>%
                               select(-sample_count, -pop_count))
    
    # # rename the columns of this aggregatd dataset
    if(length(names(temp.df))>2){
      names(temp.df)[1:2] <- c("segments", "group_var")
      
      mycols <- brewer.pal(8, "Blues")
      highchart() %>%
        hc_add_series_df(data = temp.df,
                         type = "bar",
                         x = segments,
                         y = percent,
                         group = group_var,
                         name = "Population Percentage") %>%
        hc_xAxis(title = list(text = "Banking segments and gender"),
                 type = "categorical",
                 categories = unique(temp.df$segments),
                 tickmarkPlacement = "on",
                 tickLength = 0,
                 labels = list(
                   enabled = TRUE
                 )) %>%
        hc_yAxis(title = list(text = "Population Percentage")) %>% 
        hc_legend(enabled = FALSE) %>%
        hc_colors(mycols[(8-(length(unique(temp.df$group_var)))+1):8]) %>%
        hc_exporting(enabled = TRUE) #%>% hc_add_theme(hc_theme_sandsignika())
      
    } else {
      # rename the grouping variable for ease of reference
      xlabel = reactive({(toTitleCase(gsub("_", " ", input$graph_by)))}) # x-axis label
      names(temp.df)[1] <- c("graph_var")
      
      highchart() %>%
        hc_add_series_df(data = temp.df,
                         type = "bar",
                         x = graph_var,
                         y = percent,
                         name = "Population Percentage") %>%
        hc_xAxis(title = list(text = xlabel()),
                 type = "categorical",
                 categories = temp.df$graph_var,
                 tickmarkPlacement = "on",
                 tickLength = 0,
                 labels = list(
                   enabled = TRUE
                 )) %>%
        hc_yAxis(title = list(text = "Population Percentage")) %>% 
        hc_legend(enabled = FALSE) %>%
        hc_tooltip(valueDecimals = 2,
                   pointFormat = "Percent: {point.y}%") %>%
        hc_exporting(enabled = TRUE)
      
    }
  })
  
  # distribution of variables by segments 
  output$banked_crosstab1 <- renderHighchart({
    data <- createSegments()
    temp.df <- as.data.frame(data %>% 
                               group_by_(.dots=c("banked_segments", input$cross_vars)) %>%
                               summarise(sample_count = n(),
                                         pop_count = sum(Final_weight)) %>%
                               mutate(percent = round((pop_count*100/sum(pop_count)),2)) %>%
                               select(-sample_count, -pop_count))
    
    # rename one of the columns for ease of referencing
    names(temp.df)[2] <- "group_var"
    
    #do the plot
      mycols <- brewer.pal(8, "Blues")
      highchart() %>%
        hc_add_series_df(data = temp.df,
                         type = "column",
                         x = banked_segments,
                         y = percent,
                         group = group_var,
                         name = "Population Percentage") %>%
        hc_xAxis(title = list(text = "Banking segments"),
                 type = "categorical",
                 categories = unique(temp.df$banked_segments),
                 tickmarkPlacement = "on",
                 tickLength = 0,
                 labels = list(
                   enabled = TRUE
                 )) %>%
        hc_yAxis(title = list(text = "Population Percentage")) %>% 
        hc_legend(enabled = TRUE) %>%
        hc_colors(mycols[(8-(length(unique(temp.df$group_var)))+1):8]) %>%
        hc_exporting(enabled = TRUE) #%>% hc_add_theme(hc_theme_sandsignika())
})
  
  # distribution of variables by segments and gender
  output$banked_crosstab2 <- renderUI({
    data <- createSegments()
    temp.df <- as.data.frame(data %>% 
                               group_by_(.dots=c("banked_segments", "gender",input$cross_vars)) %>%
                               summarise(sample_count = n(),
                                         pop_count = sum(Final_weight)) %>%
                               group_by_(.dots=c("banked_segments", "gender")) %>%
                               mutate(percent = round((pop_count*100/sum(pop_count)),2)) %>%
                               select(-sample_count, -pop_count))
    
    # plotting colours 
    mycols <- brewer.pal(8, "Blues")
    
    xlabel = reactive({(toTitleCase(gsub("_", " ", c("Banking segments",input$cross_vars))))}) # x-axis label
    
    ## plot for 3 way graph -  plot only if none of the grouping variables contains more than 8 levels
    ## rename the columns of this aggregatd dataset
      names(temp.df)[3] <- "cross_var"
       
        map(unique(temp.df$gender), function(x){
          highchart() %>%
            hc_add_series_df(data = temp.df[temp.df$gender==x, ] ,
                             type = "column",
                             x = banked_segments,
                             y = percent,
                             group = cross_var,
                             name = "Population Percentage") %>%
            hc_xAxis(title = list(text = xlabel()),
                     type = "categorical",
                     categories = unique(temp.df$banked_segments),
                     tickmarkPlacement = "on",
                     tickLength = 0,
                     labels = list(
                       enabled = TRUE
                     )) %>%
            hc_title(text = paste("Gender:", x),
                     margin = 2, align = "center") %>%
            hc_yAxis(title = list(text = "Population Percentage")) %>% 
            hc_legend(enabled = FALSE) %>%
            hc_colors(mycols[(8-(length(unique(temp.df$cross_var)))+1):8]) %>%
            hc_exporting(enabled = TRUE) #%>% hc_add_theme(hc_theme_darkunica())
        }) %>% 
          hw_grid(rowheight = 225, ncol =  1) 
  })
  
  
  # ## MNO VS NO-MNO TAB
  # fir propensity model and create the segments
  createSegmentsMno = reactive({
    mno.model <- glm(mm ~ .,family = binomial(), data = finScope.raw[,c("mm", input$mno_model_vars)])
    finScope.raw %>%
      mutate(mno_pr_score = predict(mno.model, type = "response")) %>%
      mutate(mno_segments = if_else(mm==1, 1,
                                   if_else(mno_pr_score>=as.numeric(input$mno_cut_off), 2, 3))) %>%
      mutate(mno_segments = factor(mno_segments, levels = 1:3, labels = c("MNO users", "DFS able", "Non-DFS able")))
  })

  output$mno_hist <- renderHighchart({
    data <- createSegmentsMno()
    hchart(data$mno_pr_score[data$mm==0]) %>%
      hc_xAxis(title = list(text = "Propensity score (Similarity to MNO users)"),
               plotLines = list(
                 list(color = "#FF0000", width = 2, value = input$mno_cut_off))) %>%
      hc_exporting(enabled = TRUE)
  })

  # plot the distribution of segments and gender
  output$mno_segments <- renderHighchart({
    data <- createSegmentsMno()
    temp.df <- as.data.frame(data %>% 
                               group_by_(.dots=c(input$mno_graph_by)) %>%
                               summarise(sample_count = n(),
                                         pop_count = sum(Final_weight)) %>%
                               mutate(percent = round((pop_count*100/sum(pop_count)),2)) %>%
                               select(-sample_count, -pop_count))
    
    # # rename the columns of this aggregatd dataset
    if(length(names(temp.df))>2){
      names(temp.df)[1:2] <- c("segments", "group_var")
      
      mycols <- brewer.pal(8, "Blues")
      highchart() %>%
        hc_add_series_df(data = temp.df,
                         type = "bar",
                         x = segments,
                         y = percent,
                         group = group_var,
                         name = "Population Percentage") %>%
        hc_xAxis(title = list(text = "MNO segments and gender"),
                 type = "categorical",
                 categories = unique(temp.df$segments),
                 tickmarkPlacement = "on",
                 tickLength = 0,
                 labels = list(
                   enabled = TRUE
                 )) %>%
        hc_yAxis(title = list(text = "Population Percentage")) %>% 
        hc_legend(enabled = FALSE) %>%
        hc_colors(mycols[(8-(length(unique(temp.df$group_var)))+1):8]) %>%
        hc_exporting(enabled = TRUE) #%>% hc_add_theme(hc_theme_sandsignika())
      
    } else {
      # rename the grouping variable for ease of reference
      xlabel = reactive({(toTitleCase(gsub("_", " ", input$mno_graph_by)))}) # x-axis label
      names(temp.df)[1] <- c("graph_var")
      
      highchart() %>%
        hc_add_series_df(data = temp.df,
                         type = "bar",
                         x = graph_var,
                         y = percent,
                         name = "Population Percentage") %>%
        hc_xAxis(title = list(text = xlabel()),
                 type = "categorical",
                 categories = temp.df$graph_var,
                 tickmarkPlacement = "on",
                 tickLength = 0,
                 labels = list(
                   enabled = TRUE
                 )) %>%
        hc_yAxis(title = list(text = "Population Percentage")) %>% 
        hc_legend(enabled = FALSE) %>%
        hc_tooltip(valueDecimals = 2,
                   pointFormat = "Percent: {point.y}%") %>%
        hc_exporting(enabled = TRUE)
      
    }
  })
  
  # distribution of variables by segments 
  output$mno_crosstab1 <- renderHighchart({
    data <- createSegmentsMno()
    temp.df <- as.data.frame(data %>% 
                               group_by_(.dots=c("mno_segments", input$mno_cross_vars)) %>%
                               summarise(sample_count = n(),
                                         pop_count = sum(Final_weight)) %>%
                               mutate(percent = round((pop_count*100/sum(pop_count)),2)) %>%
                               select(-sample_count, -pop_count))
    
    # rename one of the columns for ease of referencing
    names(temp.df)[2] <- "group_var"
    
    #do the plot
    mycols <- brewer.pal(8, "Blues")
    highchart() %>%
      hc_add_series_df(data = temp.df,
                       type = "column",
                       x = mno_segments,
                       y = percent,
                       group = group_var,
                       name = "Population Percentage") %>%
      hc_xAxis(title = list(text = "MNO segments"),
               type = "categorical",
               categories = unique(temp.df$mno_segments),
               tickmarkPlacement = "on",
               tickLength = 0,
               labels = list(
                 enabled = TRUE
               )) %>%
      hc_yAxis(title = list(text = "Population Percentage")) %>% 
      hc_legend(enabled = TRUE) %>%
      hc_colors(mycols[(8-(length(unique(temp.df$group_var)))+1):8]) %>%
      hc_exporting(enabled = TRUE) #%>% hc_add_theme(hc_theme_sandsignika())
  })
  
  # distribution of variables by segments and gender
  output$mno_crosstab2 <- renderUI({
    data <- createSegmentsMno()
    temp.df <- as.data.frame(data %>% 
                               group_by_(.dots=c("mno_segments", "gender",input$mno_cross_vars)) %>%
                               summarise(sample_count = n(),
                                         pop_count = sum(Final_weight)) %>%
                               group_by_(.dots=c("mno_segments", "gender")) %>%
                               mutate(percent = round((pop_count*100/sum(pop_count)),2)) %>%
                               select(-sample_count, -pop_count))
    
    # plotting colours 
    mycols <- brewer.pal(8, "Blues")
    
    xlabel = reactive({(toTitleCase(gsub("_", " ", c("Banking segments",input$mno_cross_vars))))}) # x-axis label
    
    ## plot for 3 way graph -  separate plots for each gender
    ## rename the columns of this aggregatd dataset
    names(temp.df)[3] <- "cross_var"
    
    map(unique(temp.df$gender), function(x){
      highchart() %>%
        hc_add_series_df(data = temp.df[temp.df$gender==x, ] ,
                         type = "column",
                         x = mno_segments,
                         y = percent,
                         group = cross_var,
                         name = "Population Percentage") %>%
        hc_xAxis(title = list(text = xlabel()),
                 type = "categorical",
                 categories = unique(temp.df$mno_segments),
                 tickmarkPlacement = "on",
                 tickLength = 0,
                 labels = list(
                   enabled = TRUE
                 )) %>%
        hc_title(text = paste("Gender:", x),
                 margin = 2, align = "center") %>%
        hc_yAxis(title = list(text = "Population Percentage")) %>% 
        hc_legend(enabled = FALSE) %>%
        hc_colors(mycols[(8-(length(unique(temp.df$cross_var)))+1):8]) %>%
        hc_exporting(enabled = TRUE) #%>% hc_add_theme(hc_theme_darkunica())
    }) %>% 
      hw_grid(rowheight = 225, ncol =  1) 
  })
  
  ### Other Analyses TAB
  # plot tanzania map
  output$map_dist_tz <- renderLeaflet({
    tmp.df <- finScope.raw %>%
      dplyr::filter_at(vars(input$mark_var), any_vars(. == 1)) %>%
      dplyr::filter(RU != "Zanzibar") %>%
      dplyr::select(Longitude, Latitude, Ward)

    # define labels
    label <- sprintf(
      "<strong>%s</strong><br/>%s",
      "Ward:", tmp.df$Ward
    ) %>% lapply(htmltools::HTML)
    
    leaflet(data = tmp.df) %>% addTiles() %>%
      addCircleMarkers(~Longitude, ~Latitude, popup = "", label = label,
                       fillColor = "blueviolet", radius = 2, color = "blueviolet", fillOpacity = 0.5,
                       stroke = FALSE) %>% setView(36, -6, 6)


  })

  output$map_dist_zanzi <- renderLeaflet({
    tmp.df <- finScope.raw %>%
      dplyr::filter_at(vars(input$mark_var), any_vars(. == 1)) %>%
      dplyr::filter(RU == "Zanzibar") %>%
      dplyr::select(Longitude, Latitude, Ward)

    # define labels
    label <- sprintf(
      "<strong>%s</strong><br/>%s",
      "Ward:", tmp.df$Ward
    ) %>% lapply(htmltools::HTML)
    
    leaflet(data = tmp.df) %>% addTiles() %>%
      addCircleMarkers(~Longitude, ~Latitude, popup = "", label = label,
                       fillColor = "blueviolet", radius = 2, color = "blueviolet", fillOpacity = 0.5,
                       stroke = FALSE)
  })

  output$map_chloro_tz <- renderLeaflet({
    tmp.df <- as.data.frame(finScope.raw %>%
                              dplyr::filter(RU != "Zanzibar") %>%
                              group_by_(.dots=c(input$mark_var2, "Region_Nam")) %>%
                              summarise(sample_count = n(),
                                        pop_count = sum(Final_weight)) %>% ungroup() %>%
                              group_by(Region_Nam) %>%
                              mutate(percent = round(pop_count*100/sum(pop_count),2)))

    names(tmp.df)[which(names(tmp.df)==input$mark_var2)] <- "interest.var"
    tmp.df <- tmp.df[tmp.df$interest.var==1, c("Region_Nam", "percent")]

    #### add to shape file
    tz_regions@data$Region_Nam <- as.character(tz_regions@data$Region_Nam)
    tz_only_regions <- unique(finScope.raw$Region_Nam[finScope.raw$RU!="Zanzibar"])
    tz_only <- tz_regions[tz_regions$Region_Nam %in% tz_only_regions, ]
    tz_only@data <- left_join(tz_only@data, tmp.df)
    
    # define colour pallette
    bins <- c(0, 20, 40, 60, 80, 100)
    pallette <- colorBin("Blues", domain = tmp.df$percent, bins = bins)

    # define labels
    labels <- sprintf(
      "<strong>%s</strong><br/>%g<span>&#37;</span>",
      tz_only@data$Region_Nam, tz_only@data$percent
    ) %>% lapply(htmltools::HTML)

    leaflet(tz_only) %>% setView(36, -6, 6) %>% addPolygons(
      fillColor = ~pallette(percent),
      weight = 0.5,
      opacity = 0.5,
      color = "darkslateblue",
      fillOpacity = 0.7,
      highlight = highlightOptions(
        weight = 2,
        color = "red",stroke = FALSE,
        fillOpacity = 0.5,
        bringToFront = TRUE),
      label = labels,
      labelOptions = labelOptions(
        style = list("font-weight" = "normal", padding = "3px 8px"),
        textsize = "15px",
        direction = "auto")) %>%
      addLegend(pal = pallette, values = ~percent, opacity = 0.5, title = NULL,
                position = "bottomright")
  })

  output$map_chloro_zanzi <- renderLeaflet({
    tmp.df <- as.data.frame(finScope.raw %>%
                              dplyr::filter(RU == "Zanzibar") %>%
                              group_by_(.dots=c(input$mark_var2, "Region_Nam")) %>%
                              summarise(sample_count = n(),
                                        pop_count = sum(Final_weight)) %>% ungroup() %>%
                              group_by(Region_Nam) %>%
                              mutate(percent = round(pop_count*100/sum(pop_count),2)))

    names(tmp.df)[which(names(tmp.df)==input$mark_var2)] <- "interest.var"
    tmp.df <- tmp.df[tmp.df$interest.var==1, c("Region_Nam", "percent")]

    tz_regions@data$Region_Nam <- as.character(tz_regions@data$Region_Nam)
    zanzibar_regions <- unique(finScope.raw$Region_Nam[finScope.raw$RU=="Zanzibar"])
    zanzibar_only <- tz_regions[tz_regions$Region_Nam %in% zanzibar_regions, ]
    zanzibar_only@data <- left_join(zanzibar_only@data, tmp.df)

    # define colour pallette
    bins <- c(0, 20, 40, 60, 80, 100)
    pallette <- colorBin("Blues", domain = tmp.df$percent, bins = bins)

    # define labels
    labels <- sprintf(
      "<strong>%s</strong><br/>%g<span>&#37;</span>",
      zanzibar_only@data$Region_Nam, zanzibar_only@data$percent
    ) %>% lapply(htmltools::HTML)

    leaflet(zanzibar_only) %>% setView(39, -6, 8) %>% addPolygons(
      fillColor = ~pallette(percent),
      weight = 0.5,
      opacity = 0.5,
      color = "darkslateblue",
      fillOpacity = 0.7,
      highlight = highlightOptions(
        weight = 2,
        color = "red",stroke = FALSE,
        fillOpacity = 0.5,
        bringToFront = TRUE),
      label = labels,
      labelOptions = labelOptions(
        style = list("font-weight" = "normal", padding = "3px 8px"),
        textsize = "15px",
        direction = "auto")) %>%
      addLegend(pal = pallette, values = ~percent, opacity = 0.5, title = NULL,
                position = "bottomright")
  })
  
  ## Regional descriptives
  # reactive dataset y=to be dispalyed/downloaded
  region_table <- reactive({
    finScope.raw %>%
      group_by_(.dots=c("Region_Nam", input$reg_cross_vars)) %>%
      summarise(sample_count = n(),
                pop_count = sum(Final_weight)) %>% ungroup() %>%
      group_by(Region_Nam) %>%
      mutate(percent = round(pop_count*100/sum(pop_count),2)) %>%
      select(-sample_count, -pop_count) %>% ungroup() %>%
      data.table::setnames(., input$reg_cross_vars, "input_var") %>% 
      spread(input_var, percent)
  })
  
  # distribution iof variables by region
  output$reg_crosstab <- DT::renderDataTable({
    region_table()
  })
  
  # Download the data
  output$downloadData <- downloadHandler(
    filename = function() {
      paste("Region_", input$reg_cross_vars, ".csv", sep = "")
    },
    content = function(file) {
      write.csv(region_table(), file, row.names = FALSE)
    }
  )
  
  # custom tables
  output$cust_crosstab1 <- renderHighchart({
    temp.df <- as.data.frame(finScope.raw %>% 
                               group_by_(.dots=c(input$cust_cross_vars)) %>%
                               summarise(sample_count = n(),
                                         pop_count = sum(Final_weight)) %>%
                               mutate(percent = round((pop_count*100/sum(pop_count)),2)) %>%
                               select(-sample_count, -pop_count))
    
    # rename one of the columns for ease of referencing
    xlabel = reactive({(toTitleCase(gsub("_", " ", input$cust_cross_vars)))}) # x-axis label
    names(temp.df)[1] <- c("graph_var")
    
    # create tree map
    tm <- treemap(temp.df,
                  index=c("graph_var"),
                  vSize="percent",
                  type="index")
    
    # create as a high charter
    highchart() %>% 
      hc_add_series_treemap(tm, allowDrillToNode = TRUE,
                            layoutAlgorithm = "squarified",
                            name = "Percentage distribution") %>% 
      hc_tooltip(pointFormat = "<b>{point.name}</b>:{point.value:,.0f}%")
  })
  
  output$cust_crosstab2 <- renderUI({
    temp.df <- as.data.frame(finScope.raw %>% 
                               group_by_(.dots=c("gender",input$cust_cross_vars)) %>%
                               summarise(sample_count = n(),
                                         pop_count = sum(Final_weight)) %>%
                               group_by_(.dots=c("gender")) %>%
                               mutate(percent = round((pop_count*100/sum(pop_count)),2)) %>%
                               select(-sample_count, -pop_count))
    
    # plotting colours 
    mycols <- brewer.pal(8, "Blues")
    xlabel = reactive({(toTitleCase(gsub("_", " ", c(input$cust_cross_vars))))}) # x-axis label
    
    ## plot separate graphs for gender
    ## rename the columns of this aggregatd dataset
    names(temp.df)[2] <- "cross_var"
    
    map(unique(temp.df$gender), function(x){
      highchart() %>%
        hc_add_series_df(data = temp.df[temp.df$gender==x, ] ,
                         type = "column",
                         x = cross_var,
                         y = percent,
                         name = "Population Percentage") %>%
        hc_xAxis(title = list(text = xlabel()),
                 type = "categorical",
                 categories = unique(temp.df$cross_var),
                 tickmarkPlacement = "on",
                 tickLength = 0,
                 labels = list(
                   enabled = TRUE
                 )) %>%
        hc_title(text = paste("Gender:", x),
                 margin = 2, align = "center") %>%
        hc_yAxis(title = list(text = "Population Percentage")) %>% 
        hc_legend(enabled = FALSE) %>%
        hc_colors(mycols[(8-(length(unique(temp.df$cross_var)))+1):8]) %>%
        hc_exporting(enabled = TRUE) 
    }) %>% 
      hw_grid(rowheight = 225, ncol =  1) 
  })
  
})
