c_year <- reactive({
  c_y <- input$c_year
})

censusMobility <- reactive({
  censusMobility <- switch(
    input$c_view,
    "CMA" = censusMobilityCma,
    "CSD" = censusMobilityCsd,
    "CD" = censusMobilityCd,
    "CT" = censusMobilityCt,
    "DA" = censusMobilityDa
  )
})

censusStir <- reactive({
  censusStir <- switch(
    input$c_view,
    "CMA" = census2016CmaStir,
    "CSD" = census2016CsdStir,
    "CD" = census2016CdStir,
    "CT" = census2016CtStir,
    "DA" = census2016DaStir
  )
  # Massage the data a little bit
  censusStir$Region <- as.character(censusStir$`Region Name`)
  censusStir %>% mutate(
    percent_less_than_30 =
      round(stir_less_than_30 / (stir_less_than_30 + stir_more_than_30) * 100, digits = 2),
    percent_more_than_30 =
      round(stir_more_than_30 / (stir_less_than_30 + stir_more_than_30) * 100, digits = 2)
  ) %<>%
    arrange(desc(percent_more_than_30))

  # Reorder data
  censusStir$Region <- factor(
    censusStir$Region,
    levels = unique(censusStir$Region)[order(
      censusStir$percent_more_than_30, decreasing = FALSE
    )]
  )
  return(censusStir)
})

censusPp2016 <- reactive({
  censusStir <- switch(
    input$c_view,
    "CMA" = census2016ppCma,
    "CSD" = census2016ppCsd,
    "CD" = census2016ppCd,
    "CT" = census2016ppCt,
    "DA" = census2016ppDa
  )
})

# Dropdown options for location based on selected geo-level
regionOptions <- reactive({
    regionOptions <- censusMobility() %>%
      mutate(label = paste0(censusMobility()$`Region Name`, " (", censusMobility()$GeoUID, ")")) %>%
      select(label, value = GeoUID)
})

# Population pyramid 2016 for selected location
censusPp2016Location <- reactive({
    censusPp2016 %>% filter(GeoUID == input$c_location)
})

# Population pyramid 2011 for selected location
censusPp2011Location <- reactive({
    censusPp2011 %>% filter(GeoUID == input$c_location)
})

# Population pyramid 2006 for selected location
censusPp2006Location <- reactive({
    censusPp2006 %>% filter(GeoUID == input$c_location)
})

locationLabel <- reactive({
  if (is.null(input$c_location)) {
    return ("Please select a location")
  } else {
    regionOptions() %>% filter(value == input$c_location) %>% select(label)
  }
})

censusPp2011 <- reactive({
  censusStir <- switch(
    input$c_view,
    "CMA" = census2011ppCma,
    "CSD" = census2011ppCsd,
    "CD" = census2011ppCd,
    "CT" = census2011ppCt,
    "DA" = census2011ppDa
  )
})

censusPp2006 <- reactive({
  censusStir <- switch(
    input$c_view,
    "CMA" = census2006ppCma,
    "CSD" = census2006ppCsd,
    "CD" = census2006ppCd,
    "CT" = census2006ppCt,
    "DA" = census2006ppDa
  )
})

censusData <- reactive({
  censusData <- switch(
    input$c_view,
    "CMA" = censusDataCma,
    "CD" = censusDataCd,
    "CSD" = censusDataCsd,
    "CT" = censusDataCt#,
    # "DA" = censusDataDa
  )
})

censusDataSpatial <- reactive({
  censusDataSpatial <- switch(
    input$c_view,
    "CMA" = censusDataSpatialCma,
    "CD" = censusDataSpatialCd,
    "CSD" = censusDataSpatialCsd,
    "CT" = censusDataSpatialCt#,
    # "DA" = censusDataSpatialDa
  )
})

# STIR
observe({
  censusCategories <- label_vectors(censusData())

  # isolate({
  #   regionOptions <- censusMobility() %>%
  #     mutate(label = paste0(censusMobility()$`Region Name`, " (", censusMobility()$GeoUID, ")")) %>%
  #     select(label, value = GeoUID)
  # })

  updateSelectizeInput(session, 'c_location', choices = regionOptions(), server = TRUE)

  output$mapCensus <- renderLeaflet({
    censusDataSpatial() %>%
      leaflet() %>%
      addProviderTiles(provider = "CartoDB.Positron") %>%
      addPolygons(
        label = ~ name,
        # color = ~ pal(v_CA16_2447),
        stroke = TRUE,
        weight = 1,
        fillOpacity = 0.5,
        smoothFactor = 1,
        color = '#333',
        fillColor = ~ palViridis(censusDataSpatial()$v_CA16_2447),
        popup = paste0(
          "<strong>",
          paste(censusDataSpatial()$`Region Name`),
          "</strong>",
          "<table class=\"leaflet-popup-table\">
          <tr><td>Census Year</td><td>2016</td></tr>",
          "<tr><td>Population</td><td>",
          format(censusDataSpatial()$Population, big.mark = ","),
          "</td></tr><tr><td>Dwellings</td><td>",
          format(censusDataSpatial()$Dwellings, big.mark = ","),
          "</td></tr><tr><td>Households</td><td>",
          format(censusDataSpatial()$Households, big.mark = ","),
          "</td></tr><tr><td>Median total income</td><td>",
          paste("$", format(censusDataSpatial()$v_CA16_2447, big.mark = ","), sep =
                  ""),
          "</td></tr><tr><td>Average Age</td><td>",
          censusDataSpatial()$v_CA16_379,
          "</td></tr><tr><td>Provate dwellings occupied by usual residents</td><td>",
          format(censusDataSpatial()$v_CA16_405, big.mark = ","),
          "</td></tr><tr><td>Single detahed houses</td><td>",
          format(censusDataSpatial()$v_CA16_412, big.mark = ","),
          "</td></tr><tr><td>Average family size</td><td>",
          format(censusDataSpatial()$v_CA16_2449, big.mark = ","),
          "</td></tr></table>"
        ),
        highlight = highlightOptions(
          weight = 5,
          color = "#696969",
          dashArray = "",
          fillOpacity = 0.5,
          bringToFront = TRUE)
      ) %>%
      addLegend(
        "bottomleft",
        pal = palViridis,
        values = ~ v_CA16_2447,
        title = "Median total income of <br> economic families in 2015",
        opacity = 0.5
      )
  })

  # Dwelling type
  traces = list(
    list(
      x = censusData()$`Region Name`,
      y = censusData()$v_CA16_410,
      color = colNonStrataRental,
      name = "Appartment in tall building"
    ),
    list(
      x = censusData()$`Region Name`,
      y = censusData()$v_CA16_412,
      color = colCanadian,
      name = "Semi-detached house"
    ),
    list(
      x = censusData()$`Region Name`,
      y = censusData()$v_CA16_413,
      color = colResidential,
      name = "Row house"
    ),
    list(
      x = censusData()$`Region Name`,
      y = censusData()$v_CA16_414,
      color = colMultiFam,
      name = "Appartment in duplex"
    ),
    list(
      x = censusData()$`Region Name`,
      y = censusData()$v_CA16_415,
      color = colStrata,
      name = "Appartment in small building"
    ),
    list(
      x = censusData()$`Region Name`,
      y = censusData()$v_CA16_416,
      color = colAcreage,
      name = "Other single-attached house"
    ),
    list(
      x = censusData()$`Region Name`,
      y = censusData()$v_CA16_417,
      color = colForeign,
      name = "Movable dwelling"
    )
  )
  c16dwellTypePlot <- plotmy(
    censusData(),
    censusData()$`Region Name`,
    censusData()$v_CA16_409,
    "Single-detached house",
    "bar",
    colSingleFam,
    "Type of dwelling",
    traces
  )
  output$c16dwellType <- renderPlotly({
    c16dwellTypePlot
  })

  # Mobility
  censusMobility <- censusMobilityCma
  if (!is.null(censusMobility())) {
    censusMobility <- censusMobility()
  }
  censusMobility %<>%
    mutate(`Region Name` = as.character(`Region Name`), Type = as.character(Type)) %<>%
    gather(
      "Non-movers", "Non-migrants",
      "External migrants", "Intraprovincial migrants", "Interprovincial migrants",
      key = "Migration", value = "count")

  # Mobility palette
  palLightRed <- "#fc95a4"# "#feb87e"# "#e85361"# "#e08176"
  palLighterBlue <- colNonStrataRental
  palLightBlue <- colMultiFam
  palDarkBlue <- colSingleFam
  palOther <- colUnknown

  output$c16mobilityTree <- renderD3tree3({
    d3tree3(
      treemap(
        censusMobility %>% filter(GeoUID == input$c_location),
        index = c("Migration", "Region Name"),
        vSize = "count",
        type = "index",
        vColor = "Migration",
        palette = c(palOther, palLighterBlue, palLightBlue, palDarkBlue, palLightRed),
        algorithm = "pivotSize",
        sortID = "Migration",
        fontsize.labels=c(15,12),                # size of labels. Give the size per level of aggregation: size for group, size for subgroup, sub-subgroups...
        fontcolor.labels=c("white","orange"),    # Color of labels
        fontface.labels=c(2,1),                  # Font of labels: 1,2,3,4 for normal, bold, italic, bold-italic...
        bg.labels=c("transparent"),              # Background color of labels
        align.labels=list(
          c("center", "center"),
          c("right", "bottom")
        ),                                   # Where to place labels in the rectangle?
        overlap.labels=0.5,                      # number between 0 and 1 that determines the tolerance of the overlap between labels. 0 means that labels of lower levels are not printed if higher level labels overlap, 1  means that labels are always printed. In-between values, for instance the default value .5, means that lower level labels are printed if other labels do not overlap with more than .5  times their area size.
        inflate.labels=F
      ),
      rootname="Mobility"
    )
  })

  # Population Pyramid
  output$popPyr <- renderPlotly(
    plot_ly(censusPp2016() %>% arrange(age) %>% filter(GeoUID == input$c_location),
            x = ~percentage, y = ~age, color = ~sex, type = 'bar', orientation = 'h',
            hoverinfo = 'y+text+name', text = ~percentage, colors = c('lightsalmon', colMultiFam)) %>%
      layout(bargap = 0.2, barmode = 'overlay',
             margin = list(l = 250),
             xaxis = list(
               title = "Percentag of population by sex and age group",
               tickmode = 'array',
               tickvals = c(-10, -9, -8, -7, -6, -5, -4, -3, -2, -1, 0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10),
               ticktext = c(
                 '10%', '9%', '8%', '7%', '6%', '5%', '4%', '3%', '2%', '1%',
                 '0', '1%', '2%', '3%', '4%', '5%', '6%', '7%', '8%', '9%', '10%'
               )
             ),
             yaxis = list(
               title = ""
             ),
             title = paste('Population Pyramid for', locationLabel(), '- Census year', input$c_year)) %>%
      # labeling the percentages of each bar (x_axis)
      add_annotations(xref = 'x', yref = 'y',
                    x = ~(percentage / abs(percentage) / 2), y = ~age,
                    # x = 1, y = ~age,
                    text = ~paste(abs(percentage), '%'),
                    font = list(family = 'Arial', size = 12,
                                color = 'rgb(67, 67, 67)'),
                    showarrow = FALSE) %>%
      config(displayModeBar = F)
  )

  output$popPyrDT <- renderDataTable(
    censusPp2016() %>% arrange(age) %>% filter(GeoUID == input$c_location),
    options = list(
      lengthChange = TRUE,
      initComplete = JS(
        "function(settings, json) {
            $(this.api().table().header()).css({
            'background-color': 'rgba(0, 51, 102, 0.80)',
            'border-bottom': '5px solid #fcba19',
            'color': '#fff'
            });
        }"
      )
    )
  )

  # SHELTER-COST-TO-INCOME RATIO
  # STIR palette
  censusStir <- st_as_sf(
    censusStir() %>% select(everything())
  )

  palStir <- colorBin(
    palette = "viridis",
    domain = censusStir$percent_more_than_30, n = 10
  )

  # STIR Map
  output$mapCensusStir <- renderLeaflet({
    censusStir %>%
      leaflet() %>%
      addProviderTiles(provider = "CartoDB.Positron") %>%
      setView(lng = -123.12, lat = 53.28, zoom = 6) %>%
      addPolygons(
        label = ~ `Region`,
        color = '#333',
        fillColor = ~ palStir(censusStir$percent_more_than_30),
        stroke = TRUE,
        weight = 1,
        fillOpacity = 0.75,
        smoothFactor = 0.2,
        popup = paste0(
          "<strong>",
          paste(censusStir$`Region`),
          "</strong>",
          "<table class=\"leaflet-popup-table\">
        <tr><td>Census Year</td><td>2016</td></tr>",
          "<tr><td>Population</td><td>",
          format(censusStir$Population, big.mark = ","),
          "</td></tr><tr><td>Dwellings</td><td>",
          format(censusStir$Dwellings, big.mark = ","),
          "</td></tr><tr><td>Households</td><td>",
          format(censusStir$Households, big.mark = ","),
          "</td></tr><tr><td>STIR < 30%</td><td>",
          format(censusStir$percent_less_than_30, big.mark = ","),
          "</td></tr><tr><td><strong>STIR > 30%</strong></td><td><strong>",
          format(censusStir$percent_more_than_30, big.mark = ","),
          "</strong></td></tr></table>"
        ),
      highlight = highlightOptions(
        weight = 5,
        color = "#696969",
        dashArray = "",
        fillOpacity = 0.5,
        bringToFront = TRUE)
      ) %>%
      addLegend(
        "bottomleft",
        pal = palStir,
        values = ~ percent_more_than_30,
        title = "",
        opacity = 0.5
      )
  })

  # censusStir() %<>%
  #   top_n(25, percent_more_than_30) %<>%
  #   mutate(
  #     # Region = ifelse(c_view %in% c("csd", "ct", "da"), paste(`Region Name`, str_sub(GeoUID, -2)), `Region Name`),
  #     Region = factor(
  #       paste(`Region Name`, str_sub(GeoUID, -2)),
  #       levels = unique(Region)[order(percent_more_than_30, decreasing = FALSE)]
  #     )
  #   )

  # STIR Lollipop
  censusStirgg2 <- ggplot(censusStir() %>% top_n(25, percent_more_than_30), aes(x = Region, y = percent_more_than_30)) +
    geom_segment(aes(x = Region, xend = Region, y = 0, yend = percent_more_than_30), size = 1.5, color = "lightsalmon") +
    geom_point(color = "lightsalmon", size = 2.5, alpha = 1, shape = 21, stroke = 2.5) +
    theme_light() +
    coord_flip() +
    theme(
      panel.grid.major.y = element_blank(),
      panel.border = element_blank(),
      axis.ticks.y = element_blank(),
      plot.margin = unit(c(20,20,20,150), "pt")
    )
  output$stirLollipop <- renderPlotly(
    ggplotly(censusStirgg2) %>%
      layout(
        xaxis = list(title = ""),
        yaxis = list(title = "")
      ) %>%
      layout (
        margin = list(l = 150)
      )
  )

  # STIR Stacked Bar
  topLabels <- c('More than 30%', 'Less than 30%')
  labelPositions <- c(
    censusStir()[1,"percent_more_than_30"]$percent_more_than_30 / 2,
    censusStir()[1,"percent_more_than_30"]$percent_more_than_30 +
      censusStir()[1,"percent_less_than_30"]$percent_less_than_30 / 2
  )
  output$stirStacked <- renderPlotly(
    plot_ly(censusStir() %>% top_n(25, percent_more_than_30), x = ~percent_more_than_30, name = "More than 30%", y = ~`Region`, type = 'bar', orientation = 'h',
            marker = list(color = "lightsalmon",
                          line = list(color = 'rgb(248, 248, 249)', width = 1), hoverinfo="x+y+name")) %>%
      # add_trace(x = ~between_30_and_50_percent, name = "Between 30 and 50%", marker = list(color = colFarms)) %>%
      # add_trace(x = ~between_15_and_30_percent, name = "Between 15 and 30%", marker = list(color = colMultiFam)) %>%
      add_trace(x = ~percent_less_than_30, name = "Less than 30%", marker = list(color = colMultiFam)) %>%
      layout(xaxis = list(title = "",
                          showgrid = FALSE,
                          showline = FALSE,
                          showticklabels = FALSE,
                          zeroline = FALSE,
                          domain = c(0.15, 1)),
             yaxis = list(title = "",
                          showgrid = FALSE,
                          showline = FALSE,
                          showticklabels = FALSE,
                          zeroline = FALSE),
             barmode = 'stack',
             #paper_bgcolor = 'rgb(248, 248, 255)', plot_bgcolor = 'rgb(248, 248, 255)',
             margin = list(l = 63, r = 10, t = 70, b = 30),
             showlegend = FALSE) %>%
      # labeling the y-axis
      add_annotations(xref = 'paper', yref = 'y', x = 0.14, y = ~`Region`,
                      xanchor = 'right',
                      text = ~`Region`,
                      font = list(family = 'Arial', size = 12,
                                  color = 'rgb(67, 67, 67)'),
                      showarrow = FALSE, align = 'right') %>%
      # labeling the percentages of each bar (x_axis)
      add_annotations(xref = 'x', yref = 'y',
                      x = ~(percent_more_than_30 / 2), y = ~`Region`,
                      text = ~paste(percent_more_than_30, '%'),
                      font = list(family = 'Arial', size = 12,
                                  color = 'rgb(67, 67, 67)'),
                      showarrow = FALSE) %>%
      add_annotations(xref = 'x', yref = 'y',
                      x = ~(percent_more_than_30 + percent_less_than_30 / 2), y = ~Region,
                      text = ~paste(percent_less_than_30, '%'),
                      font = list(family = 'Arial', size = 12,
                                  color = 'rgb(67, 67, 67)'),
                      showarrow = FALSE) %>%
      # labeling the first Likert scale (on the top)
      add_annotations(xref = 'x', yref = 'paper',
                      x = labelPositions,
                      y = 1.05,
                      text = topLabels,
                      font = list(family = 'Arial', size = 12,
                                  color = 'rgb(67, 67, 67)'),
                      showarrow = FALSE) %>%
      config(displayModeBar = F)
  )
})
