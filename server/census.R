c_year <- reactive({
  c_y <- input$c_year
})

c_loc <- reactive({
  input$c_location
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
  # TODO: This needs to be moved to get_census_data.R to be done only once on app loading
  censusStir$Region <- as.character(censusStir$`Region`)
  censusStir %>% mutate(
    percent_less_than_30 =
      round(stir_less_than_30 / (stir_less_than_30 + stir_more_than_30) * 100, digits = 2),
    percent_more_than_30 =
      round(stir_more_than_30 / (stir_less_than_30 + stir_more_than_30) * 100, digits = 2)
  ) %<>%
    arrange(desc(percent_more_than_30))

  # Reorder data
  censusStir$GeoUID <- factor(
    censusStir$GeoUID,
    levels = unique(censusStir$GeoUID)[order(
      censusStir$percent_more_than_30, decreasing = FALSE
    )]
  )
  return(censusStir)
})

censusAvgAge <- reactive({
  censusAvgAge <- switch(
    input$c_view,
    "CMA" = census2016aaCma,
    "CSD" = census2016aaCsd,
    "CD" = census2016aaCd,
    "CT" = census2016aaCt,
    "DA" = census2016aaDa
  )
})

censusPp2016 <- reactive({
  censusPp <- switch(
    input$c_view,
    "CMA" = censusPpCma,
    "CSD" = censusPpCsd,
    "CD" = censusPpCd,
    "CT" = censusPpCt,
    "DA" = censusPpDa
  )
  return(censusPp)
})

#
# Dropdown options for location based on selected geo-level
#
regionOptions <- reactive({
    regionOptions <- censusMobility() %>%
      mutate(label = paste0(censusMobility()$`Region`, " (", censusMobility()$GeoUID, ")")) %>%
      select(label, value = GeoUID)
})

#
# Population pyramid 2016 for selected location
#
censusPp2016Location <- reactive({
    censusPp2016() %>%
      filter(GeoUID == input$c_location) %>%
      mutate("percentage_compare" = 0)
    # censusPp2016()$age <- factor(censusPp2016()$age, levels = unique(censusPp2016()$age)[order(censusPp2016()$ageStartYear, decreasing = FALSE)])
    # return(censusPp2016())
})

#
# Population pyramid 2016 to compare with selected location
#
censusPp2016LocationCompare <- reactive({
  locationA <- input$c_location
  locationB <- input$c_location_pp_compare

  censusPp2016CompareA <- censusPp2016() %>%
    filter(GeoUID == locationA)

  if (locationB == "") {
    locationB = locationA
  }

  censusPp2016CompareB <- censusPp2016() %>%
    filter(GeoUID == locationB) %>%
    ungroup() %>%
    select(Region_compare = Region, age, sex, percentage_compare = percentage_2016)

  censusPp2016Compare <- inner_join(
    censusPp2016CompareA,
    censusPp2016CompareB,
    by = c("age", "sex") # $compare_loc <- censusPp2016Compare$percentage
  )
  return(censusPp2016Compare)
})

# Reactive location label
locationLabel <- reactive({
  locationLabel <- censusMobility() %>%
    filter(GeoUID == input$c_location) %>%
    mutate(label = paste0(Region, " (", GeoUID, ")")) %>%
    select(label)
  st_geometry(locationLabel) <- NULL
  return(locationLabel)
})

# Reactive PP compare location label
locationCompareLabel <- reactive({
  locationCompareLabel <- censusMobility() %>%
    filter(GeoUID == input$c_location_pp_compare) %>%
    mutate(label = paste0(Region, " (", GeoUID, ")")) %>%
    select(label)
  st_geometry(locationCompareLabel) <- NULL
  return(locationCompareLabel)
})

# Reactive housing types
housingTypes <- reactive({
  housingTypes <- switch(
    input$c_view,
    "CMA" = housingTypesCma,
    "CD" = housingTypesCd,
    "CSD" = housingTypesCsd,
    "CT" = housingTypesCt,
    "DA" = housingTypesDa
  )
})

# Reactive housing types depending on selected type
housingTypeMapData <- reactive({
  htMapData <- housingTypes() %>%
    # select(Region, GeoUID, typewatch = input$c_housing_types)
    select(typewatch = input$c_housing_types, everything()) %>%
    mutate(typewatch2 = as.numeric(input$c_housing_types))
})

#
# Mobility palette
#
palHousingTypes <- colorNumeric(
  palette = "viridis",
  domain = housingTypesCma$`Single detached house ratio`,
  na.color = "#e6e6e6"
)

#
# Census observer
#
observe({
  # Update locations if geo-level selection changes
  # updateSelectizeInput(session, 'c_location', choices = regionOptions(), server = TRUE)

  # Housing type barchart
  traces = list(
    list(
      x = housingTypes()$`Region`,
      y = housingTypes()$`Appartment in tall building`,
      color = colNonStrataRental,
      name = "Appartment in tall building"
    ),
    list(
      x = housingTypes()$`Region`,
      y = housingTypes()$`Semi detached house`,
      color = colCommercial,
      name = "Semi-detached house"
    ),
    list(
      x = housingTypes()$`Region`,
      y = housingTypes()$`Row house`,
      color = colResidential,
      name = "Row house"
    ),
    list(
      x = housingTypes()$`Region`,
      y = housingTypes()$`Appartment in duplex`,
      color = colMultiFam,
      name = "Appartment in duplex"
    ),
    list(
      x = housingTypes()$`Region`,
      y = housingTypes()$`Appartment in small building`,
      color = colStrata,
      name = "Appartment in small building"
    ),
    list(
      x = housingTypes()$`Region`,
      y = housingTypes()$`Other single attached house`,
      color = colAcreage,
      name = "Other single-attached house"
    ),
    list(
      x = housingTypes()$`Region`,
      y = housingTypes()$`Movable dwelling`,
      color = colForeign,
      name = "Movable dwelling"
    )
  )
  c16dwellTypePlot <- plotmy(
    housingTypes(),
    housingTypes()$`Region`,
    housingTypes()$`Single detached house`,
    "Single-detached house",
    "bar",
    colSingleFam,
    "Type of dwelling",
    traces
  )
  output$c16dwellType <- renderPlotly({
    c16dwellTypePlot
  })

  # Defaults and shape the data
  if (!is.null(housingTypes())) {
    housingTypes <- housingTypes()
  }

  #
  # Housing Types map palette
  #
  palHousingTypes <- colorNumeric(
    palette = "viridis",
    domain = housingTypeMapData()$typewatch,
    na.color = "#e6e6e6"
  )

  #
  # Mobility observer
  #
  censusMobility <- censusMobilityCma
  if (!is.null(censusMobility())) {
    censusMobility <- censusMobility()
  }
  censusMobility %<>%
    mutate(`Region` = as.character(`Region`), Type = as.character(Type)) %<>%
    gather(
      "Non-Movers Ratio", "Non-Migrants Ratio", "External Migrants Ratio",
      "Intraprovincial Migrants Ratio", "Interprovincial Migrants Ratio",
      key = "Migration", value = "count")

  # Mobility palette
  palMobility <- colorNumeric(
    palette = "viridis",
    domain = censusMobility$`Movers Ratio`,
    na.color = "#e6e6e6"
  )
  palLightRed <- "#fc95a4"# "#feb87e"# "#e85361"# "#e08176"
  palLighterBlue <- colNonStrataRental
  palLightBlue <- colMultiFam
  palDarkBlue <- colSingleFam
  palOther <- colUnknown

  #
  # Population Pyramid observer
  #
  # Average Age map
  censusAvgAge <- census2016aaCma
  if (!is.null(censusAvgAge())) {
    censusAvgAge <- censusAvgAge()
  }
  censusAvgAge %<>%
    mutate(`Region` = as.character(`Region.Name`), Type = as.character(Type))

  # AvgAge palette
  palAvgAge <- colorNumeric(
    palette = "viridis",
    domain = censusAvgAge$`Average Age`,
    na.color = "#e6e6e6"
  )

  #
  # STIR observer
  #
  # SHELTER-COST-TO-INCOME RATIO
  censusStir <- st_as_sf(
    censusStir() %>% select(everything())
  )

  # STIR palette
  palStir <- colorBin(
    palette = "viridis",
    domain = censusStir$percent_more_than_30, n = 10
  )

  # Redraw polygons when geo-level or housing type selection changes
  output$mapCensus <- renderLeaflet({
    leaflet() %>%
      addProviderTiles(provider = "CartoDB.Positron", options = providerTileOptions(minZoom = 6, maxZoom = 12)) %>%
      setView(lng = -122.12, lat = 51.78, zoom = 7) %>%
      addPolygons(data = housingTypeMapData(),
        label = ~ `Region`, color = '#333',
        fillColor = ~ palHousingTypes(housingTypeMapData()$typewatch),
        stroke = TRUE, weight = 1, fillOpacity = 0.5, smoothFactor = 0.2,
        layerId = ~ paste0("ht-", GeoUID), group = "Housing",
        popup = paste0(
          "<strong>", paste0(housingTypeMapData()$`Region`, " (", housingTypeMapData()$GeoUID), ")</strong>",
          "<table class=\"leaflet-popup-table\"><tr><td>Census Year</td><td>2016</td></tr>",
          "<tr><td>Single family homes ratio</td><td><strong>",
          format(housingTypeMapData()$`Single detached house ratio`, big.mark = ","), "</strong></td></tr>",
          "<tr><td>Semi detached house ratio</strong></td><td>",
          format(housingTypeMapData()$`Semi detached house ratio`, big.mark = ","), "</strong></td></tr>",
          "<tr><td>Appartment in duplex ratio</strong></td><td>",
          format(housingTypeMapData()$`Appartment in duplex ratio`, big.mark = ","), "</strong></td></tr>",
          "<tr><td>Row house ratio</strong></td><td>",
          format(housingTypeMapData()$`Row house ratio`, big.mark = ","), "</strong></td></tr>",
          "<tr><td>Appartment in small building ratio</strong></td><td>",
          format(housingTypeMapData()$`Appartment in small building ratio`, big.mark = ","), "</strong></td></tr>",
          "<tr><td>Appartment in tall building ratio</strong></td><td>",
          format(housingTypeMapData()$`Appartment in tall building ratio`, big.mark = ","), "</strong></td></tr>",
          "<tr><td>Other single attached house ratio</strong></td><td>",
          format(housingTypeMapData()$`Other single attached house ratio`, big.mark = ","), "</strong></td></tr>",
          "<tr><td>Movable dwelling ratio</strong></td><td>",
          format(housingTypeMapData()$`Movable dwelling ratio`, big.mark = ","), "</strong></td></tr>",
          "</table>"
        ),
        highlight = highlightOptions(
          weight = 5, color = "#696969", dashArray = "", fillOpacity = 0.75, bringToFront = TRUE)
      ) %>%
      addPolygons(data = censusMobility,
        label = ~ `Region`, color = '#333', fillColor = ~ palMobility(censusMobility$`Movers Ratio`),
        stroke = TRUE, weight = 1, fillOpacity = 0.5, smoothFactor = 0.2,
        layerId = ~ paste0("m-", GeoUID), group = "Mobility",
        # fillOpacity = 0.5,
        # smoothFactor = 1,
        popup = paste0(
          "<strong>", paste0(censusMobility$`Region`, " (", censusMobility$GeoUID), ")</strong>",
          "<table class=\"leaflet-popup-table\"><tr><td>Census Year</td><td>2016</td></tr>",
          "<tr><td>Population</td><td>", format(censusMobility$Population, big.mark = ","),
          "</td></tr><tr><td>Dwellings</td><td>", format(censusMobility$Dwellings, big.mark = ","),
          "</td></tr><tr><td>Households</td><td>", format(censusMobility$Households, big.mark = ","),
          "</td></tr><tr><td><strong>Movers Ratio</strong></td><td><strong>",
          format(censusMobility$`Movers Ratio`, big.mark = ","), "</strong></td></tr></table>"
        ),
        highlight = highlightOptions(
          weight = 5, color = "#696969", dashArray = "", fillOpacity = 0.75, bringToFront = TRUE)
      ) %>%
      addPolygons(data = censusStir,
        label = ~ `Region`, color = '#333', fillColor = ~ palStir(censusStir$percent_more_than_30),
        stroke = TRUE, weight = 1, fillOpacity = 0.5, smoothFactor = 0.2,
        layerId = ~ paste0("s-", GeoUID), group = "STIR",
        popup = paste0(
          "<strong>", paste(censusStir$`Region`), "</strong>",
          "<table class=\"leaflet-popup-table\"><tr><td>Census Year</td><td>2016</td></tr>",
          "<tr><td>Population</td><td>", format(censusStir$Population, big.mark = ","),
          "</td></tr><tr><td>Dwellings</td><td>", format(censusStir$Dwellings, big.mark = ","),
          "</td></tr><tr><td>Households</td><td>", format(censusStir$Households, big.mark = ","),
          "</td></tr><tr><td>STIR < 30%</td><td>", format(censusStir$percent_less_than_30, big.mark = ","),
          "</td></tr><tr><td><strong>STIR > 30%</strong></td><td><strong>",
          format(censusStir$percent_more_than_30, big.mark = ","), "</strong></td></tr></table>"
        ),
        highlight = highlightOptions(
          weight = 5, color = "#696969", dashArray = "", fillOpacity = 0.5, bringToFront = TRUE)
      ) %>%
      addPolygons(data = censusAvgAge,
        label = ~ `Region`, color = '#333', fillColor = ~ palAvgAge(censusAvgAge$`Average Age`),
        stroke = TRUE, weight = 1, fillOpacity = 0.5, smoothFactor = 0.2,
        layerId = ~ paste0("p-", GeoUID), group = "Population",
        popup = paste0(
          "<strong>", paste0(censusAvgAge$`Region`, " (", censusAvgAge$GeoUID), ")</strong>",
          "<table class=\"leaflet-popup-table\"><tr><td>Census Year</td><td>2016</td></tr>",
          "<tr><td>Population</td><td>", format(censusAvgAge$Population, big.mark = ","),
          "</td></tr><tr><td>Dwellings</td><td>", format(censusAvgAge$Dwellings, big.mark = ","),
          "</td></tr><tr><td>Households</td><td>", format(censusAvgAge$Households, big.mark = ","),
          "</td></tr><tr><td><strong>Average Age</strong></td><td><strong>",
          format(censusAvgAge$`Average Age`, big.mark = ","), "</strong></td></tr></table>"
        ),
        highlight = highlightOptions(
          weight = 5, color = "#696969", dashArray = "", fillOpacity = 0.75, bringToFront = TRUE)
      ) %>%
    # Layers control
    addLayersControl(
      # overlayGroups = c("OSM (default)", "Toner", "Toner Lite"),
      # baseGroups = c("Population", "Housing", "Mobility", "STIR"),
      baseGroups = c("Population", "Housing", "Mobility", "STIR"),
      options = layersControlOptions(collapsed = FALSE)
    )

  })

  # Housing Types treemap
  # Have to drop geometry, i.e. convert sf to df to use in treemap
  housingTypesDf <- housingTypes
  st_geometry(housingTypesDf) <- NULL

  housingTypesDf %<>%
    gather(
      "Single detached house ratio",
      "Appartment in tall building ratio",
      "Semi detached house ratio",
      "Row house ratio",
      "Appartment in duplex ratio",
      "Appartment in small building ratio",
      "Other single attached house ratio",
      "Movable dwelling ratio",
      key = "HousingType", value = "ratio")

  output$housingTypeTreemap <- renderPlot({
    treemap(
      title = paste("Distribution of Housing Types at ", locationLabel()),
      housingTypesDf %>% filter(GeoUID == input$c_location) %>% mutate(ratioFormat = paste0(gsub(" ratio", "", HousingType), " - ", ratio, "%")),
      index = c("ratioFormat"),
      vSize = "ratio",
      type = "index",
      vColor = "ratio",
      palette = c(colMultiFam, colStrata, colNonStrataRental, colForeign, colAcreage, colResidential, colCommercial, colSingleFam),
      algorithm = "pivotSize",
      # sortID = "HousingType",
      fontsize.title = c(14),
      fontsize.labels = c(12),
      fontcolor.labels = c("#121212"),
      fontface.labels = c(1),
      bg.labels = c("#CCCCCCDC"),
      align.labels = list(c("center", "center")),
      overlap.labels = 0.5,
      border.col = "#696969",
      border.lwds = c(1),
      force.print.labels = FALSE,
      inflate.labels = F
    )
  })

  #
  # Mobility treemap
  #
  # Have to drop geometry, i.e. convert sf to df to use in treemap
  censusMobilityDf <- censusMobility
  st_geometry(censusMobilityDf) <- NULL

  output$c16mobilityTree <- renderPlot({
    treemap(
      # housingTypesDf %>% filter(GeoUID == input$c_location) %>% mutate(ratioFormat = paste0(gsub(" ratio", "", HousingType), " - ", ratio, "%")),
      # title = paste("Distribution of Mobility Categories at ", locationLabel()),
      title = "",
      censusMobilityDf %>% filter(GeoUID == input$c_location) %>% mutate(ratioFormat = paste0(Migration, " - ", count, "%")),
      index = c("ratioFormat"),
      vSize = "count",
      type = "index",
      vColor = "Migration",
      palette = c(palOther, palLighterBlue, palLightBlue, palDarkBlue, palLightRed),
      algorithm = "pivotSize",
      # sortID = "Migration",
      fontsize.title = c(14),
      fontsize.labels = c(12),
      fontcolor.labels = c("#121212"),
      fontface.labels = c(1),
      bg.labels = c("#CCCCCCDC"),
      align.labels = list(c("center", "center")),
      overlap.labels = 0.5,
      border.col = "#696969",
      border.lwds = c(1),
      force.print.labels = FALSE,
      inflate.labels = F
    )
  })

  #
  # Population Pyramid
  #
  output$popPyr <- renderPlotly({
    # plot_ly(censusPp2016() %>% filter(GeoUID == input$c_location),
    p <- plot_ly(censusPp2016LocationCompare(),
            x = ~percentage_2016, y = ~age, name = ~paste(Region, '2016'), color = ~sex, type = 'bar', orientation = 'h',
            hoverinfo = 'y+text+name', text = ~paste('Census 2016</br>', Region, abs(percentage_2016), '%'), colors = c('lightsalmon', colMultiFam)) %>%
      add_trace(x = ~percentage_2011, y = ~age, name = ~paste(Region, '2011'), type = "scatter", mode = 'lines+markers',
                line = list(color = '#2222cc99', shape = "spline"),
                hoverinfo = "x+y+text",
                text = ~paste('Census 2011</br>', Region, abs(percentage_2011), '%')
                ) %>%
      add_annotations(xref = 'x', yref = 'y',
                      x = ~(percentage_2016 / abs(percentage_2016) / 2), y = ~age,
                      # x = 1, y = ~age,
                      text = ~paste(abs(percentage_2016), '%'),
                      font = list(family = 'Arial', size = 12,
                                  color = 'rgb(67, 67, 67)'),
                      showarrow = FALSE) #%>%

      # add_trace(x = ~percentage_2006, y = ~age, name = '2006', type = "scatter", mode = 'lines+markers',
      #           line = list(color = 'lightslategrey', shape = "spline"),
      #           hoverinfo = "x+y+text",
      #           text = ~paste(percentage_2011, '%')) %>%
      # add_trace(x = ~percentage_compare, y = ~age, name = 'compare', type = "scatter", mode = 'lines+markers',
      #           line = list(color = 'red', shape = "spline"),
      #           hoverinfo = "x+y+text",
      #           text = ~paste(percentage_compare, '%')) %>%
      # labeling the percentages of each bar (x_axis)


    ppTitle <- paste('Population Pyramid for', locationLabel(), '- Census year', input$c_year)
    if (input$c_location_pp_compare != "") {
      p <- p %>% add_trace(x = ~percentage_compare, y = ~age, name = ~ paste(Region_compare, '2016'), type = "scatter", mode = 'lines+markers',
                            line = list(color = '#cc222299', shape = "spline"),
                            hoverinfo = "x+y+text",
                            text = ~paste('Census 2016</br>', Region_compare, abs(percentage_compare), '%')) %>%
        add_annotations(xref = 'paper', yref = 'paper',
                        x = 0.5, y = 1.05,
                        text = ~paste('compared to', locationCompareLabel()),
                        font = list(size = 12, color = '#969696'),
                        showarrow = FALSE)
      # ppTitle <- paste(ppTitle, '<br />compared to', locationCompareLabel())
    }
    p %>%
      layout(bargap = 0.2, barmode = 'overlay',
             margin = list(l = 150, t = 75),
             xaxis = list(
               title = "",
               tickmode = 'array',
               tickvals = c(-10, -9, -8, -7, -6, -5, -4, -3, -2, -1, 0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10),
               ticktext = c(
                 '10%', '9%', '8%', '7%', '6%', '5%', '4%', '3%', '2%', '1%',
                 '0', '1%', '2%', '3%', '4%', '5%', '6%', '7%', '8%', '9%', '10%'
               )
             ),
             yaxis = list(title = ""),
             title = ppTitle,
             titleFont = list(size = 12, color = "#ff0000")) %>%
      config(displayModeBar = F)
  })


  #
  # STIR Lollipop
  #
  output$stirLollipop <- renderPlotly(
    plot_ly(censusStir() %>% top_n(25, percent_more_than_30), x = ~percent_more_than_30,
            name = "More than 30%", y = ~`GeoUID`, type = 'bar', orientation = 'h',
            marker = list(
              color = ~percent_more_than_30, # color = "lightsalmon",
              line = list(width = 0.1),
              hoverinfo="x+y+name"
            )
      ) %>%
      add_trace(x = ~percent_more_than_30, name = "Less than 30%",
                type="scatter",
                marker = list(size = 15,
                              color = ~percent_more_than_30, #color = 'lightsalmon',
                              line = list(color = 'lightsalmon',
                                          width = 4))
      ) %>%
      layout(
        title = "Percentage of households having Shelter-Cost-to-Income Ratio > 30%",
        bargap = 0.85,
        xaxis = list(
          title = "", showgrid = FALSE, showline = FALSE, zerolinecolor = "#e6e6e6",
          showticklabels = TRUE, zeroline = TRUE, domain = c(0.15, 1)
        ),
        yaxis = list(
          title = "", showgrid = FALSE, showline = FALSE, zerolinecolor = "#cccccc",
          showticklabels = FALSE, zeroline = TRUE),
        margin = list(l = 150, r = 10, t = 70, b = 30),
        showlegend = FALSE) %>%
    # labeling the y-axis
    add_annotations(xref = 'paper', yref = 'y', x = 0.14, y = ~`GeoUID`,
                    xanchor = 'right',
                    text = ~paste0(`Region`, ' (', `GeoUID`, ')'),
                    font = list(family = 'Arial', size = 12,
                                color = 'rgb(67, 67, 67)'),
                    showarrow = FALSE, align = 'right') %>%
      config(displayModeBar = F)

  )

  # STIR Stacked Bar
  topLabels <- c('More than 30%', 'Less than 30%')
  labelPositions <- c(
    censusStir()[1,"percent_more_than_30"]$percent_more_than_30 / 2,
    censusStir()[1,"percent_more_than_30"]$percent_more_than_30 +
      censusStir()[1,"percent_less_than_30"]$percent_less_than_30 / 2
  )
  output$stirStacked <- renderPlotly(
    plot_ly(censusStir() %>% top_n(25, percent_more_than_30), x = ~percent_more_than_30, name = "More than 30%",
            y = ~`GeoUID`, type = 'bar', orientation = 'h',
            marker = list(color = "lightsalmon",
                          line = list(color = 'rgb(248, 248, 249)', width = 1), hoverinfo="x+y+name")) %>%
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
             margin = list(l = 150, r = 10, t = 70, b = 30),
             showlegend = FALSE) %>%
      # labeling the y-axis
      add_annotations(xref = 'paper', yref = 'y', x = 0.14, y = ~`GeoUID`,
                      xanchor = 'right',
                      text = ~`Region`,
                      font = list(family = 'Arial', size = 12,
                                  color = 'rgb(67, 67, 67)'),
                      showarrow = FALSE, align = 'right') %>%
      # labeling the percentages of each bar (x_axis)
      add_annotations(xref = 'x', yref = 'y',
                      x = ~(percent_more_than_30 / 2), y = ~`GeoUID`,
                      text = ~paste(percent_more_than_30, '%'),
                      font = list(family = 'Arial', size = 12,
                                  color = 'rgb(67, 67, 67)'),
                      showarrow = FALSE) %>%
      add_annotations(xref = 'x', yref = 'y',
                      x = ~(percent_more_than_30 + percent_less_than_30 / 2), y = ~GeoUID,
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

# Map click observer
observeEvent(input$mapCensus_shape_click, {
  m <- input$mapCensus_shape_click
  if(!is.null(m$id)){
    id <- str_split(m$id, "-", simplify = TRUE)
    locationId = id[1,2]

    # updateSelectInput(session, "c_location", selected = p$id)
    updateTextInput(session, "c_location", value = locationId)

    locationLabel <- censusMobility() %>%
      filter(GeoUID == locationId) %>%
      mutate(label = paste0(Region, " (", GeoUID, ")")) %>%
      select(label)
    st_geometry(locationLabel) <- NULL
    updateTextInput(session, "c_location_name", value = locationLabel$label)

    updateSelectizeInput(session, 'c_location_pp_compare', choices = regionOptions(), server = TRUE)
  }
})
