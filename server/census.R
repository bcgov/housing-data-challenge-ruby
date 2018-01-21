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

#
# Dropdown options for location based on selected geo-level
#
# regionOptions <- reactive({
#     regionOptions <- censusMobility() %>%
#       mutate(label = paste0(censusMobility()$`Region`, " (", censusMobility()$GeoUID, ")")) %>%
#       select(label, value = GeoUID)
# })

#
# Population pyramid 2016 for selected location
#
censusPp2016Location <- reactive({
    censusPp2016 %>% filter(GeoUID == input$c_location)
})

#
# Population pyramid 2011 for selected location
#
# censusPp2011Location <- reactive({
#     censusPp2011 %>% filter(GeoUID == input$c_location)
# })

#
# Population pyramid 2006 for selected location
#
# censusPp2006Location <- reactive({
#     censusPp2006 %>% filter(GeoUID == input$c_location)
# })

locationLabel <- reactive({
  # if (is.null(input$c_location)) {
  #   return ("Please select a location")
  # } else {
  #   regionOptions() %>% filter(value == input$c_location) %>% select(label)
  # }
  locationLabel <- censusMobility() %>%
    filter(GeoUID == input$c_location) %>%
    mutate(label = paste0(Region, " (", GeoUID, ")")) %>%
    select(label)
  st_geometry(locationLabel) <- NULL
  return(locationLabel)
})

# censusPp2011 <- reactive({
#   censusStir <- switch(
#     input$c_view,
#     "CMA" = census2011ppCma,
#     "CSD" = census2011ppCsd,
#     "CD" = census2011ppCd,
#     "CT" = census2011ppCt,
#     "DA" = census2011ppDa
#   )
# })
#
# censusPp2006 <- reactive({
#   censusStir <- switch(
#     input$c_view,
#     "CMA" = census2006ppCma,
#     "CSD" = census2006ppCsd,
#     "CD" = census2006ppCd,
#     "CT" = census2006ppCt,
#     "DA" = census2006ppDa
#   )
# })

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
# Housing Types map
#
output$mapCensusHousingType <- renderLeaflet({
  leaflet(housingTypesCma) %>%
    addProviderTiles(provider = "CartoDB.Positron", options = providerTileOptions(minZoom = 6, maxZoom = 10)) %>%
    setView(lng = -122.12, lat = 51.78, zoom = 6) %>%
    addPolygons(
      label = ~ `Region`, color = '#333', fillColor = ~ palHousingTypes(housingTypesCma$`Single detached house ratio`),
      stroke = TRUE, weight = 1, fillOpacity = 0.5, smoothFactor = 0.2,
      layerId = ~ GeoUID,
      # fillOpacity = 0.5,
      # smoothFactor = 1,
      popup = paste0(
        "<strong>", paste0(housingTypesCma$`Region`, " (", housingTypesCma$GeoUID), ")</strong>",
        "<table class=\"leaflet-popup-table\"><tr><td>Census Year</td><td>2016</td></tr>",
        "<tr><td>Single family homes ratio</td><td><strong>",
        format(housingTypesCma$`Single detached house ratio`, big.mark = ","), "</strong></td></tr>",
        "<tr><td>Semi detached house ratio</strong></td><td>",
        format(housingTypesCma$`Semi detached house ratio`, big.mark = ","), "</strong></td></tr>",
        "<tr><td>Appartment in duplex ratio</strong></td><td>",
        format(housingTypesCma$`Appartment in duplex ratio`, big.mark = ","), "</strong></td></tr>",
        "<tr><td>Row house ratio</strong></td><td>",
        format(housingTypesCma$`Row house ratio`, big.mark = ","), "</strong></td></tr>",
        "<tr><td>Appartment in small building ratio</strong></td><td>",
        format(housingTypesCma$`Appartment in small building ratio`, big.mark = ","), "</strong></td></tr>",
        "<tr><td>Appartment in tall building ratio</strong></td><td>",
        format(housingTypesCma$`Appartment in tall building ratio`, big.mark = ","), "</strong></td></tr>",
        "<tr><td>Other single attached house ratio</strong></td><td>",
        format(housingTypesCma$`Other single attached house ratio`, big.mark = ","), "</strong></td></tr>",
        "<tr><td>Movable dwelling ratio</strong></td><td>",
        format(housingTypesCma$`Movable dwelling ratio`, big.mark = ","), "</strong></td></tr>",
        "</table>"
      ),
      highlight = highlightOptions(
        weight = 5, color = "#696969", dashArray = "", fillOpacity = 0.75, bringToFront = TRUE)
    ) %>%
    addLegend(
      "bottomleft", pal = palHousingTypes, values = ~ `Single detached house ratio`,
      title = "", opacity = 0.5
    )
})

#
# Housing Types observer
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

  # Housing Types palette
  palHousingTypes <- colorNumeric(
    palette = "viridis",
    domain = housingTypeMapData()$typewatch,
    na.color = "#e6e6e6"
  )

  # Redraw polygons when housing type selection changes
  leafletProxy("mapCensusHousingType", data = housingTypeMapData()) %>%
    clearShapes() %>%
    addPolygons(
      label = ~ `Region`, color = '#333',
      fillColor = ~ palHousingTypes(housingTypeMapData()$typewatch),
      # fillColor = ~ pal(housingTypeMapData()$typewatch),
      stroke = TRUE, weight = 1, fillOpacity = 0.5, smoothFactor = 0.2,
      layerId = ~ GeoUID,
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
    )

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

  # isolate({
  #   if (!c_loc() == '') {
  #     housingTypesDf %<>% filter(GeoUID == c_loc())
  #   }
  # })
  # if (!'' == input$c_location) {
  output$housingTypeTreemap <- renderPlot({
    # renderD3tree3({
    # d3tree3(
    par(mar=c(0,0,0,0), xaxs='i', yaxs='i')
    plot(c(0,1), c(0,1),axes=F, col="white")

      treemap(
        title = paste("Housing Types Distribution at ", locationLabel()),
        housingTypesDf %>% filter(GeoUID == input$c_location) %>% mutate(ratioFormat = paste0(gsub(" ratio", "", HousingType), " - ", ratio, "%")),
        index = c("ratioFormat"),
        vSize = "ratio",
        type = "index",
        vColor = "ratio",
        palette = c(colForeign, colResidential, colStrata, colAcreage, colMultiFam, colCommercial, colNonStrataRental, colSingleFam),
        algorithm = "pivotSize",
        # sortID = "HousingType",
        fontsize.title = c(14),
        fontsize.labels = c(12),                # size of labels. Give the size per level of aggregation: size for group, size for subgroup, sub-subgroups...
        fontcolor.labels = c("#121212"),    # Color of labels
        fontface.labels = c(1),                  # Font of labels: 1,2,3,4 for normal, bold, italic, bold-italic...
        bg.labels = c("#CCCCCCDC"),              # Background color of labels
        align.labels = list(
          c("center", "center")
        ),                                   # Where to place labels in the rectangle?
        overlap.labels = 0.5,                      # number between 0 and 1 that determines the tolerance of the overlap between labels. 0 means that labels of lower levels are not printed if higher level labels overlap, 1  means that labels are always printed. In-between values, for instance the default value .5, means that lower level labels are printed if other labels do not overlap with more than .5  times their area size.
        border.col = "#696969",
        border.lwds = c(1),
        force.print.labels = TRUE,
        inflate.labels = F

      )#,
    #   rootname = paste("Housing Types Distribution at ", locationLabel())
    # )
  })


  # Housing Types datatable
  output$housingTypesDT = DT::renderDataTable(datatable(
    housingTypeMapData() %>% select(Region, GeoUID, typewatch),
    # housingTypesDf,# %>% select(Region, GeoUID, typewatch),
      # filter(Type == input$c_search_data_level),
    filter = 'bottom',
    extensions = 'Buttons',
    options = list(
      pageLength = 25, autoWidth = TRUE, dom = 'Blfrtip',
      buttons = c('copy', 'csv', 'excel', 'pdf', 'print'),
      lengthChange = TRUE,
      initComplete = JS(
        "
        function(settings, json) {
        $(this.api().table().header()).css({
        'background-color': 'rgba(0, 51, 102, 0.80)',
        'border-bottom': '5px solid #fcba19',
        'color': '#fff'
        });
        }"
          )
      )
      ))

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
      "Non-movers", "Non-migrants",
      "External migrants", "Intraprovincial migrants", "Interprovincial migrants",
      key = "Migration", value = "count")

  # Mobility palette
  palMobility <- colorBin(
    palette = "viridis",
    domain = censusMobility$`Movers Ratio`, n = 10
  )
  palLightRed <- "#fc95a4"# "#feb87e"# "#e85361"# "#e08176"
  palLighterBlue <- colNonStrataRental
  palLightBlue <- colMultiFam
  palDarkBlue <- colSingleFam
  palOther <- colUnknown

  # Mobility Map
  output$mapCensusMobility <- renderLeaflet({
      leaflet(censusMobility) %>%
      addProviderTiles(provider = "CartoDB.Positron", options = providerTileOptions(minZoom = 6, maxZoom = 10)) %>%
      setView(lng = -122.12, lat = 51.78, zoom = 6) %>%
      addPolygons(
        label = ~ `Region`, color = '#333', fillColor = ~ palMobility(censusMobility$`Movers Ratio`),
        stroke = TRUE, weight = 1, fillOpacity = 0.5, smoothFactor = 0.2,
        layerId = ~ GeoUID,
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
      addLegend(
        "bottomleft", pal = palMobility, values = ~ `Movers Ratio`,
        title = "", opacity = 0.5
      )
  })

  # Have to drop geometry, i.e. convert sf to df to use in treemap
  censusMobilityDf <- censusMobility
  st_geometry(censusMobilityDf) <- NULL

  # isolate({
  #   if (!c_loc() == '') {
  #     censusMobilityDf %<>% filter(GeoUID == c_loc())
  #   }
  # })
  # if (!'' == input$c_location) {
  output$c16mobilityTree <- renderD3tree3({
    d3tree3(
      treemap(
        censusMobilityDf %>% filter(GeoUID == input$c_location),
        index = c("Migration", "Region"),
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
  # }

  #
  # Population Pyramid observer
  #
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
})

#
# STIR observer
#
observe({
  # SHELTER-COST-TO-INCOME RATIO
  censusStir <- st_as_sf(
    censusStir() %>% select(everything())
  )

  # STIR palette
  palStir <- colorBin(
    palette = "viridis",
    domain = censusStir$percent_more_than_30, n = 10
  )

  # STIR Map
  output$mapCensusStir <- renderLeaflet({
      leaflet(censusStir) %>%
      addProviderTiles(provider = "CartoDB.Positron", options = providerTileOptions(minZoom = 6, maxZoom = 10)) %>%
      setView(lng = -122.12, lat = 51.78, zoom = 6) %>%
      addPolygons(
        label = ~ `Region`, color = '#333', fillColor = ~ palStir(censusStir$percent_more_than_30),
        stroke = TRUE, weight = 1, fillOpacity = 0.75, smoothFactor = 0.2,
        layerId = ~ GeoUID,
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
      addLegend(
        "bottomleft", pal = palStir, values = ~ percent_more_than_30,
        title = "", opacity = 0.5
      )
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

# Mobility map click observer
observeEvent(input$mapCensusMobility_shape_click, {
  m <- input$mapCensusMobility_shape_click
  if(!is.null(m$id)){
    # updateSelectInput(session, "c_location", selected = p$id)
    updateTextInput(session, "c_location", value = m$id)
    locationLabel <- censusMobility() %>%
      filter(GeoUID == m$id) %>%
      mutate(label = paste0(Region, " (", GeoUID, ")")) %>%
      select(label)
    st_geometry(locationLabel) <- NULL
    updateTextInput(session, "c_location_name", value = locationLabel$label)
  }
})

# Housing Type map click observer
observeEvent(input$mapCensusHousingType_shape_click, {
  h <- input$mapCensusHousingType_shape_click
  if(!is.null(h$id)){
    # updateSelectInput(session, "c_location", selected = p$id)
    updateTextInput(session, "c_location", value = h$id)
    locationLabel <- censusMobility() %>%
      filter(GeoUID == h$id) %>%
      mutate(label = paste0(Region, " (", GeoUID, ")")) %>%
      select(label)
    st_geometry(locationLabel) <- NULL
    updateTextInput(session, "c_location_name", value = locationLabel$label)
  }
})

# HousingSTIR map click observer
observeEvent(input$mapCensusStir_shape_click, {
  s <- input$mapCensusStir_shape_click
  if(!is.null(s$id)){
    # updateSelectInput(session, "c_location", selected = p$id)
    updateTextInput(session, "c_location", value = s$id)
    locationLabel <- censusMobility() %>%
      filter(GeoUID == s$id) %>%
      mutate(label = paste0(Region, " (", GeoUID, ")")) %>%
      select(label)
    st_geometry(locationLabel) <- NULL
    updateTextInput(session, "c_location_name", value = locationLabel$label)
  }
})

