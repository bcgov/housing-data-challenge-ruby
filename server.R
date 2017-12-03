server <- function(input, output, session) {
  # observeEvent(input$map_shape_click, {
  #     #create object for clicked polygon
  #     click <- input$map_shape_click
  #
  #     #define leaflet proxy for second regional level map
  #     proxy <- leafletProxy("map")
  #
  #     #subset regions shapefile by the clicked on polygons
  #     selectedReg <-
  #         bcCensusDivsMap[bcCensusDivsMap@data$OBJECTID == click$id, ]
  #
  #     output$text <- renderText({
  #         paste0(" Selected CDNAME: ", selectedReg@data$CDNAME)
  #     })
  #
  #     output$text2 <- renderText({
  #         paste0(" Selected regional district: ",
  #                selectedReg@data$RegionalDistrict)
  #     })
  #
  #     #map clicked on polygons
  #     proxy %>% addPolygons(
  #         data = selectedReg,
  #         fillColor = "red",
  #         fillOpacity = 0.7,
  #         weight = 1,
  #         color = "black",
  #         stroke = T,
  #         group = "selected",
  #         # layerId = "selected")
  #         layerId = selectedReg@data$OBJECTID
  #     )
  #
  #
  #     #remove polygon group that are clicked twice
  #     if (click$group == "selected") {
  #         proxy %>%
  #             clearGroup(group = "selected")
  #     }
  #
  # })

  # This observer is responsible for maintaining the circles and legend,
  # according to the variables the user has chosen to map to color and size.
  observe({
    pt_view <- input$pt_view
    pt_trans_period <- input$pt_trans_period
    pt_metric <- input$pt_metric

    c_view <- input$c_view
    c_year <- input$c_year
    c_metric <- input$c_metric

    # Census observer switch
    switch(
      c_view,
      "cma" = {
        censusData <- readRDS("./data/census2016-CMA.rds")
        censusDataSpatial <-
          readRDS("./data/censusSpatial2016-CMA.rds")
      },
      "csd" = {
        censusData <- readRDS("./data/census2016-CSD.rds")
        censusDataSpatial <-
          readRDS("./data/censusSpatial2016-CSD.rds")
      },
      "cd" = {
        censusData <- readRDS("./data/census2016-CD.rds")
        censusDataSpatial <-
          readRDS("./data/censusSpatial2016-CD.rds")
      },
      "ct" = {
        censusData <- readRDS("./data/census2016-CT.rds")
        censusDataSpatial <-
          readRDS("./data/censusSpatial2016-CT.rds")
      }
    )
    censusCategories <- label_vectors(censusData)

    output$mapCensus <- renderLeaflet({
      censusDataSpatial %>%
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
          fillColor = ~ palViridis(censusDataSpatial$v_CA16_2447),
          popup = paste0(
            "<strong>",
            paste(censusDataSpatial$`Region Name`),
            "</strong>",
            "<table class=\"leaflet-popup-table\">
            <tr><td>Census Year</td><td>2016</td></tr>",
            "<tr><td>Population</td><td>",
            format(censusDataSpatial$Population, big.mark = ","),
            "</td></tr><tr><td>Dwellings</td><td>",
            format(censusDataSpatial$Dwellings, big.mark = ","),
            "</td></tr><tr><td>Households</td><td>",
            format(censusDataSpatial$Households, big.mark = ","),
            "</td></tr><tr><td>Median total income</td><td>",
            paste("$", format(censusDataSpatial$v_CA16_2447, big.mark = ","), sep =
                    ""),
            "</td></tr><tr><td>Average Age</td><td>",
            censusDataSpatial$v_CA16_379,
            "</td></tr><tr><td>Provate dwellings occupied by usual residents</td><td>",
            format(censusDataSpatial$v_CA16_405, big.mark = ","),
            "</td></tr><tr><td>Single detahed houses</td><td>",
            format(censusDataSpatial$v_CA16_412, big.mark = ","),
            "</td></tr><tr><td>Average family size</td><td>",
            format(censusDataSpatial$v_CA16_2449, big.mark = ","),
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
        x = censusData$`Region Name`,
        y = censusData$v_CA16_410,
        color = colNonStrataRental,
        name = "Appartment in tall building"
      ),
      list(
        x = censusData$`Region Name`,
        y = censusData$v_CA16_412,
        color = colCanadian,
        name = "Semi-detached house"
      ),
      list(
        x = censusData$`Region Name`,
        y = censusData$v_CA16_413,
        color = colResidential,
        name = "Row house"
      ),
      list(
        x = censusData$`Region Name`,
        y = censusData$v_CA16_414,
        color = colMultiFam,
        name = "Appartment in duplex"
      ),
      list(
        x = censusData$`Region Name`,
        y = censusData$v_CA16_415,
        color = colStrata,
        name = "Appartment in small building"
      ),
      list(
        x = censusData$`Region Name`,
        y = censusData$v_CA16_416,
        color = colAcreage,
        name = "Other single-attached house"
      ),
      list(
        x = censusData$`Region Name`,
        y = censusData$v_CA16_417,
        color = colForeign,
        name = "Movable dwelling"
      )
    )
    c16dwellTypePlot <- plotmy(
      censusData,
      censusData$`Region Name`,
      censusData$v_CA16_409,
      "Single-detached house",
      "bar",
      colSingleFam,
      "Type of dwelling",
      traces
    )
    output$c16dwellType <- renderPlotly({
      c16dwellTypePlot
    })

    # Income
    traces = list()

    c16incomeTotalMed <- plotmy(
      censusData,
      censusData$`Region Name`,
      censusData$v_CA16_2447,
      "Median total income of economic families in 2015 ($)",
      "bar",
      colResidential,
      "Median after-tax income of economic families in 2015 ($)",
      traces
    )
    output$c16incomeTotalMed <- renderPlotly({
      c16incomeTotalMed
    })

    c16incomeTotalMedaT <- plotmy(
      censusData,
      censusData$`Region Name`,
      censusData$v_CA16_2448,
      "Median total income of economic families in 2015 ($)",
      "bar",
      colResidential,
      "Median after-tax income of economic families in 2015 ($)",
      traces
    )
    output$c16incomeTotalMedaT <-
      renderPlotly({
        c16incomeTotalMedaT
      })

    c16incomeAvgFamSize <- plotmy(
      censusData,
      censusData$`Region Name`,
      censusData$v_CA16_2449,
      "Average Family Size",
      "bar",
      colResidential,
      "Average Family Size",
      traces
    )
    output$c16incomeAvgFamSize <-
      renderPlotly({
        c16incomeAvgFamSize
      })

    # Age
    c16avgAge <- plotmy(
      censusData,
      censusData$`Region Name`,
      censusData$v_CA16_379,
      "Average Age",
      "bar",
      colResidential,
      "Average Age",
      traces
    )
    output$c16avgAge <- renderPlotly({
      c16avgAge
    })

    c16ageDist <- plotmy(
      censusData,
      censusData$`Region Name`,
      censusData$v_CA16_382,
      "Age group distribution",
      "bar",
      colResidential,
      "Age group distribution",
      traces
    )
    output$c16ageDist <- renderPlotly({
      c16ageDist
    })



    # PTT observer switch
    switch(pt_view,
           "regdis" = {
             propertyTaxPeriod <- ptRegDisMth %>%
               filter(trans_period %in% pt_trans_period)

             propertyTaxPeriod$geoUnit <-
               propertyTaxPeriod$Regional.District

             # For use in overview charts
             propertyTax <- ptRegDisMth
             propertyTax$geoUnit <-
               ptRegDisMth$Regional.District

             c16 <- c16Divs
             c16$geoUnitVal <- c16Divs$CDNAME

             propertyTax <-
               merge(
                 propertyTax,
                 c16,
                 by.x = "Regional.District",
                 by.y = "CDNAME",
                 sort = FALSE,
                 by = ALL
               )

             # Convert join columns to uppercase to avoid mismatches due to case sensitivity
             bcCensusDivs@data$CDNAME <-
               toupper(bcCensusDivs@data$CDNAME)
             geoUnit <- as.character(bcCensusDivs$CDNAME)
             byY <- "Regional.District"
             shapesDF <-
               merge(
                 bcCensusDivs,
                 propertyTaxPeriod,
                 by.x = "CDNAME",
                 by.y = "Regional.District",
                 sort = FALSE,
                 by = ALL
               )
           },
           "devreg" = {
             propertyTaxPeriod <- ptDevRegMth %>%
               filter(trans_period %in% pt_trans_period) %>%
               arrange_(.dots = c(paste0("desc(", pt_metric, ")")))

             propertyTaxPeriod$geoUnit <-
               propertyTaxPeriod$DevelopmentRegion

             c16 <- c16EconRegs
             c16$geoUnitVal <- c16EconRegs$ERNAME
             c16$Total.Private.Dwellings.Change <- 0

             # For use in overview charts
             propertyTax <- ptDevRegMth
             propertyTax$geoUnit <-
               ptDevRegMth$DevelopmentRegion

             propertyTax <-
               merge(
                 propertyTax,
                 c16,
                 by.x = "DevelopmentRegion",
                 by.y = "ERNAME",
                 sort = FALSE,
                 by = ALL
               )

             # Convert join columns to uppercase to avoid mismatches due to case sensitivity
             erData <- bcCensusEconRegs@data
             erData <-
               separate(
                 data = erData,
                 col = ERNAME,
                 into = c("ERNAME_E", "ERNAME_F"),
                 sep = "\\/",
                 fill = "right"
               )
             erData$ERNAME_E <-
               trimws(toupper(gsub("--", "/", erData$ERNAME_E)))
             bcCensusEconRegs@data$ERNAME <- erData$ERNAME_E

             geoUnit <- as.character(bcCensusEconRegs$ERNAME)
             byY <- "DevelopmentRegion"
             shapesDF <-
               merge(
                 bcCensusEconRegs,
                 propertyTaxPeriod,
                 by.x = "ERNAME",
                 by.y = "DevelopmentRegion",
                 sort = FALSE,
                 by = ALL
               )
           },
           "mun" = {
             propertyTaxPeriod <- ptMunMth %>%
               filter(trans_period %in% pt_trans_period) %>%
               arrange_(.dots = c(paste0("desc(", pt_metric, ")")))

             propertyTaxPeriod$geoUnit <-
               propertyTaxPeriod$Municipality

             c16 <- c16MetroAreas
             c16$geoUnitVal <- c16MetroAreas$CMANAME

             # For use in overview charts
             propertyTax <- ptMunMth
             propertyTax$geoUnit <- ptMunMth$Municipality

             propertyTax <-
               merge(
                 propertyTax,
                 c16,
                 by.x = "Municipality",
                 by.y = "CMANAME",
                 sort = FALSE,
                 by = ALL
               )

             # Convert join columns to uppercase to avoid mismatches due to case sensitivity
             bcCensusMetroAreas@data$CMANAME <-
               toupper(bcCensusMetroAreas@data$CMANAME)

             geoUnit <-
               as.character(bcCensusMetroAreas$CMANAME)
             byY <- "Municipality"
             shapesDF <-
               merge(
                 bcCensusMetroAreas,
                 propertyTaxPeriod,
                 by.x = "CMANAME",
                 by.y = "Municipality",
                 sort = FALSE,
                 by = ALL
               )
           })

    # reorder data for chart sorting
    propertyTaxPeriod$geoUnit <-
      factor(propertyTaxPeriod$geoUnit,
             levels = unique(propertyTaxPeriod$geoUnit)
             [order(propertyTaxPeriod[[pt_metric]], decreasing = FALSE)])
    c16$geoUnitVal <- factor(c16$geoUnitVal,
                             levels = unique(c16$geoUnitVal)
                             [order(c16$Population.2016, decreasing = FALSE)])

    pal <-
      colorQuantile("viridis", n = 9, shapesDF[[pt_metric]])
    # pal <- colorBin("YlGnBu", shapesDF[[pt_metric]])
    # pal <- colorNumeric("viridis", shapesDF[[pt_metric]])
    data <- shapesDF@data

    output$map <- renderLeaflet({
      leaflet(shapesDF) %>%
        setView(lng = -125,
                lat = 53,
                zoom = 5) %>%
        addTiles(group = "OpenStreetMap") %>%
        addProviderTiles("CartoDB.Positron") %>%
    
        addPolygons(
          data = shapesDF,
          stroke = TRUE,
          weight = 1,
          fillOpacity = 0.5,
          smoothFactor = 1,
          color = '#333',
          layerId = shapesDF@data$OBJECTID,
          fillColor = ~ pal(shapesDF[[pt_metric]]),
          label = geoUnit,
          popup = paste0(
            "<strong>",
            geoUnit,
            "</strong>",
            "<table class=\"leaflet-popup-table\">
            <tr><td>Period</td><td>",
            as.Date(shapesDF$trans_period),
            "</td></tr><tr><td>Number of transactions</td><td>",
            format(shapesDF$no_mkt_trans, big.mark = ","),
            "</td></tr><tr><td>Number of foreign transactions</td><td>",
            format(shapesDF$no_foreign, big.mark = ","),
            "</td></tr><tr><td>Number % by foreign purchasers</td><td>",
            format(shapesDF$no_foreign_perc, big.mark = ","),
            "</td></tr><tr><td>Total value</td><td>",
            paste("$", format(shapesDF$sum_FMV, big.mark = ","), sep =
                    ""),
            "</td></tr><tr><td>Total value by foreign purchasers</td><td>",
            paste("$",
                  format(shapesDF$sum_FMV_foreign, big.mark = ","),
                  sep = ""),
            "</td></tr><tr><td>Value % by foreign purchasers</td><td>",
            format(shapesDF$sum_FMV_foreign_perc, big.mark = ","),
            "</td></tr><tr><td>PTT Paid</td><td>",
            paste("$",
                  format(shapesDF$sum_PPT_paid, big.mark = ","),
                  sep = ""),
            "</td></tr><tr><td>Additional Tax Paid</td><td>",
            paste("$",
                  format(shapesDF$add_tax_paid, big.mark = ","),
                  sep = ""),
            "</td></tr></table>"
          ),
          # group = "divisions",
          highlight = highlightOptions(
            weight = 5,
            color = "#696969",
            dashArray = "",
            fillOpacity = 0.5,
            bringToFront = TRUE)
        ) %>%
        addLegend(
          "bottomleft",
          pal = pal,
          values = shapesDF$no_mkt_trans,
          title = "Transactions #",
          labFormat = labelFormat(prefix = "$"),
          opacity = 0.8
        ) %>%
        # addLayersControl(
        #     overlayGroups = c("Census Divisions"),
        #     options = layersControlOptions(collapsed = FALSE)
        # ) %>%
        clearGroup(group = "selected")
    })

    output$dt = DT::renderDataTable(
      datatable(
        propertyTaxPeriod,
        #%>%
        # select_(.dots = selectionMetricsDF$Metric),
        options = list(
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
          ),
colnames = allMetrics,
selection = list(target = 'row+column')
          ) %>%
  formatCurrency(
    c(
      "FMV Sum",
      "FMV Average",
      "FMV Median",
      "PTT Paid",
      "PTT Paid Median",
      "FMV sum of Foreign Transactions",
      "FMV Mean of Foreign Transactions",
      "FMV Median of Foreign Transactions",
      "Additional Tax Paid"
    ),
    currency = "$",
    digits = 0
  ) %>%
  formatCurrency(
    c(
      "Total Market Transactions #",
      "Res. Total #",
      "Res. - Acreage #",
      "Res. - Commerce #",
      "Res. - Farm #",
      "Res. - Multi-family #",
      "Res. - Single-family Res. #",
      "Res. - Strata Res. #",
      "Res. - Strata Non- Res. or Rental #",
      "Res. - Other #",
      "Comm. Total #",
      "Comm. - Comm. #",
      "Comm. - Strata Non-Res. #",
      "Comm. - Other #",
      "Recr. Total #",
      "Farm Total #",
      "Other/Unknown Total #",
      "Foreign Transactions #"
    ),
    currency = "",
    digits = 0
  )
      )

    # Interactive based on user input
    output$interactive <- renderPlotly({
      plot_ly(
        propertyTaxPeriod,
        x = ~ propertyTaxPeriod[[pt_metric]],
        y = ~ geoUnit,
        type = "bar",
        orientation = "h"
      ) %>%
        layout(
          title = pt_metric,
          #"Number of market transactions",
          xaxis = axisFormat,
          yaxis = axisFormat,
          margin = marginFormatMonthly,
          barmode = 'group',
          legend = legendFormat
        ) %>%
        config(displayModeBar = F)
    })

    # Census 2016 population
    # traces = list(
    #   list(
    #     x = c16$Population.2011,
    #     y = c16$geoUnitName,
    #     color = colC11,
    #     name = "Population 2011"
    #   )
    # )
    # c16popPlot <- plotmy(
    #   c16,
    #   c16$Population.2016,
    #   c16$geoUnitVal,
    #   "Population 2016",
    #   "bar",
    #   list(color = colC16),
    #   "Census Population",
    #   traces
    # )
    #
    # output$c16pop <- renderPlotly({c16popPlot})
    output$c16pop <- renderPlotly({
      plot_ly(
        c16,
        y = ~ geoUnitVal,
        x = ~ Population.2016,
        name = "Population 2016",
        marker = list(color = colC16),
        type = "bar"
      ) %>%
        add_trace(
          x = ~ Population.2011,
          name = "Population 2011",
          marker = list(color = colC11)
        ) %>%
        layout(
          title = "Census Population",
          xaxis = axisFormat,
          yaxis = axisFormat,
          margin = marginFormatMonthly,
          barmode = 'group',
          legend = legendFormat
        ) %>%
        config(displayModeBar = F)
    })

    # Census 2016 dwellings
    output$c16dwell <- renderPlotly({
      plot_ly(
        c16,
        y = ~ geoUnitVal,
        x = ~ Total.Private.Dwellings.2016,
        name = "Dwellings 2016",
        marker = list(color = colC16),
        type = "bar"
      ) %>%
        add_trace(
          x = ~ Total.Private.Dwellings.2011,
          name = "Dwellings 2011",
          marker = list(color = colC11)
        ) %>%
        layout(
          title = "Census Private Dwellings",
          xaxis = axisFormat,
          yaxis = axisFormat,
          margin = marginFormatMonthly,
          barmode = 'group',
          legend = legendFormat
        ) %>%
        config(displayModeBar = F)
    })

    # Census 2016 change
    output$c16change <- renderPlotly({
      plot_ly(
        c16,
        y = ~ geoUnitVal,
        x = ~ Population.Change,
        name = "Population",
        marker = list(color = colC16),
        type = "bar"
      ) %>%
        add_trace(
          x = ~ Total.Private.Dwellings.Change,
          name = "Dwellings",
          marker = list(color = colC11)
        ) %>%
        layout(
          title = "Change from 2011 Census (%)",
          xaxis = axisFormat,
          yaxis = axisFormat,
          margin = marginFormatMonthly,
          # barmode = 'group',
          legend = legendFormat
        ) %>%
        config(displayModeBar = F)
    })

    # add_lines(y = ~Total.Private.Dwellings.2016, name = "Total.Private.Dwellings.2016",

    # Foreign - Number of Foreign Transactions
    output$no_foreign_period <- renderPlotly({
      plot_ly(
        propertyTaxPeriod %>%
          arrange(desc(no_foreign)),
        x = ~ geoUnit,
        y = ~ no_foreign,
        type = "bar",
        marker = list(color = colForeign)
      ) %>%
        layout(
          title = "Number of Foreign Transactions",
          xaxis = axisFormat,
          yaxis = axisFormat,
          margin = marginFormat,
          legend = legendFormat
        ) %>%
        config(displayModeBar = F)
    })

    # Foreign - FMV Mean
    output$foreign_period_mn <- renderPlotly({
      plot_ly(
        propertyTaxPeriod %>%
          filter(!is.na(mn_FMV_foreign)) %>%
          arrange(desc(mn_FMV)),
        x = ~ geoUnit,
        y = ~ mn_FMV,
        name = "Canadian",
        marker = list(color = colCanadian),
        type = "bar"
      ) %>%
        add_trace(
          y = ~ mn_FMV_foreign,
          name = "Foreign",
          marker = list(color = colForeign)
        ) %>%
        layout(
          title = "FMV Mean",
          xaxis = axisFormat,
          yaxis = axisFormat,
          margin = marginFormat,
          barmode = 'group',
          legend = legendFormat
        ) %>%
        config(displayModeBar = F)
    })

    # Foreign - FMV Median
    output$foreign_period_md <- renderPlotly({
      plot_ly(
        propertyTaxPeriod %>%
          filter(!is.na(md_FMV_foreign)) %>%
          arrange(desc(md_FMV)),
        x = ~ geoUnit,
        y = ~ md_FMV,
        name = "Canadian",
        marker = list(color = colCanadian),
        type = "bar"
      ) %>%
        add_trace(
          y = ~ md_FMV_foreign,
          name = "Foreign",
          marker = list(color = colForeign)
        ) %>%
        layout(
          title = "FMV Median",
          xaxis = axisFormat,
          yaxis = axisFormat,
          margin = marginFormat,
          barmode = 'group',
          legend = legendFormat
        ) %>%
        config(displayModeBar = F)
    })

    })

  # Monthly Overview - FMV (Fair Market Value)
  output$pt_mothly_fmv <- renderPlotly({
    plot_ly(
      ptProvMth,
      x = ~ trans_period,
      y = ~ sum_FMV,
      name = "Total FMV",
      type = 'scatter',
      mode = 'lines',
      line = list(shape = "spline", color = colCanadian)
    ) %>%
      add_lines(
        y = ~ sum_FMV_foreign,
        name = "Total FMV Foreign",
        line = list(shape = "spline", color = colForeign)
      ) %>%
      add_lines(
        y = ~ no_foreign_perc,
        name = "Foreign %",
        yaxis = "y2",
        line = list(
          shape = "spline",
          color = colForeign,
          dash = 'dot'
        )
      ) %>%
      layout(
        title = "FMV (Fair Market Value)",
        xaxis = axisFormat,
        yaxis = axisFormat,
        yaxis2 = list(
          tickfont = tickfontRd,
          overlaying = "y",
          side = "right",
          title = "Foreign %"
        ),
        margin = marginFormat,
        legend = legendFormat
      ) %>%
      config(displayModeBar = F)
  })

  # Monthly Overview - Average FMV
  output$pt_mothly_mnd_fmv <- renderPlotly({
    plot_ly(
      ptProvMth,
      x = ~ trans_period,
      y = ~ mn_FMV,
      name = "Mean FMV",
      type = 'scatter',
      mode = 'lines',
      line = list(shape = "spline", color = colCanadian)
    ) %>%
      add_lines(
        y = ~ mn_FMV_foreign,
        name = "Mean FMV Foreign",
        line = list(shape = "spline", color = colForeign)
      ) %>%
      add_lines(
        y = ~ md_FMV,
        name = "Median FMV",
        line = list(
          shape = "spline",
          color = colCanadian,
          dash = 'dot'
        )
      ) %>%
      add_lines(
        y = ~ md_FMV_foreign,
        name = "Median FMV Foreign",
        line = list(
          shape = "spline",
          color = colForeign,
          dash = 'dot'
        )
      ) %>%
      layout(
        title = "Average FMV",
        xaxis = axisFormat,
        yaxis = axisFormat,
        margin = marginFormat,
        legend = legendFormat
      ) %>%
      config(displayModeBar = F)
  })

  # Monthly Overview - Property Transfer Tax
  output$pt_mothly_ptt <- renderPlotly({
    plot_ly(
      ptProvMth,
      x = ~ trans_period,
      y = ~ sum_PPT_paid,
      name = "PTT",
      type = 'scatter',
      mode = 'lines',
      line = list(shape = "spline", color = colCanadian)
    ) %>%
      add_lines(
        y = ~ add_tax_paid,
        name = "Additional PTT",
        line = list(shape = "spline", color = colForeign)
      ) %>%
      layout(
        title = "Property Transfer Tax",
        xaxis = axisFormat,
        yaxis = axisFormat,
        margin = marginFormat,
        legend = legendFormat
      ) %>%
      config(displayModeBar = F)
  })

  # Monthly Overview - Number of market transactions
  output$pt_mothly <- renderPlotly({
    plot_ly(
      ptProvMth,
      x = ~ trans_period,
      y = ~ no_resid_trans,
      name = "Residential",
      type = "bar",
      marker = list(color = colResidential),
      hoverinfo = "y+name"
    ) %>%
      add_trace(
        y = ~ no_comm_tot,
        name = "Commercial",
        marker = list(color = colCommercial)
      ) %>%
      add_trace(
        y = ~ no_recr_tot,
        name = "Recreational",
        marker = list(color = colRecreational)
      ) %>%
      add_trace(
        y = ~ no_farm_tot,
        name = "Farms",
        marker = list(color = colFarms)
      ) %>%
      add_trace(
        y = ~ no_unkn_tot,
        name = "Unknown",
        marker = list(color = colUnknown)
      ) %>%
      layout(
        title = "Number of market transactions",
        xaxis = axisFormat,
        yaxis = axisFormat,
        margin = marginFormat,
        barmode = 'stack',
        legend = legendFormat
      ) %>%
      config(displayModeBar = F)
  })

  # Monthly Overview - Number of market transactions - Residential
  output$pt_mothly_res <- renderPlotly({
    plot_ly(
      ptProvMth,
      x = ~ trans_period,
      y = ~ no_res_1fam,
      name = "Single Family",
      type = "bar",
      marker = list(color = colSingleFam),
      hoverinfo = "y+name"
    ) %>%
      add_trace(
        y = ~ no_resid_fam,
        name = "Multi Family",
        marker = list(color = colMultiFam)
      ) %>%
      add_trace(
        y = ~ no_resid_strata,
        name = "Strata",
        marker = list(color = colStrata)
      ) %>%
      add_trace(
        y = ~ no_resid_non_strata,
        name = "Non-strata / Rental",
        marker = list(color = colNonStrataRental)
      ) %>%
      add_trace(
        y = ~ no_resid_acreage_trans,
        name = "Acreage",
        marker = list(color = colAcreage)
      ) %>%
      add_trace(
        y = ~ resid_comm_count,
        name = "Commercial",
        marker = list(color = colCommercial)
      ) %>%
      add_trace(
        y = ~ no_resid_farm,
        name = "Farm",
        marker = list(color = colFarms)
      ) %>%
      add_trace(
        y = ~ no_resid_other,
        name = "Other",
        marker = list(color = colUnknown)
      ) %>%
      layout(
        title = "Number of market transactions - Residential",
        xaxis = axisFormat,
        yaxis = axisFormat,
        margin = marginFormat,
        barmode = 'stack',
        legend = legendFormat
      ) %>%
      config(displayModeBar = F)
  })

  # Monthly Overview - Number of market transactions - Commercial
  output$pt_mothly_comm <- renderPlotly({
    plot_ly(
      ptProvMth,
      x = ~ trans_period,
      y = ~ no_comm_comm,
      name = "Commerce",
      type = "bar",
      marker = list(color = colCommercial),
      hoverinfo = "y+name"
    ) %>%
      add_trace(
        y = ~ no_comm_strata_nores,
        name = "Strata non-residential",
        marker = list(color = colStrata)
      ) %>%
      add_trace(
        y = ~ no_comm_other,
        name = "Other",
        marker = list(color = colUnknown)
      ) %>%
      layout(
        title = "Number of market transactions - Commercial",
        xaxis = axisFormat,
        yaxis = axisFormat,
        margin = marginFormat,
        barmode = 'stack',
        legend = legendFormat
      ) %>%
      config(displayModeBar = F)
  })

  # Search census vectors by keyword
  observeEvent(input$c_search, {
    vectorsSearch <-
      search_census_vectors(
        input$c_search_keyword,
        paste0('CA', substr(paste0(
          input$c_search_year
        ), 3, 4)),
        type = "Total",
        use_cache = TRUE
      ) # %>%
      if (nrow(vectorsSearch) > 0) {
        vectorsSearch <- vectorsSearch %>%
          child_census_vectors(leaves_only = FALSE)
      }
      # https://github.com/mountainMath/cancensus/pull/95
      # child_census_vectors(leaves_only = FALSE)

    output$c_dt = DT::renderDataTable(datatable(
      vectorsSearch,
      #%>%
      # select_(.dots = selectionMetricsDF$Metric),
      options = list(
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
    })

    # Search census data by vector
  observeEvent(input$c_search_data, {
    vectorsSearch <-
      search_census_vectors(
        input$c_search_vector,
        paste0('CA', substr(paste0(
          input$c_search_year
        ), 3, 4)),
        type = "Total",
        use_cache = TRUE
      )  %>%
      child_census_vectors(leaves_only = FALSE)

    censusDataSearch <-
      get_census(
        # paste0('CA', substr(paste0(
        #   input$c_search_data_year
        # ), 3, 4)),
        level = "CD",
        regions = regions,
        vectors = input$c_search_vector,
        use_cache = TRUE,
        api_key = "CensusMapper_f17c13c7fc5e60de7cdd341d5d4db866",
        labels = "short",
        geo_format = NA
      )

    output$c_dt_data = DT::renderDataTable(datatable(
      censusDataSearch,
      #%>%
      # select_(.dots = selectionMetricsDF$Metric),
      options = list(
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
    })
  }
