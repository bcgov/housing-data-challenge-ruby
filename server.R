server <- function(input, output, session) {

  censusSearchDataset <- reactive({
    paste0('CA', substr(paste0(
      input$c_search_data_year
    ), 3, 4))
  })

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
    censusMobility <- read_rds("./data/census2016-mobility-CMA.rds")
    switch(
      c_view,
      "cma" = {
        censusData <- readRDS("./data/census2016-CMA.rds")
        censusDataSpatial <-
          readRDS("./data/censusSpatial2016-CMA.rds")
        censusMobility <- read_rds("./data/census2016-mobility-CMA.rds")
      },
      "csd" = {
        censusData <- readRDS("./data/census2016-CSD.rds")
        censusDataSpatial <-
          readRDS("./data/censusSpatial2016-CSD.rds")
        censusMobility <- read_rds("./data/census2016-mobility-CSD.rds")
      },
      "cd" = {
        censusData <- readRDS("./data/census2016-CD.rds")
        censusDataSpatial <-
          readRDS("./data/censusSpatial2016-CD.rds")
        censusMobility <- read_rds("./data/census2016-mobility-CD.rds")
      },
      "ct" = {
        censusData <- readRDS("./data/census2016-CT.rds")
        censusDataSpatial <-
          readRDS("./data/censusSpatial2016-CT.rds")
        censusMobility <- read_rds("./data/census2016-mobility-CT.rds")
      },
      "cda" = {
        censusData <- readRDS("./data/census2016-CT.rds")
        censusDataSpatial <-
          readRDS("./data/censusSpatial2016-CT.rds")
        censusMobility <- read_rds("./data/census2016-mobility-DA.rds")
      }
    )
    censusCategories <- label_vectors(censusData)

    censusMobility %<>%
      mutate(`Region Name` = as.character(`Region Name`), Type = as.character(Type))
    regionOptions <- censusMobility %>%
      mutate(label = paste0(censusMobility$`Region Name`, " (", censusMobility$GeoUID, ")")) %>%
      select(label, value = GeoUID)
    updateSelectizeInput(session, 'c_location', choices = regionOptions, server = TRUE)

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


    censusMobilityTM <- censusMobility %>%
      gather(
        "Non-movers", "Non-migrants",
        "External migrants", "Intraprovincial migrants", "Interprovincial migrants",
        key = "Migration", value = "count")


    palLightRed <- "#fc95a4"# "#feb87e"# "#e85361"# "#e08176"
    palLighterBlue <- colNonStrataRental
    palLightBlue <- colMultiFam
    palDarkBlue <- colSingleFam
    palOther <- colUnknown

    # Mobility
    output$c16mobilityTree <- renderD3tree3({
      d3tree3(
        treemap(
          censusMobilityTM,
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

  allVectors <- reactive({
    list_census_vectors(censusSearchDataset(), use_cache = TRUE)
  })

    # Search census data by vector
  observeEvent(input$c_search_data, {
    vectorsSearch <-
      search_census_vectors(
        input$c_search_vector,
        censusSearchDataset(),
        type = "Total",
        use_cache = TRUE
      )  %>%
      child_census_vectors(leaves_only = FALSE)

    censusDataSearch <-
      get_census(
        censusSearchDataset(),
        level = input$c_search_data_level,
        regions = list(PR = "59"),
        vectors = input$c_search_vector,
        use_cache = TRUE,
        api_key = "CensusMapper_f17c13c7fc5e60de7cdd341d5d4db866",
        labels = "short",
        geo_format = NA
      )

    output$c_search_vector_desc <- renderTable(allVectors() %>% filter(vector == input$c_search_vector))
    output$c_dt_data = DT::renderDataTable(datatable(
      censusDataSearch %>%
        filter(Type == input$c_search_data_level),
      #%>%
      # select_(.dots = selectionMetricsDF$Metric),
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
    })

  # POPULATION PYRAMID
  # ggplot(censusPP %>% filter(GeoUID == "59915"), aes(x = age, fill = sex)) +
  #   geom_bar(aes(y = percentage), stat = "identity", position = "identity",
  #            color = "gray10", size = 0.1,
  #            data = filter(censusPP, sex == "male")) +
  #   geom_bar(aes(y = -1 * percentage), stat = "identity", position = "identity",
  #            color = "gray10", size = 0.1,
  #            data = filter(censusPP, sex == "female")) +
  #   geom_text(aes(label = `Region Name`), y = 0.06, x = 19.5, #family = "Calibri",
  #             size = 3) +
  #   # facet_grid(geo ~ `Region Name`, scales = "fixed") +
  #   coord_flip() +
  #   scale_fill_brewer(palette = "Pastel1") +
  #   labs(x = NULL, y = NULL, title = NULL, fill = NULL) +
  #   theme_bw()
  #
  # final.pop <- censusPP %>%
  #   mutate(abspercentage = percentage) %>%
  #   mutate(percentage = ifelse(sex == "male", percentage * -1, percentage))
  #
  # plot_ly(final.pop, x = ~percentage, y = ~age, color = ~sex, type = 'bar', orientation = 'h',
  #         hoverinfo = 'y+text+name', text = ~percentage, colors = c('salmon', 'lightblue')) %>%
  #   layout(bargap = 0.1, barmode = 'overlay',
  #          margin(l = 250),
  #          xaxis = list(tickmode = 'array'#,
  #                       # tickvals = c(-10000, -5000, 0, 5000, 10000),
  #                       # ticktext = c('10k', '5k', '0', '5k', '10k')
  #          ),
  #          title = 'Population Pyramid for Kelowna - Census 2016')


  # STIR
  observe({
    c_year <- input$c_year
    c_view <- input$c_view
    c_location <- input$c_location

    # Census observer switch
    censusStir <- census2016CmaStir
    censusPp2016 <- census2016ppPr
    censusPp2011 <- census2011ppPr
    censusPp2006 <- census2006ppPr
    switch(
      c_view,
      "CMA" = {
        # censusStir <- census2016CsdStir
        censusPp2016 <- census2016ppCma
        censusPp2011 <- census2011ppCma
        censusPp2006 <- census2006ppCma
      },
      "CSD" = {
        censusStir <- census2016CsdStir
        censusPp2016 <- census2016ppCsd
        censusPp2011 <- census2011ppCsd
        censusPp2006 <- census2006ppCsd
      },
      "CD" = {
        censusStir <- census2016CdStir
        censusPp2016 <- census2016ppCd
        censusPp2011 <- census2011ppCd
        censusPp2006 <- census2006ppCd
      },
      "CT" = {
        censusStir <- census2016CtStir
        censusPp2016 <- census2016ppCt
        censusPp2011 <- census2011ppCt
        censusPp2006 <- census2006ppCt
      },
      "DA" = {
        censusStir <- census2016DaStir
        censusPp2016 <- census2016ppDa
        censusPp2011 <- census2011ppDa
        censusPp2006 <- census2006ppDa
      }
    )

    # Population Pyramid
    censusPp2016 %<>%
      filter(GeoUID == c_location)
    censusPp2011 %<>%
      filter(GeoUID == c_location)
    censusPp2006 %<>%
      filter(GeoUID == c_location)
    output$popPyr <- renderPlotly(
      plot_ly(censusPp2016, x = ~percentage, y = ~age, color = ~sex, type = 'bar', orientation = 'h',
              hoverinfo = 'y+text+name', text = ~percentage, colors = c('salmon', 'lightblue')) %>%
        layout(bargap = 0.1, barmode = 'overlay',
               margin = list(l = 250),
               xaxis = list(tickmode = 'array'#,
                            # tickvals = c(-10000, -5000, 0, 5000, 10000),
                            # ticktext = c('10k', '5k', '0', '5k', '10k')
               ),
               title = 'Population Pyramid for Kelowna - Census 2016')
    )

    # censusStir %<>%
    #   top_n(25, percent_more_than_30) %<>%
    #   mutate(
    #     # Region = ifelse(c_view %in% c("csd", "ct", "da"), paste(`Region Name`, str_sub(GeoUID, -2)), `Region Name`),
    #     Region = factor(
    #       paste(`Region Name`, str_sub(GeoUID, -2)),
    #       levels = unique(Region)[order(percent_more_than_30, decreasing = FALSE)]
    #     )
    #   )

    palStir <- colorNumeric(
      palette = "YlGnBu",
      domain = censusStir$percent_more_than_30)

    # STIR Map
    output$mapCensusStir <- renderLeaflet({
      st_as_sf(censusStir) %>%
      # censusStir %>%
        leaflet() %>%
        addProviderTiles(provider = "CartoDB.Positron") %>%
        addPolygons(
          label = ~ `Region`,
          color = ~ pal(percent_more_than_30),
          stroke = TRUE,
          weight = 1,
          fillOpacity = 0.5,
          smoothFactor = 1,
          # color = '#333',
          # fillColor = ~ palViridis(censusStir$percent_more_than_30),
          # fillColor = ~ pal(censusStir$percent_more_than_30),
          popup = paste0(
            "<strong>",
            paste(censusStir$`Region Name`),
            "</strong>",
            "<table class=\"leaflet-popup-table\">
            <tr><td>Census Year</td><td>", c_year, "</td></tr>",
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
          title = "Percentage of families with STIR > 30%",
          opacity = 0.5
        )
    })

    # STIR Lollipop
    censusStirgg2 <- ggplot(censusStir, aes(x = Region, y = percent_more_than_30)) +
      geom_segment(aes(x = Region, xend = Region, y = 0, yend = percent_more_than_30), size = 1.5, color = colCommercial) +
      geom_point(color = colCommercial, size = 4, alpha = 0.8, shape = 21, stroke = 4) +
      theme_light() +
      coord_flip() +
      theme(
        panel.grid.major.y = element_blank(),
        panel.border = element_blank(),
        axis.ticks.y = element_blank()
      )
    output$stirLollipop <- renderPlotly(
      ggplotly(censusStirgg2) %>%
        layout(
          xaxis = list(title = "")
        )
    )

    # STIR Stacked Bar
    topLabels <- c('More than 30%', 'Less than 30%')
    labelPositions <- c(
      censusStir[1,"percent_more_than_30"]$percent_more_than_30 / 2,
      censusStir[1,"percent_more_than_30"]$percent_more_than_30 +
        censusStir[1,"percent_less_than_30"]$percent_less_than_30 / 2
    )
    output$stirStacked <- renderPlotly(
      plot_ly(censusStir, x = ~percent_more_than_30, name = "More than 30%", y = ~`Region`, type = 'bar', orientation = 'h',
              marker = list(color = colCommercial,
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
               margin = list(l = 70, r = 10, t = 70, b = 30),
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
}
