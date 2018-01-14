# This observer is responsible for PTT related data and charts.
observe({
  pt_view <- input$pt_view
  pt_trans_period <- input$pt_trans_period
  pt_metric <- input$pt_metric

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
