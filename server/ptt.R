createAlert(session, "pt_location_alert_fmv", alertId = "pt_location_alert_fmv_id", title = NULL, style = "alert", dismiss = FALSE, append = TRUE,
            content = "Click location on the map to draw a time series chart showing monthly total sales values for that location.")
createAlert(session, "pt_location_alert_fmv_avg", alertId = "pt_location_alert_fmv_avg_id", title = NULL, style = NULL, dismiss = FALSE, append = TRUE,
            content = "Click location on the map to draw a time series chart showing monthly average sales values for that location.")
createAlert(session, "pt_location_alert_tax", alertId = "pt_location_alert_tax_id", title = NULL, style = NULL, dismiss = FALSE, append = TRUE,
            content = "Click location on the map to draw a time series chart showing monthly property transfer tax paid for that location.")
createAlert(session, "pt_location_alert_types", alertId = "pt_location_alert_types_id", title = NULL, style = NULL, dismiss = FALSE, append = TRUE,
            content = "Click location on the map to draw a time series chart showing monthly numbers of transactions for
            different property types for that location.")
createAlert(session, "pt_location_alert_res", alertId = "pt_location_alert_res_id", title = NULL, style = NULL, dismiss = FALSE, append = TRUE,
            content = "Click location on the map to draw a time series chart showing monthly number of transactions
            for residential properties for that location.")
createAlert(session, "pt_location_alert_comm", alertId = "pt_location_alert_comm_id", title = NULL, style = NULL, dismiss = FALSE, append = TRUE,
            content = "Click location on the map to draw a time series chart showing monthly number of transactions
            for commerction properties for that location.")

ptData <- reactive({
  ptData <- switch(
    input$pt_view,
    "regdis" = ptRegDis,
    "devreg" = ptDevReg,
    "mun" = ptMun
  )
  return(ptData)
})

ptDataPeriod <- reactive({
  ptDataPeriod <- ptData() %>%
    filter(trans_period == input$pt_trans_period)
  return(ptDataPeriod)
})

ptRegionOptions <- reactive({
  ptRegionOptions <- ptDataPeriod() %>%
    mutate(label = ptDataPeriod()$Location) %>%
    select(label, value = GeoUID)
  st_geometry(ptRegionOptions) <- NULL

  return(ptRegionOptions %>% distinct())
})

ptLocationLabel <- reactive({
  locationLabel <- as.data.frame(ptRegionOptions()) %>%
    filter(value == input$pt_location) %>%
    select(label) %>%
    distinct()
  return(locationLabel)
})

# This observer is responsible for PTT related data and charts.
observe({
  pt_view <- input$pt_view
  pt_trans_period <- input$pt_trans_period
  pt_metric <- input$pt_metric

  # Reset location
  updateTextInput(session, "pt_location", value = "")
  updateTextInput(session, "pt_location_name", value = "")

  pal <- colorNumeric("viridis", ptDataPeriod()[[pt_metric]])

  output$mapPtt <- renderLeaflet({
    leaflet() %>%
      setView(lng = -123.12, lat = 52.78, zoom = 6) %>%
      addProviderTiles(provider = "CartoDB.Positron", options = providerTileOptions(minZoom = 5, maxZoom = 12)) %>%
      addPolygons(
        data = ptDataPeriod(),
        stroke = TRUE,
        weight = 1,
        fillOpacity = 0.5,
        smoothFactor = 0.2,
        color = '#333',
        layerId = ptDataPeriod()$GeoUID,
        fillColor = ~ pal(ptDataPeriod()[[pt_metric]]),
        label = ptDataPeriod()$Location,
        popup = paste0(
          "<strong>",
          ptDataPeriod()$Location,
          "</strong>",
          "<table class=\"leaflet-popup-table\">
          <tr><td>Period</td><td>",
          as.Date(ptDataPeriod()$trans_period),
          "</td></tr><tr><td>Number of transactions</td><td>",
          format(ptDataPeriod()$no_mkt_trans, big.mark = ",", scientific=FALSE),
          "</td></tr><tr><td>Number of foreign transactions</td><td>",
          format(ptDataPeriod()$no_foreign, big.mark = ",", scientific=FALSE),
          "</td></tr><tr><td>Number % by foreign purchasers</td><td>",
          format(ptDataPeriod()$no_foreign_perc, big.mark = ",", scientific=FALSE),
          "</td></tr><tr><td>Total value</td><td>",
          paste0("$", format(ptDataPeriod()$sum_FMV, big.mark = ",", scientific=FALSE)),
          "</td></tr><tr><td>Total value by foreign purchasers</td><td>",
          paste0("$", format(ptDataPeriod()$sum_FMV_foreign, big.mark = ",", scientific=FALSE)),
          "</td></tr><tr><td>Value % by foreign purchasers</td><td>",
          format(ptDataPeriod()$sum_FMV_foreign_perc, big.mark = ",", scientific=FALSE),
          "</td></tr><tr><td>PTT Paid</td><td>",
          paste0("$",
                format(ptDataPeriod()$sum_PPT_paid, big.mark = ",", scientific=FALSE)),
          "</td></tr><tr><td>Additional Tax Paid</td><td>",
          paste0("$",
                format(ptDataPeriod()$add_tax_paid, big.mark = ",", scientific=FALSE)),
          "</td></tr></table>"
        ),
        highlight = highlightOptions(
          weight = 5,
          color = "#696969",
          fillOpacity = 0.5,
          bringToFront = TRUE)
      ) %>%
      addLegend(
        "bottomleft",
        pal = pal,
        values = ptDataPeriod()[[pt_metric]],
        title = selectionMetricsDF %>% filter(value == rlang::sym(pt_metric)) %>% pull(label),
        opacity = 0.8
      )
  })


  # Interactive based on user input
  output$interactive <- renderPlotly({
    plot_ly(
      ptDataPeriod(),
      x = ~ ptDataPeriod()[[pt_metric]],
      y = ~ Location,
      type = "bar",
      orientation = "h"
    ) %>%
      layout(
        title = selectionMetricsDF %>% filter(value == rlang::sym(pt_metric)) %>% pull(label),
        xaxis = axisFormat,
        yaxis = axisFormat,
        margin = marginFormatMonthly,
        barmode = 'group',
        legend = legendFormat
      ) %>%
      config(displayModeBar = F)
  })

})

# Monthly Overview - FMV (Fair Market Value)
output$pt_mothly_fmv <- renderPlotly({
  plot_ly(
    ptData() %>% filter(GeoUID == input$pt_location),
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
      title = paste("FMV (Fair Market Value) in", ptLocationLabel()),
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
    ptData() %>% filter(GeoUID == input$pt_location),
    x = ~ trans_period,
    y = ~ md_FMV,
    name = "Median FMV",
    type = 'scatter',
    mode = 'lines',
    line = list(shape = "spline", color = colCanadian)
  ) %>%
    add_lines(
      y = ~ md_FMV_foreign,
      name = "Median FMV Foreign",
      line = list(shape = "spline", color = colForeign)
    ) %>%
    layout(
      title = paste("Average FMV in", ptLocationLabel()),
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
    ptData() %>% filter(GeoUID == input$pt_location),
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
      title = paste("Property Transfer Tax Paid in", ptLocationLabel()),
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
    ptData() %>% filter(GeoUID == input$pt_location),
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
      title = paste("Number of Transactions in", ptLocationLabel()),
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
    ptData() %>% filter(GeoUID == input$pt_location),
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
      title = paste("Number of Transactions (Residential) in", ptLocationLabel()),
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
    ptData() %>% filter(GeoUID == input$pt_location),
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
      title = paste("Number of transactions (Commercial) in", ptLocationLabel()),
      xaxis = axisFormat,
      yaxis = axisFormat,
      margin = marginFormat,
      barmode = 'stack',
      legend = legendFormat
    ) %>%
    config(displayModeBar = F)
})

# Map region click observer
observeEvent(input$mapPtt_shape_click, {
  m <- input$mapPtt_shape_click
  if(!is.null(m$id)){

    updateTextInput(session, "pt_location", value = m$id)

    locationLabel <- as.data.frame(ptDataPeriod()) %>%
      filter(GeoUID == m$id) %>%
      mutate(label = Location) %>%
      select(label) %>%
      distinct()
    updateTextInput(session, "pt_location_name", value = locationLabel$label)
  }
})
