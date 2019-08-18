createAlert(
  session,
  anchorId = "pt_location_alert_fmv",
  alertId = "pt_location_alert_fmv_id",
  style = "alert",
  dismiss = FALSE,
  content = "Click location on the map to draw a time series chart showing monthly total sales values for that location."
)
createAlert(
  session,
  anchorId = "pt_location_alert_fmv_avg",
  alertId = "pt_location_alert_fmv_avg_id",
  dismiss = FALSE,
  content = "Click location on the map to draw a time series chart showing monthly average sales values for that location."
)
createAlert(
  session,
  anchorId = "pt_location_alert_tax",
  alertId = "pt_location_alert_tax_id",
  dismiss = FALSE,
  content = "Click location on the map to draw a time series chart showing monthly property transfer tax paid for that location."
)
createAlert(
  session,
  anchorId = "pt_location_alert_types",
  alertId = "pt_location_alert_types_id",
  dismiss = FALSE,
  content = "Click location on the map to draw a time series chart showing monthly numbers of transactions for different property types for that location."
)
createAlert(
  session,
  anchorId = "pt_location_alert_res",
  alertId = "pt_location_alert_res_id",
  dismiss = FALSE,
  content = "Click location on the map to draw a time series chart showing monthly number of transactions for residential properties for that location."
)
createAlert(
  session,
  anchorId = "pt_location_alert_comm",
  alertId = "pt_location_alert_comm_id",
  dismiss = FALSE,
  content = "Click location on the map to draw a time series chart showing monthly number of transactions for commerction properties for that location."
)

ptData <- reactive({
  ptData <- switch(
    input$pt_view,
    "regdis" = ptt_rd_sf,
    "devreg" = ptt_dr_sf,
    "mun" = ptt_mun_sf
  )

  ptData <- ptData %>% arrange(trans_period)

  return(ptData)
})

ptDataPeriod <- reactive({
  ptDataPeriod <- ptData() %>%
    filter(trans_period == input$pt_trans_period)
  return(ptDataPeriod)
})

ptt_data_location <- reactive({
  ptt_data_location <- ptData() %>%
    filter(GeoUID == input$pt_location)

  return(ptt_data_location)
})

ptRegionOptions <- reactive({
  ptRegionOptions <- ptDataPeriod() %>%
    mutate(label = ptDataPeriod()$GeoName) %>%
    select(label, value = GeoUID)
  st_geometry(ptRegionOptions) <- NULL

  return(ptRegionOptions %>% distinct())
})

ptGeoNameLabel <- reactive({
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
        label = ptDataPeriod()$GeoName,
        popup = paste0(
          "<strong>",
          ptDataPeriod()$GeoName,
          "</strong>",
          "<table class=\"leaflet-popup-table\">
          <tr><td>Period</td><td>",
          as.Date(ptDataPeriod()$trans_period),
          "</td></tr><tr><td>Number of transactions</td><td>",
          format(ptDataPeriod()$tot_mkt_trans, big.mark = ",", scientific=FALSE),
          "</td></tr><tr><td>Number of foreign transactions</td><td>",
          format(ptDataPeriod()$n_foreign_tran, big.mark = ",", scientific=FALSE),
          "</td></tr><tr><td>Number % by foreign purchasers</td><td>",
          format(ptDataPeriod()$no_foreign_perc, big.mark = ",", scientific=FALSE),
          "</td></tr><tr><td>Total value</td><td>",
          paste0("$", format(ptDataPeriod()$sum_FMV, big.mark = ",", scientific=FALSE)),
          "</td></tr><tr><td>Total value by foreign purchasers</td><td>",
          paste0("$", format(ptDataPeriod()$sum_FMV_foreign, big.mark = ",", scientific=FALSE)),
          "</td></tr><tr><td>Value % by foreign purchasers</td><td>",
          format(ptDataPeriod()$sum_FMV_foreign_perc, big.mark = ",", scientific=FALSE),
          "</td></tr><tr><td>PTT Paid</td><td>",
          paste0("$", format(ptDataPeriod()$sum_PPT_paid, big.mark = ",", scientific=FALSE)),
          "</td></tr><tr><td>Additional Tax Paid</td><td>",
          paste0("$", format(ptDataPeriod()$add_tax_paid, big.mark = ",", scientific=FALSE)),
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
})

# Monthly Overview - FMV (Fair Market Value)
output$pt_mothly_fmv <- renderPlotly({
  plot_ly(
    ptt_data_location(),
    x = ~ trans_period,
    y = ~ sum_FMV,
    name = "Total FMV",
    type = 'scatter',
    mode = 'lines',
    line = list(color = colCanadian)
  ) %>%
    add_lines(
      y = ~ sum_FMV_foreign,
      name = "Total FMV Foreign",
      line = list(color = colForeign)
    ) %>%
    add_lines(
      y = ~ no_foreign_perc,
      name = "Foreign %",
      yaxis = "y2",
      line = list(
        color = colForeign,
        dash = 'dot'
      )
    ) %>%
    layout(
      title = PlotlyChartTitle(title_text = paste("FMV (Fair Market Value) in", ptGeoNameLabel())),
      xaxis = axisFormat,
      yaxis = axisFormat,
      yaxis2 = list(
        tickfont = tickfontRd,
        overlaying = "y",
        side = "right",
        title = "Foreign %"
      ),
      margin = marginFormat,
      legend = legendFormat#,
      # annotations = PlotlyChartAnnotation(annotation_text = 'Total fair market value by month.')
    ) %>%
    config(displayModeBar = F)
})

# Monthly Overview - Average and Median FMV
output$pt_mothly_mnd_fmv <- renderPlotly({
  plot_ly(
    ptt_data_location(),
    x = ~ trans_period,
    y = ~ md_FMV,
    name = "Median FMV",
    type = 'scatter',
    mode = 'lines',
    line = list(color = colCanadian)
  ) %>%
    add_lines(
      y = ~ md_FMV_foreign_res,
      name = "Median FMV Foreign",
      line = list(color = colForeign)
    ) %>%
    add_lines(
      y = ~ mn_FMV,
      name = "Mean FMV",
      line = list(
        color = colCanadian,
        dash = 'dot'
      )
    ) %>%
    add_lines(
      y = ~ mn_FMV_foreign_res,
      name = "Mean FMV Foreign",
      line = list(
        color = colForeign,
        dash = 'dot'
      )
    ) %>%
    layout(
      title = PlotlyChartTitle(title_text = paste("Average FMV in", ptGeoNameLabel())),
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
    ptt_data_location(),
    x = ~ trans_period,
    y = ~ sum_PPT_paid,
    name = "PTT",
    type = 'scatter',
    mode = 'lines',
    line = list(color = colCanadian)
  ) %>%
    add_lines(
      y = ~ add_tax_paid,
      name = "Additional PTT",
      line = list(color = colForeign)
    ) %>%
    layout(
      title = PlotlyChartTitle(title_text = paste("Property Transfer Tax Paid in", ptGeoNameLabel())),
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
    ptt_data_location(),
    x = ~ trans_period,
    y = ~ no_res_trans,
    name = "Residential",
    type = "bar",
    marker = list(color = colResidential),
    hoverinfo = "y+name"
  ) %>%
    add_trace(
      y = ~ n_comm_tran,
      name = "Commercial",
      marker = list(color = colCommercial)
    ) %>%
    add_trace(
      y = ~ n_recr_tran,
      name = "Recreational",
      marker = list(color = colRecreational)
    ) %>%
    add_trace(
      y = ~ n_farm_tran,
      name = "Farms",
      marker = list(color = colFarms)
    ) %>%
    add_trace(
      y = ~ n_unkn_tran,
      name = "Unknown",
      marker = list(color = colUnknown)
    ) %>%
    layout(
      title = PlotlyChartTitle(title_text = paste("Number of Transactions in", ptGeoNameLabel())),
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
    ptt_data_location(),
    x = ~ trans_period,
    y = ~ n_res_1fam_dwelling,
    name = "Single Family",
    type = "bar",
    marker = list(color = colSingleFam),
    hoverinfo = "y+name"
  ) %>%
    add_trace(
      y = ~ n_res_fam,
      name = "Multi Family",
      marker = list(color = colMultiFam)
    ) %>%
    add_trace(
      y = ~  (n_res_strata),
      name = "Strata",
      marker = list(color = colStrata)
    ) %>%
    add_trace(
      y = ~ n_res_acreage_trans,
      name = "Acreage",
      marker = list(color = colAcreage)
    ) %>%
    add_trace(
      y = ~ n_res_other,
      name = "Other",
      marker = list(color = colUnknown)
    ) %>%
    layout(
      title = PlotlyChartTitle(title_text = paste("Number of Transactions (Residential) in", ptGeoNameLabel())),
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
    ptt_data_location(),
    x = ~ trans_period,
    y = ~ n_comm_comm,
    name = "Commerce",
    type = "bar",
    marker = list(color = colCommercial),
    hoverinfo = "y+name"
  ) %>%
    add_trace(
      y = ~ n_comm_strata_nores,
      name = "Strata non-residential",
      marker = list(color = colStrata)
    ) %>%
    add_trace(
      y = ~ n_comm_other,
      name = "Other",
      marker = list(color = colUnknown)
    ) %>%
    layout(
      title = PlotlyChartTitle(title_text = paste("Number of transactions (Commercial) in", ptGeoNameLabel())),
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
      mutate(label = GeoName) %>%
      select(label) %>%
      distinct()
    updateTextInput(session, "pt_location_name", value = locationLabel$label)
  }
})
