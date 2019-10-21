#' @title Shiny server function
#'
#' @param input Input variables
#' @param output Output variables
#' @param session Session data
#'
#' @import shiny
#' @importFrom sf st_as_sf
#' @importFrom sf st_geometry
#' @import leaflet
#' @import stringr
#' @import magrittr
#' @importFrom dplyr arrange
#' @importFrom dplyr distinct
#' @importFrom dplyr everything
#' @importFrom dplyr filter
#' @importFrom dplyr if_else
#' @importFrom dplyr mutate
#' @importFrom dplyr pull
#' @importFrom dplyr select
#' @importFrom dplyr top_n
#' @importFrom dplyr ungroup
#' @importFrom tidyr gather
#' @importFrom scales dollar_format
#' @import htmlwidgets
#' @import plotly
#' @importFrom sunburstR sunburst
#' @importFrom sunburstR renderSunburst
#' @importFrom treemap treemap
#'
#' @export
app_server <- function(input, output, session) {
  data("ptt_prov_dash", package = "bchousing")
  data("ptt_dr_sf", package = "bchousing")
  data("ptt_rd_sf", package = "bchousing")
  data("ptt_mun_sf", package = "bchousing")

  # Chart formatting
  fontFamily <- "Myriad-Pro, Calibri, Arial, 'sans serif'"
  tickfontBl <- list(family = fontFamily,
                     size = 12,
                     color = "black")

  tickfontRd = list(family = fontFamily,
                    size = 12,
                    color = "#C40C0C")

  axisFormat <- list(title = "",
                     showticklabels = TRUE,
                     # tickangle = 45,
                     tickfont = tickfontBl)

  legendFormat <- list(
    font = list(
      family = fontFamily,
      size = 11,
      color = "#393939"
    ),
    bordercolor = "#e6e6e6",
    borderwidth = 1,
    bgcolor = "rgba(255, 255, 255, 0.5)",
    orientation = 'h',
    # xanchor = "center",  # use center of legend as anchor
    x = 0
  )

  marginFormat <- list(
    l = 50,
    r = 50,
    b = 100,
    t = 50,
    pad = 4
  )

  marginFormatMonthly <- list(
    l = 150,
    r = 50,
    b = 50,
    t = 50,
    pad = 4
  )

  # color schema
  colResidential <- "#80b1d3"
  colSingleFam <- "#4292c6"
  colMultiFam <- "#9ecae1"
  colStrata <- "#abdda4"
  colNonStrataRental <- "#c7eae5"
  # colCommercial <- "#fdae61"
  colCommercial <- "#E39398"
  colRecreational <- "#80cdc1"
  colFarms <- "#fee08b"
  colUnknown <- "#d9d9d9"
  colAcreage <- "#dfc27d"
  # colC16 <- colCanadian <- "#c40c0c"
  # colC11 <- colForeign <- "#3eb4f0"
  colC16 <- colCanadian <- "#E39398"
  colC11 <- colForeign <- "#9ecae1"

  FormatCurrency <- scales::dollar_format()
  FormatCount <- scales::number_format(big.mark = ',', accuracy = 1)
  FormatFloat <- scales::number_format(big.mark = ',', accuracy = 0.01)
  FormatPercent <- scales::percent_format(scale = 1, accuracy = 0.01)

  # Selection of metrics, variables and options
  selectionMetricsDF <- data.frame(
    value =
      c("no_mkt_trans", "sum_FMV", "sum_PPT_paid", "no_foreign", "no_foreign_perc",
        "sum_FMV_foreign", "add_tax_paid", "mn_FMV", "mn_FMV_foreign"),
    label =
      c("Number of transactions", "Total FMV", "Property transfer tax paid",
        "Number of foreign transactions", "% of foreign transactions", "Total FMV of foreign transactions",
        "Additional tax paid", "Mean FMV", "Mean foreign FMV")
  )

  # Reactives ----
  # 01. PTT ----
  ptData <- reactive({
    ptData <- switch(
      input$pt_view,
      "regdis" = ptt_rd_sf,
      "devreg" = ptt_dr_sf,
      "mun" = ptt_mun_sf
    )

    ptData <- ptData %>% dplyr::arrange(trans_period)

    return(ptData)
  })

  ptDataPeriod <- reactive({
    ptDataPeriod <- ptData() %>%
      dplyr::filter(trans_period == input$pt_trans_period)
    return(ptDataPeriod)
  })

  ptt_data_location <- reactive({
    ptt_data_location <- ptData() %>%
      dplyr::filter(GeoUID == input$pt_location)

    return(ptt_data_location)
  })

  ptt_data_location_period <- reactive({
    ptDataPeriod <- ptData() %>%
      dplyr::filter(
        trans_period == input$pt_trans_period & GeoUID == input$pt_location
      ) %>%
      dplyr::select(
        trans_period_label, GeoName, sum_FMV, sum_FMV_foreign, sum_FMV_res, sum_FMV_foreign_res, mn_FMV, md_FMV,
        sum_PPT_paid, md_PPT, add_tax_paid, tot_mkt_trans, no_res_trans, n_comm_tran,
        n_res_1fam_dwelling, n_res_strata
      ) %>%
      dplyr::mutate(
        trans_period_label = as.character(trans_period_label),
        fmv_perc_res = dplyr::if_else(is.na(sum_FMV_res), 'NA', paste0(round(sum_FMV_res / sum_FMV * 100, 2), '%')),
        fmv_perc_foreign = dplyr::if_else(is.na(sum_FMV_foreign), 'NA', paste0(round(sum_FMV_foreign / sum_FMV * 100, 2), '%')),
        fmv_perc_foreign_res = dplyr::if_else(is.na(sum_FMV_foreign_res), 'NA', paste0(round(sum_FMV_foreign_res / sum_FMV_res * 100, 2), '%')),
        mn_FMV = scales::dollar_format()(mn_FMV),
        md_FMV = scales::dollar_format()(md_FMV),
        sum_PPT_paid = scales::dollar_format()(sum_PPT_paid),
        add_tax_paid = scales::dollar_format()(add_tax_paid),
        md_PPT = scales::dollar_format()(md_PPT),
        no_other_tran = tot_mkt_trans - no_res_trans - n_comm_tran,
        no_other_res = no_res_trans - n_res_1fam_dwelling - n_res_strata
      )
  })

  ptRegionOptions <- reactive({
    ptRegionOptions <- ptDataPeriod() %>%
      dplyr::mutate(label = ptDataPeriod()$GeoName) %>%
      dplyr::select(label, value = GeoUID)
    sf::st_geometry(ptRegionOptions) <- NULL

    return(ptRegionOptions %>% distinct())
  })

  ptGeoNameLabel <- reactive({
    locationLabel <- as.data.frame(ptRegionOptions()) %>%
      dplyr::filter(value == input$pt_location) %>%
      dplyr::select(label) %>%
      dplyr::distinct()
    return(locationLabel)
  })

  # PTT observer (view, period, variable) ----
  observe({
    pt_view <- input$pt_view
    pt_trans_period <- input$pt_trans_period
    pt_metric <- input$pt_metric

    # Reset location
    updateTextInput(session, "pt_location", value = "")
    updateTextInput(session, "pt_location_name", value = "")

    pal <- colorNumeric("GnBu", ptDataPeriod()[[pt_metric]])

    # 01.1 - PTT Leaflet ----
    output$mapPtt <- leaflet::renderLeaflet({
      leaflet::leaflet() %>%
        setView(lng = -125.2, lat = 54.7, zoom = 5) %>%
        addProviderTiles(provider = "CartoDB.Positron", options = providerTileOptions(minZoom = 5, maxZoom = 12)) %>%
        addPolygons(
          data = ptDataPeriod(),
          stroke = TRUE,
          weight = 1,
          fillOpacity = 0.65,
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
          <tr><td>Period</td><td class=\"text-right\">",
            ptDataPeriod()$trans_period_label,
            "</td></tr><tr><td>Number of transactions</td><td class=\"text-right\">",
            FormatCount(ptDataPeriod()$tot_mkt_trans),
            "</td></tr><tr><td>Number of foreign transactions</td><td class=\"text-right\">",
            FormatCount(ptDataPeriod()$n_foreign_tran),
            "</td></tr><tr><td>Number % by foreign purchasers</td><td class=\"text-right\">",
            FormatPercent(ptDataPeriod()$no_foreign_perc),
            "</td></tr><tr><td>Total value</td><td class=\"text-right\">",
            FormatCurrency(ptDataPeriod()$sum_FMV),
            "</td></tr><tr><td>Total value by foreign purchasers</td><td class=\"text-right\">",
            FormatCurrency(ptDataPeriod()$sum_FMV_foreign),
            "</td></tr><tr><td>Value % by foreign purchasers</td><td class=\"text-right\">",
            FormatPercent(ptDataPeriod()$sum_FMV_foreign_perc),
            "</td></tr><tr><td>Property transfer tax paid</td><td class=\"text-right\">",
            FormatCurrency(ptDataPeriod()$sum_PPT_paid),
            "</td></tr><tr><td>Additional tax paid</td><td class=\"text-right\">",
            FormatCurrency(ptDataPeriod()$add_tax_paid),
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
          title = selectionMetricsDF %>%
            dplyr::filter(value == rlang::sym(pt_metric)) %>%
            dplyr::pull(label),
          opacity = 0.8
        )
    })
  })

  # 01.2 - PTT dashboard widgets values ----
  output$fmv_perc_month <- renderText({
    # browser()
    label <- ptt_data_location_period() %>% dplyr::pull(trans_period_label)
    return(label)
  })
  output$fmv_perc_loc <- renderText({
    ptt_data_location_period() %>% dplyr::pull(GeoName)
  })
  output$fmv_perc_month_mn <- renderText({
    ptt_data_location_period() %>% dplyr::pull(trans_period_label)
  })
  output$fmv_perc_loc_mn <- renderText({
    ptt_data_location_period() %>% dplyr::pull(GeoName)
  })
  output$fmv_perc_month_ptt <- renderText({
    ptt_data_location_period() %>% dplyr::pull(trans_period_label)
  })
  output$fmv_perc_loc_ptt <- renderText({
    ptt_data_location_period() %>% dplyr::pull(GeoName)
  })
  output$fmv_perc_month_n <- renderText({
    ptt_data_location_period() %>% dplyr::pull(trans_period_label)
  })
  output$fmv_perc_loc_n <- renderText({
    ptt_data_location_period() %>% dplyr::pull(GeoName)
  })
  output$fmv_perc_month_res <- renderText({
    ptt_data_location_period() %>% dplyr::pull(trans_period_label)
  })
  output$fmv_perc_loc_res <- renderText({
    ptt_data_location_period() %>% dplyr::pull(GeoName)
  })
  output$fmv_perc_res <- renderText({
    ptt_data_location_period() %>% dplyr::pull(fmv_perc_res)
  })
  output$fmv_perc_foreign <- renderText({
    ptt_data_location_period() %>% dplyr::pull(fmv_perc_foreign)
  })
  output$fmv_perc_foreign_res <- renderText({
    ptt_data_location_period() %>% dplyr::pull(fmv_perc_foreign_res)
  })
  output$mn_fmv <- renderText({
    ptt_data_location_period() %>% dplyr::pull(mn_FMV)
  })
  output$md_fmv <- renderText({
    ptt_data_location_period() %>% dplyr::pull(md_FMV)
  })
  output$sum_ptt_paid <- renderText({
    ptt_data_location_period() %>% dplyr::pull(sum_PPT_paid)
  })
  output$add_ptt_paid <- renderText({
    ptt_data_location_period() %>% dplyr::pull(add_tax_paid)
  })
  output$md_ppt_paid <- renderText({
    ptt_data_location_period() %>% dplyr::pull(md_PPT)
  })


  output$tot_mkt_trans <- renderText({
    ptt_data_location_period() %>% dplyr::pull(tot_mkt_trans)
  })
  output$no_res_trans <- renderText({
    ptt_data_location_period() %>% dplyr::pull(no_res_trans)
  })
  output$n_comm_tran <- renderText({
    ptt_data_location_period() %>% dplyr::pull(n_comm_tran)
  })
  output$n_res_1fam_dwelling <- renderText({
    ptt_data_location_period() %>% dplyr::pull(n_res_1fam_dwelling)
  })
  output$n_res_strata <- renderText({
    ptt_data_location_period() %>% dplyr::pull(n_res_strata)
  })
  output$no_other_res <- renderText({
    ptt_data_location_period() %>% dplyr::pull(no_other_res)
  })

  # 01.3 - FMV ----
  output$pt_mothly_fmv <- plotly::renderPlotly({
    plotly::plot_ly(
      ptt_data_location(),
      x = ~ trans_period,
      y = ~ sum_FMV,
      name = "Total FMV",
      type = 'scatter',
      mode = 'markers+lines',
      line = list(color = colCanadian),
      marker = list(
        opacity = 0.75,
        size = 5,
        color = colCanadian
      ), fill = 'tozeroy', fillcolor = 'rgba(227, 147, 152, 0.5)'
    ) %>%
      add_trace(
        y = ~ sum_FMV_foreign,
        name = "Total FMV foreign",
        type = 'scatter',
        mode = 'markers+lines',
        line = list(color = colForeign),
        marker = list(
          opacity = 0.75,
          size = 5,
          color = colForeign
        ), fill = 'tozeroy', fillcolor = 'rgba(158, 202, 225, 0.75)'
    ) %>%
      add_trace(
        y = ~ sum_FMV_res,
        name = "Residential FMV",
        type = 'scatter',
        mode = 'markers+lines',
        line = list(color = colRecreational),
        marker = list(
          opacity = 0.75,
          size = 5,
          color = colRecreational
        ), fill = 'tozeroy', fillcolor = 'rgba(158, 202, 225, 0.25)'
    ) %>%
      add_trace(
        y = ~ sum_FMV_foreign_res,
        name = "Residential FMV foreign",
        type = 'scatter',
        mode = 'markers+lines',
        line = list(color = colFarms),
        marker = list(
          opacity = 0.75,
          size = 5,
          color = colFarms
        ), fill = 'tozeroy', fillcolor = 'rgba(158, 202, 225, 0.25)'
      # ) %>%
      # add_trace(
      #   y = ~ no_foreign_perc,
      #   name = "Foreign %",
      #   yaxis = "y2",
      #   type = 'scatter',
      #   mode = 'markers+lines',
      #   line = list(
      #     color = colForeign,
      #     dash = 'dot'
      #   ),
      #   marker = list(
      #     opacity = 0.75,
      #     size = 5,
      #     color = colForeign
      #   ), fill = 'tozeroy'
      ) %>%
      plotly::layout(
        title = PlotlyChartTitle(title_text = paste("FMV (fair market value) in", ptGeoNameLabel())),
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
      config(
        displaylogo = FALSE,
        modeBarButtonsToRemove = c("zoomIn3d", "zoomOut3d", "resetScale2d", "toggleSpikelines")
      )
  })

  # 01.4 - Mean FMV ----
  output$pt_mothly_mnd_fmv <- plotly::renderPlotly({
    plotly::plot_ly(
      ptt_data_location(),
      x = ~ trans_period,
      y = ~ md_FMV,
      name = "Median FMV",
      type = 'scatter',
      mode = 'lines+markers',
      marker = list(
        opacity = 0.75,
        size = 5,
        color = colForeign
      ),
      line = list(color = colCanadian)
    ) %>%
      add_trace(
        y = ~ md_FMV_foreign_res,
        name = "Median FMV foreign",
        line = list(color = colForeign),
        type = 'scatter',
        mode = 'markers+lines',
        marker = list(
          opacity = 0.75,
          size = 5,
          color = colForeign
        )
      ) %>%
      add_trace(
        y = ~ mn_FMV,
        name = "Mean FMV",
        line = list(
          color = colCanadian,
          dash = 'dot'
        ),
        type = 'scatter',
        mode = 'markers+lines',
        marker = list(
          opacity = 0.75,
          size = 5,
          color = colCanadian
        )
      ) %>%
      add_trace(
        y = ~ mn_FMV_foreign_res,
        name = "Mean FMV foreign",
        line = list(
          color = colForeign,
          dash = 'dot'
        ),
        type = 'scatter',
        mode = 'markers+lines',
        marker = list(
          opacity = 0.75,
          size = 5,
          color = colForeign
        )
      ) %>%
      plotly::layout(
        title = PlotlyChartTitle(title_text = paste("Mean FMV in", ptGeoNameLabel())),
        xaxis = axisFormat,
        yaxis = axisFormat,
        margin = marginFormat,
        legend = legendFormat
      ) %>%
      config(
        displaylogo = FALSE,
        modeBarButtonsToRemove = c("zoomIn3d", "zoomOut3d", "resetScale2d", "toggleSpikelines")
      )
  })

  # 01.5 - PTT paid ----
  output$pt_mothly_ptt <- plotly::renderPlotly({
    plotly::plot_ly(
      ptt_data_location(),
      x = ~ trans_period,
      y = ~ sum_PPT_paid,
      name = "PTT",
      type = 'scatter',
      mode = 'lines+markers',
      marker = list(
        opacity = 0.75,
        size = 5,
        color = colCanadian
      ),
      line = list(color = colCanadian)
    ) %>%
      add_trace(
        y = ~ add_tax_paid,
        name = "Additional PTT",
        line = list(color = colForeign),
        type = 'scatter',
        mode = 'markers+lines',
        marker = list(
          opacity = 0.75,
          size = 5,
          color = colForeign
        )
      ) %>%
      plotly::layout(
        title = PlotlyChartTitle(title_text = paste("Property transfer tax paid in", ptGeoNameLabel())),
        xaxis = axisFormat,
        yaxis = axisFormat,
        margin = marginFormat,
        legend = legendFormat
      ) %>%
      config(
        displaylogo = FALSE,
        modeBarButtonsToRemove = c("zoomIn3d", "zoomOut3d", "resetScale2d", "toggleSpikelines")
      )
  })

  # 01.6 - Number of market transactions ----
  output$pt_mothly <- plotly::renderPlotly({
    plotly::plot_ly(
      ptt_data_location(),
      x = ~ trans_period,
      y = ~ no_res_trans,
      name = "Residential",
      type = "bar",
      marker = list(color = colMultiFam),
      hoverinfo = "y+name",
      opacity = 0.75
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
      plotly::layout(
        title = PlotlyChartTitle(title_text = paste("Number of transactions in", ptGeoNameLabel())),
        xaxis = axisFormat,
        yaxis = axisFormat,
        margin = marginFormat,
        barmode = 'stack',
        legend = legendFormat
      ) %>%
      config(
        displaylogo = FALSE,
        modeBarButtonsToRemove = c("zoomIn3d", "zoomOut3d", "resetScale2d", "toggleSpikelines")
      )
  })

  # 01.7 - Residential ----
  output$pt_mothly_res <- plotly::renderPlotly({
    plotly::plot_ly(
      ptt_data_location(),
      x = ~ trans_period,
      y = ~ n_res_1fam_dwelling,
      name = "Single family",
      type = "bar",
      marker = list(color = colSingleFam),
      hoverinfo = "y+name",
      opacity = 0.75
    ) %>%
      add_trace(
        y = ~ n_res_fam,
        name = "Multi family",
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
      plotly::layout(
        title = PlotlyChartTitle(title_text = paste("Number of transactions (residential) in", ptGeoNameLabel())),
        xaxis = axisFormat,
        yaxis = axisFormat,
        margin = marginFormat,
        barmode = 'stack',
        legend = legendFormat
      ) %>%
      config(
        displaylogo = FALSE,
        modeBarButtonsToRemove = c("zoomIn3d", "zoomOut3d", "resetScale2d", "toggleSpikelines")
      )
  })

  # 01.8 - Commercial ----
  output$pt_mothly_comm <- plotly::renderPlotly({
    plotly::plot_ly(
      ptt_data_location(),
      x = ~ trans_period,
      y = ~ n_comm_comm,
      name = "Commerce",
      type = "bar",
      marker = list(color = colCommercial),
      hoverinfo = "y+name",
      opacity = 0.75
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
      plotly::layout(
        title = PlotlyChartTitle(title_text = paste("Number of transactions (commercial) in", ptGeoNameLabel())),
        xaxis = axisFormat,
        yaxis = axisFormat,
        margin = marginFormat,
        barmode = 'stack',
        legend = legendFormat
      ) %>%
      config(
        displaylogo = FALSE,
        modeBarButtonsToRemove = c("zoomIn3d", "zoomOut3d", "resetScale2d", "toggleSpikelines")
      )
  })

  # 01.9 - PTT Map region click observer ----
  observeEvent(input$mapPtt_shape_click, {
    m <- input$mapPtt_shape_click
    if(!is.null(m$id)){

      updateTextInput(session, "pt_location", value = m$id)

      locationLabel <- as.data.frame(ptDataPeriod()) %>%
        dplyr::filter(GeoUID == m$id) %>%
        dplyr::mutate(label = GeoName) %>%
        dplyr::select(label) %>%
        dplyr::distinct()
      updateTextInput(session, "pt_location_name", value = locationLabel$label)
    }
  })

  # 02. Census ----
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
      "CT" = censusMobilityCt#,
      # "DA" = censusMobilityDa
    )

    return(censusMobility)
  })

  censusMobilitySeq <- reactive({
    censusMobilitySeq <- switch(
      input$c_view,
      "CMA" = censusMobilityCmaSeq,
      "CSD" = censusMobilityCsdSeq,
      "CD" = censusMobilityCdSeq,
      "CT" = censusMobilityCtSeq#,
      # "DA" = censusMobilityDa
    )
    return(censusMobilitySeq)
  })

  censusMobilityGathered <- reactive({
    censusMobilityGathered <- switch(
      input$c_view,
      "CMA" = censusMobilityCmaGathered,
      "CSD" = censusMobilityCsdGathered,
      "CD" = censusMobilityCdGathered,
      "CT" = censusMobilityCtGathered#,
      # "DA" = censusMobilityDa
    )
    return(censusMobilityGathered)
  })

  censusStir <- reactive({
    censusStir <- switch(
      input$c_view,
      "CMA" = census2016CmaStir,
      "CSD" = census2016CsdStir,
      "CD" = census2016CdStir,
      "CT" = census2016CtStir#,
      # "DA" = census2016DaStir
    )
    return(censusStir)
  })

  censusAvgAge <- reactive({
    censusAvgAge <- switch(
      input$c_view,
      "CMA" = census2016aaCma,
      "CSD" = census2016aaCsd,
      "CD" = census2016aaCd,
      "CT" = census2016aaCt#,
      # "DA" = census2016aaDa
    )
    return(censusAvgAge)
  })

  censusPp2016 <- reactive({
    censusPp <- switch(
      input$c_view,
      "CMA" = censusPpCma,
      "CSD" = censusPpCsd,
      "CD" = censusPpCd,
      "CT" = censusPpCt#,
      # "DA" = censusPpDa
    )
    return(censusPp)
  })

  #
  # Dropdown options for location based on selected geo-level
  #
  regionOptions <- reactive({
    regionOptions <- censusMobility() %>%
      dplyr::mutate(label = paste0(censusMobility()$`Region`, " (", censusMobility()$GeoUID, ")")) %>%
      dplyr::select(label, value = GeoUID)
    sf::st_geometry(regionOptions) <- NULL

    updateSelectizeInput(session, 'c_location_pp_compare', choices = regionOptions, server = TRUE, selected = "")

    return(regionOptions %>% distinct())
  })

  #
  # Population pyramid 2016 to compare with selected location
  #
  censusPp2016LocationCompare <- reactive({
    locationA <- input$c_location
    locationB <- input$c_location_pp_compare

    censusPp2016CompareA <- censusPp2016() %>%
      dplyr::filter(GeoUID == locationA)

    if (locationB == "") {
      locationB = locationA
    }

    censusPp2016CompareB <- censusPp2016() %>%
      dplyr::filter(GeoUID == locationB) %>%
      dplyr::ungroup() %>%
      dplyr::select(Region_compare = Region, age, sex, percentage_compare = percentage_2016)

    censusPp2016Compare <- inner_join(
      censusPp2016CompareA,
      censusPp2016CompareB,
      by = c("age", "sex")
    )
    return(censusPp2016Compare)
  })

  # Reactive location label
  locationLabel <- reactive({
    locationLabel <- as.data.frame(regionOptions()) %>%
      dplyr::filter(value == input$c_location) %>%
      dplyr::pull(label)
    return(locationLabel)
  })

  # Reactive PP compare location label
  locationCompareLabel <- reactive({
    locationCompareLabel <- as.data.frame(censusMobility()) %>%
      dplyr::filter(GeoUID == input$c_location_pp_compare) %>%
      dplyr::mutate(label = paste0(Region, " (", GeoUID, ")")) %>%
      dplyr::pull(label)
    return(locationCompareLabel)
  })

  # Reactive housing types
  housingTypes <- reactive({
    housingTypes <- switch(
      input$c_view,
      "CMA" = housingTypesCma,
      "CD" = housingTypesCd,
      "CSD" = housingTypesCsd,
      "CT" = housingTypesCt#,
      # "DA" = housingTypesDa
    )
    return(housingTypes)
  })

  # Reactive housing types depending on selected type
  housingTypeMapData <- reactive({
    htMapData <- housingTypes() %>%
      dplyr::select(typewatch = input$c_housing_types, dplyr::everything()) %>%
      dplyr::mutate(typewatch2 = as.numeric(input$c_housing_types))
    return(htMapData)
  })

  # Reactive STIR data
  dataStir <- reactive({
    data <- censusStir()
    if (input$c_location == '') {
      dataStir <- censusStir() %>%
        dplyr::top_n(25, percent_more_than_30)
    } else {
      dataStir <- censusStir() %>%
        dplyr::filter(GeoUID == input$c_location)
    }
    return(dataStir)
  })

  #
  # Mobility palette
  #
  palHousingTypes <- colorNumeric(
    palette = "GnBu",
    domain = housingTypesCma$`Single detached house ratio`,
    na.color = "#e6e6e6"
  )

  #
  # Census location observer ----
  #
  observe({
    # Reset location
    updateTextInput(session, "c_location", value = "")
    updateTextInput(session, "c_location_name", value = "")

    # Defaults and shape the data
    if (!is.null(housingTypes())) {
      housingTypes <- housingTypes()
    }

    #
    # Housing Types map palette
    #
    palHousingTypes <- colorNumeric(
      palette = "GnBu",
      domain = housingTypeMapData()$typewatch,
      na.color = "#e6e6e6"
    )

    #
    # Mobility observer
    #
    censusMobility <- censusMobilityCd
    censusMobilitySeq <- censusMobilityCdSeq
    censusMobilityGathered <- censusMobilityCdGathered
    if (!is.null(censusMobility())) {
      censusMobility <- censusMobility()
    }
    if (!is.null(censusMobilitySeq())) {
      censusMobilitySeq <- censusMobilitySeq()
    }
    if (!is.null(censusMobilityGathered())) {
      censusMobilityGathered <- censusMobilityGathered()
    }

    # Mobility palette
    palMobility <- colorNumeric(
      palette = "GnBu",
      domain = censusMobility$`Movers Ratio`,
      na.color = "#e6e6e6"
    )
    palLightRed <- "#fc95a4"
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

    # AvgAge palette
    palAvgAge <- colorNumeric(
      palette = "GnBu",
      domain = censusAvgAge$`Average Age`,
      na.color = "#e6e6e6"
    )

    #
    # STIR observer
    #
    # SHELTER-COST-TO-INCOME RATIO
    censusStir <- sf::st_as_sf(
      censusStir() %>% dplyr::select(dplyr::everything())
    )

    # STIR palette
    palStir <- colorBin(
      palette = "GnBu",
      domain = censusStir$percent_more_than_30, n = 10
    )

    # 02.1 - Census Leaflet ----
    # Redraw polygons when geo-level or housing type selection changes
    output$mapCensus <- leaflet::renderLeaflet({
      leaflet::leaflet() %>%
        addProviderTiles(
          provider = "CartoDB.Positron",
          options = providerTileOptions(
            minZoom = 5,
            maxZoom = 12
          )
        ) %>%
        setView(lng = -125.2, lat = 54.7, zoom = 5) %>%
        addPolygons(
          data = housingTypeMapData(),
          label = ~ `Region`, color = '#333',
          fillColor = ~ palHousingTypes(housingTypeMapData()$typewatch),
          stroke = TRUE, weight = 1, fillOpacity = 0.5, smoothFactor = 0.2,
          layerId = ~ paste0("ht-", GeoUID), group = "Housing",
          popup = paste0(
            "<strong>", paste0(housingTypeMapData()$`Region`, " (", housingTypeMapData()$GeoUID), ")</strong>",
            "<table class=\"leaflet-popup-table\"><tr><td>Census Year</td><td class=\"text-right\">2016</td></tr>",
            "<tr><td>Single family homes</td><td class=\"text-right\">",
            FormatCount(housingTypeMapData()$`Single detached house`),
            "</td></tr><tr><td>Semi detached house</td><td class=\"text-right\">",
            FormatCount(housingTypeMapData()$`Semi detached house`),
            "</strong></td></tr><tr><td>Apartment in duplex</td><td class=\"text-right\">",
            FormatCount(housingTypeMapData()$`Apartment in duplex`),
            "</strong></td></tr><tr><td>Row house</td><td class=\"text-right\">",
            FormatCount(housingTypeMapData()$`Row house`),
            "</strong></td></tr><tr><td>Apartment in small building</td><td class=\"text-right\">",
            FormatCount(housingTypeMapData()$`Apartment in small building`),
            "</strong></td></tr><tr><td>Apartment in tall building ratio</td><td class=\"text-right\">",
            FormatCount(housingTypeMapData()$`Apartment in tall building`),
            "</strong></td></tr><tr><td>Other single attached house</td><td class=\"text-right\">",
            FormatCount(housingTypeMapData()$`Other single attached house`),
            "</strong></td></tr><tr><td>Movable dwelling</td><td class=\"text-right\">",
            FormatCount(housingTypeMapData()$`Movable dwelling`),
            "</strong></td></tr></table>"
          ),
          highlight = highlightOptions(
            weight = 5, color = "#696969", fillOpacity = 0.75, bringToFront = TRUE
          )
        ) %>%
        addPolygons(
          data = censusAvgAge,
          label = ~ `Region`, color = '#333', fillColor = ~ palAvgAge(censusAvgAge$`Average Age`),
          stroke = TRUE, weight = 1, fillOpacity = 0.5, smoothFactor = 0.2,
          layerId = ~ paste0("p-", GeoUID), group = "Population",
          popup = paste0(
            "<strong>", paste0(censusAvgAge$`Region`, " (", censusAvgAge$GeoUID), ")</strong>",
            "<table class=\"leaflet-popup-table\">",
            "<tr><td>Census Year</td><td class=\"text-right\">2016</td></tr>",
            "<tr><td>Population</td><td class=\"text-right\">",
            FormatCount(censusAvgAge$Population),
            "</td></tr><tr><td>Dwellings</td><td class=\"text-right\">",
            FormatCount(censusAvgAge$Dwellings),
            "</td></tr><tr><td>Households</td><td class=\"text-right\">",
            FormatCount(censusAvgAge$Households),
            "</td></tr><tr><td><strong>Average Age</strong></td><td class=\"text-right\"><strong>",
            FormatFloat(censusAvgAge$`Average Age`),
            "</strong></td></tr></table>"
          ),
          highlight = highlightOptions(
          weight = 5, color = "#696969", fillOpacity = 0.75, bringToFront = TRUE)
        ) %>%
        addPolygons(
          data = censusMobility,
          label = ~ `Region`, color = '#333', fillColor = ~ palMobility(censusMobility$`Movers Ratio`),
          stroke = TRUE, weight = 1, fillOpacity = 0.5, smoothFactor = 0.2,
          layerId = ~ paste0("m-", GeoUID), group = "Mobility",
          popup = paste0(
            "<strong>", paste0(censusMobility$`Region`, " (", censusMobility$GeoUID), ")</strong>",
            "<table class=\"leaflet-popup-table\"><tr><td>Census Year</td><td class=\"text-right\">2016</td></tr>",
            "<tr><td>Population</td><td class=\"text-right\">",
            FormatCount(censusMobility$Population),
            "</td></tr><tr><td>Dwellings</td><td class=\"text-right\">",
            FormatCount(censusMobility$Dwellings),
            "</td></tr><tr><td>Households</td><td class=\"text-right\">",
            FormatCount(censusMobility$Households),
            "</td></tr><tr><td><strong>Movers Ratio</strong></td><td class=\"text-right\"><strong>",
            FormatFloat(censusMobility$`Movers Ratio`),
            "</strong></td></tr></table>"
          ),
          highlight = highlightOptions(
          weight = 5, color = "#696969", fillOpacity = 0.75, bringToFront = TRUE)
        ) %>%
        addPolygons(
          data = censusStir,
          label = ~ `Region`, color = '#333', fillColor = ~ palStir(censusStir$percent_more_than_30),
          stroke = TRUE, weight = 1, fillOpacity = 0.5, smoothFactor = 0.2,
          layerId = ~ paste0("s-", GeoUID), group = "STIR",
          popup = paste0(
            "<strong>", paste(censusStir$`Region`), "</strong>",
            "<table class=\"leaflet-popup-table\"><tr><td>Census Year</td><td class=\"text-right\">2016</td></tr>",
            "<tr><td>Households with income</td><td class=\"text-right\">",
            FormatCount(censusStir$total_households_with_income),
            "</td></tr><tr><td>STIR < 30%</td><td class=\"text-right\">",
            FormatCount(censusStir$stir_less_than_30),
            "</td></tr><tr><td><strong>STIR > 30%</strong></td><td class=\"text-right\"><strong>",
            FormatCount(censusStir$stir_more_than_30),
            "</strong></td></tr></table>"
          ),
          highlight = highlightOptions(
          weight = 5, color = "#696969", fillOpacity = 0.5, bringToFront = TRUE)
        ) %>%
        # Layers control
        addLayersControl(
          baseGroups = c("Housing", "Population", "Mobility", "STIR"),
          options = layersControlOptions(collapsed = FALSE)
        )
    })
    outputOptions(output, "mapCensus", suspendWhenHidden = FALSE)

    # 02.2 - Map layers click observer ----
    observeEvent(input$mapCensus_groups, {
      selectedGroup <- input$mapCensus_groups

      mapCensus <- leaflet::leafletProxy("mapCensus") %>% clearControls()

      updateTabsetPanel(session, "censusTopicsTabs", selected = selectedGroup)

      if (selectedGroup == 'Population') {
        mapCensus %>%
          addLegend(
            "bottomleft", title = "Average age", opacity = 0.5,
            pal = palAvgAge, values = censusAvgAge$`Average Age`
          )
      } else if (selectedGroup == 'Housing') {
        mapCensus %>%
          addLegend(
            title = '', #input$c_housing_types
            "bottomleft", opacity = 0.5,
            pal = palHousingTypes, values = housingTypeMapData()$typewatch,
            labFormat = labelFormat(suffix = "%")
          )
      } else if (selectedGroup == 'Mobility') {
        mapCensus <- mapCensus %>%
          addLegend(
            "bottomleft", title = "Movers ratio", opacity = 0.5,
            pal = palMobility, values = censusMobility$`Movers Ratio`
          )
      } else if (selectedGroup == 'STIR') {
        mapCensus <- mapCensus %>%
          addLegend(
            title = "Percentage of<br>households with<br>STIR > 30%", "bottomleft", opacity = 0.5,
            pal = palStir, values = censusStir$percent_more_than_30,
            labFormat = labelFormat(suffix = "%")
          )
      }
    })

    # 02.3 - Housing Types treemap ----
    # Have to drop geometry, i.e. convert sf to df to use in treemap
    housingTypesDf <- housingTypes
    sf::st_geometry(housingTypesDf) <- NULL

    housingTypesDf <- housingTypesDf %>%
      tidyr::gather(
        "Single detached house ratio",
        "Apartment in tall building ratio",
        "Semi detached house ratio",
        "Row house ratio",
        "Apartment in duplex ratio",
        "Apartment in small building ratio",
        "Other single attached house ratio",
        "Movable dwelling ratio",
        key = "HousingType", value = "ratio")

    output$housingTypeTreemap <- renderPlot({
      treemap::treemap(
        title = paste("Distribution of Housing Types in ", locationLabel()),
        housingTypesDf %>%
          dplyr::filter(GeoUID == input$c_location) %>%
          dplyr::mutate(ratioFormat = paste0(gsub(" ratio", "", HousingType), " - ", ratio, "%")),
        index = c("ratioFormat"),
        vSize = "ratio",
        type = "value",
        vColor = "ratio",
        # palette = c(colMultiFam, colStrata, colNonStrataRental, colForeign, colAcreage, colResidential, colCommercial, colSingleFam),
        palette = RColorBrewer::brewer.pal(8, "RdYlBu"),
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
        # title.legend = "Housing Types",
        position.legend = "bottom",
        fontsize.legend = 11,
        format.legend = list(scientific = FALSE, big.mark = " "),
        drop.unused.levels = TRUE,
        inflate.labels = F
      )
    })

    #
    # 02.4 - Mobility sunburst chart ----
    #
    # Have to drop geometry, i.e. convert sf to df to use in treemap
    censusMobilityDf <- censusMobilityGathered
    sf::st_geometry(censusMobilityDf) <- NULL

    output$mobility_sunburst_title <- renderText(paste("Mobility Distribution for ", locationLabel()))
    # sequenceColors = c(palLighterBlue, colCommercial, "steelblue", palDarkBlue, "#009900", palOther, "palegreen", palLightBlue)
    sequenceColors = c('#08306b', colCommercial, '#c6dbef', '#9ecae1', '#6baed6', '#4292c6', '#2171b5', '#08519c')
    output$mobilitySunburst <- sunburstR::renderSunburst({
      sb <- sunburstR::sunburst(
        censusMobilitySeq %>%
          dplyr::filter(GeoUID == input$c_location) %>%
          dplyr::select(sequence, count),
        colors = sequenceColors,
        percent = TRUE,
        count = TRUE,
        height = 400,
        legend = list(w = 200, h = 25, s = 2, t = 10),
        withD3 = TRUE
      )
      htmlwidgets::onRender(
        sb,
        "
      function(el, x) {
        d3.selectAll('.sunburst-legend text').attr('font-size', '12px');
        d3.select(el).select('.sunburst-togglelegend').property('checked', true);
        d3.select(el).select('.sunburst-legend').style('visibility', '');
      }
      "
      )
    })

    #
    # 02.5 - Population Pyramid ----
    #
    output$popPyr <- plotly::renderPlotly({
      p <- plotly::plot_ly(
        censusPp2016LocationCompare(),
        x = ~percentage_2016, y = ~age,
        name = ~paste(Region, '2016', sex),
        color = ~sex, type = 'bar', orientation = 'h',
        hoverinfo = 'y+text+name',
        text = ~paste('Census 2016</br>', Region, abs(percentage_2016), '%'),
        opacity = 0.75,
        colors = c(colCommercial, colMultiFam)) %>%
        add_annotations(xref = 'x', yref = 'y',
                        x = ~(percentage_2016 / abs(percentage_2016) / 2), y = ~age,
                        text = ~paste(abs(percentage_2016), '%'),
                        font = list(family = 'Arial', size = 12,
                                    color = 'rgb(67, 67, 67)'),
                        showarrow = FALSE)

      if (input$c_pp_compare_2011 == TRUE) {
        p <- p  %>% add_trace(x = ~percentage_2011, y = ~age, name = ~paste(Region, '2011', sex), type = "scatter", mode = 'lines+markers',
                         line = list(color = '#22229999', shape = "spline"),
                         hoverinfo = "x+y+text",
                         text = ~paste('Census 2011</br>', Region, abs(percentage_2011), '%')
        )
      }

      if (input$c_pp_compare_2006 == TRUE) {
        p <- p  %>% add_trace(x = ~percentage_2006, y = ~age, name = ~paste(Region, '2006', sex), type = "scatter", mode = 'lines+markers',
                         line = list(color = '#22992299', shape = "spline"),
                         hoverinfo = "x+y+text",
                         text = ~paste('Census 2006</br>', Region, abs(percentage_2006), '%'))
      }

      if (input$c_location_pp_compare != "") {
        p <- p  %>% add_trace(x = ~percentage_compare, y = ~age, name = ~ paste(Region_compare, '2016', sex), type = "scatter", mode = 'lines+markers',
                         line = list(color = '#cc222299', shape = "spline"),
                         hoverinfo = "x+y+text",
                         text = ~paste('Census 2016</br>', Region_compare, abs(percentage_compare), '%')) %>%
          add_annotations(xref = 'paper', yref = 'paper',
                          x = 0.5, y = 1.05,
                          text = ~paste('compared to', locationCompareLabel()),
                          font = list(size = 12, color = '#969696'),
                          showarrow = FALSE)
      }
      p %>%
        plotly::layout(bargap = 0.05, barmode = 'overlay',
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
               title = PlotlyChartTitle(
                 title_text = paste('Population Pyramid for', locationLabel(), '- Census year 2016')
               ),
               legend = list(orientation = 'h')) %>%
        config(
          displaylogo = FALSE,
          modeBarButtonsToRemove = c("zoomIn3d", "zoomOut3d", "resetScale2d", "toggleSpikelines")
        )
    })

    # 02.6 - STIR Stacked Bar ----
    topLabels <- c('More than 30%', 'Less than 30%')
    labelPositions <- c(
      censusStir()[1,"percent_more_than_30"]$percent_more_than_30 / 2,
      censusStir()[1,"percent_more_than_30"]$percent_more_than_30 +
        censusStir()[1,"percent_less_than_30"]$percent_less_than_30 / 2
    )
    output$stirStacked <- renderPlotly(
      plotly::plot_ly(
        dataStir(),
        x = ~percent_more_than_30, name = "More than 30%",
        y = ~`GeoUID`, type = 'bar', orientation = 'h',
        opacity = 0.75,
        marker = list(
          color = colCommercial,
          line = list(color = 'rgb(248, 248, 249)', width = 1),
          hoverinfo="x+y+name"
        )
      ) %>%
        add_trace(
          x = ~percent_less_than_30,
          name = "Less than 30%",
          marker = list(color = colMultiFam)
        ) %>%
        plotly::layout(xaxis = list(title = "",
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
        config(
          displaylogo = FALSE,
          modeBarButtonsToRemove = c("zoomIn3d", "zoomOut3d", "resetScale2d", "toggleSpikelines")
        )
    )
  })

  # 02.7 - Census Map region click observer ----
  observeEvent(input$mapCensus_shape_click, priority = 1, {
    m <- input$mapCensus_shape_click
    if (!is.null(m$id)) {
      id <- str_split(m$id, "-", simplify = TRUE)
      locationId = id[1, 2]

      updateTextInput(session, "c_location", value = locationId)

      locationLabel <- as.data.frame(censusMobility()) %>%
        dplyr::filter(GeoUID == locationId) %>%
        dplyr::mutate(label = paste0(Region, " (", GeoUID, ")")) %>%
        dplyr::pull(label)
      updateTextInput(session, "c_location_name", value = locationLabel)
    }
    freezeReactiveValue(input, "mapCensus_click")
  })

  observeEvent(input$mapCensus_click, {
    m <- input$mapCensus_click
  })
}
