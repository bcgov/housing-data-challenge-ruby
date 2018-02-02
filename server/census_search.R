# Search census vectors by keyword
censusSearchDataset <- reactive({
  paste0('CA', substr(paste0(
    input$c_search_data_year
  ), 3, 4))
})

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
