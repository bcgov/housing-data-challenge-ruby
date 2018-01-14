tabPanel(
  'Census Search',
  fluidPage(
    titlePanel("Census Search"),

    tabsetPanel(
      tabPanel(
        "Search vectors",
        tags$p(
          "Search census variables by keyword."
        ),
        sidebarLayout(
          sidebarPanel(
            width = 2,
            textInput("c_search_keyword", label = "Keyword", placeholder = "search term"),
            selectInput("c_search_year",
                        "Period",
                        c("2016", "2011", "2006"),
                        multiple = FALSE),
            actionButton("c_search", "Search")
          ),
          mainPanel(
            width = 10,
            dataTableOutput("c_dt")
          )
        )
      ),
      tabPanel(
        "Search data",
        tags$p(
          "Search census data by variable."
        ),
        sidebarLayout(
          sidebarPanel(
            width = 2,
            textInput("c_search_vector", label = "Variable", placeholder = "search variable"),
            selectInput("c_search_data_year",
                        "Period",
                        c("2016", "2011", "2006"),
                        multiple = FALSE),
            selectInput("c_search_data_level",
                        "Level",
                        c("CMA" = "CMA", "CD" = "CD", "CSD" = "CSD", "CT" = "CT", "DA" = "DA"),
                        multiple = FALSE),
            actionButton("c_search_data", "Search")
          ),
          mainPanel(
            width = 10,
            tags$h4("Variable description"),
            tableOutput("c_search_vector_desc"),
            tags$h4("Census data for variable"),
            dataTableOutput("c_dt_data")
          )
        )
      )
    )
  )
)
