tabPanel(
  'Census Topics',
  fluidPage(
    titlePanel("Census Data Visualization"),
    tags$p(
      "Source: Statistics Canada (Statistics Canada Open Data License)"
    ),
    tags$p(
      "The 2016 Census and National Household Survey present a wealth of granular and
      detailed information about the socio-demographic characteristics of households in Canada.
      Because the Census is conducted every five years, it is possible to compare these measures over time.
      Relevant to housing are a number of measures captured by the Census and National Household Survey 2011."
    ),
    textOutput("locator"),
    wellPanel(
      fluidRow(
        column(1, selectInput("c_year", "Period", c("2016", "2011", "2006"), multiple = FALSE)),
        column(2, selectizeInput('c_view', choices = geoLevels, label = "Geographical Level")),
        column(3, selectizeInput("c_location", label = "Location",
                                 choices = NULL, options = list(placeholder = "Select location")))
      )
    ),
    # sidebarLayout(
    #   sidebarPanel(
    #     width = 2,
    #   ),
    #   mainPanel(
    #     width = 10,
    # column(
    #   7, leafletOutput("mapCensus", height = mapHeight)
    # ),
    # column(
    #   5, plotlyOutput("pyramid", height = mapHeight)
    # ),
    #   )
    # ),
    tabsetPanel(
      tabPanel(
        "Population Pyramid",
        fluidRow(
          column(10, plotlyOutput("popPyr", height = chartHeight, width = "100%") %>% withSpinner(color="#0dc5c1"), offset = 1)
        ),
        fluidRow(
          column(12, dataTableOutput("popPyrDT") %>% withSpinner(color="#0dc5c1"))
        )
      ),
      tabPanel(
        "Mobility",
        fluidRow(
          column(4, leafletOutput("mapCensusMobility", height = mapHeight) %>% withSpinner(color="#0dc5c1")),
          column(8, d3tree3Output("c16mobilityTree", height = chartHeight) %>% withSpinner(color="#0dc5c1"))
        )
      ),
      tabPanel(
        "Housing Type",
        fluidRow(
          column(4, leafletOutput("mapCensusHousingType", height = mapHeight) %>% withSpinner(color="#0dc5c1")),
          column(8, plotlyOutput("c16dwellType", height = chartHeight) %>% withSpinner(color="#0dc5c1"))
        )
      ),
      tabPanel(
        "Shelter-to-Income Ratio",
        fluidRow(
          column(6, leafletOutput("mapCensusStir", height = mapHeight * 1.5) %>% withSpinner(color="#0dc5c1")),
          column(6, fluidRow(
            plotlyOutput("stirLollipop", height = chartHeight) %>% withSpinner(color="#0dc5c1")
            ),
            fluidRow(
              plotlyOutput("stirStacked", height = chartHeight) %>% withSpinner(color="#0dc5c1")
            )
          )
        )
      )
    )
    )
  )
