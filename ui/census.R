tabPanel(
  'Census Topics',
  fluidPage(
    titlePanel("Census Data Visualization"),

    useShinyjs(),

    tags$p(
      "Source: Statistics Canada (Statistics Canada Open Data License)"
    ),
    tags$p(
      "The 2016 Census and National Household Survey present a wealth of granular and
      detailed information about the socio-demographic characteristics of households in Canada.
      Because the Census is conducted every five years, it is possible to compare these measures over time.
      Relevant to housing are a number of measures captured by the Census and National Household Survey 2011."
    ),

    wellPanel(
      fluidRow(
        column(1, selectInput("c_year", "Period", c("2016", "2011", "2006"), multiple = FALSE)),
        column(2, selectizeInput('c_view', choices = geoLevels, label = "Geographical Level")),
        column(3,
               disabled(
                 textInput("c_location_name", label = "Location", placeholder = "Click map region to select location")
               ),
               tags$div(class = "hidden",
                 textInput("c_location", label = "Location")
                 # selectizeInput("c_location", label = "Location",
                 #                   choices = NULL, options = list(placeholder = "Select location"))
               )
        )
      )#,
      # fluidRow(
      #   column(6, offset = 3, tags$small("Location is required for Population Pyramid and Mobility topics."))
      # )
    ),

    # Population pyramid
    tabsetPanel(
      tabPanel(
        "Population Pyramid",
        tags$p("The age profile of an area has a significant impact on the type of housing that is required."),
        tags$p("An abundance of children suggests a need for family housing, while a greater proportion of seniors
               may indicate a need for “downsized” housing."),
        conditionalPanel(
          condition = "input.c_location != ''",
          fluidRow(
            column(10, plotlyOutput("popPyr", height = chartHeight, width = "100%") %>% withSpinner(color="#0dc5c1"), offset = 1)
          )
        ),
        fluidRow(
          column(12, dataTableOutput("popPyrDT") %>% withSpinner(color="#0dc5c1"))
        )
      ),

      # Mobility
      tabPanel(
        "Mobility",
        tags$p("This report shows number of people who had moved to the current location in the previous year."),
        fluidRow(
          column(6, leafletOutput("mapCensusMobility", height = mapHeight) %>% withSpinner(color="#0dc5c1")),
          conditionalPanel(
            condition = "input.c_location != ''",
            column(6, d3tree3Output("c16mobilityTree", height = chartHeight) %>% withSpinner(color="#0dc5c1"))
          )
        )
      ),

      # Housing Type
      tabPanel(
        "Housing Type",
        tags$p("For the purpose of the Census, housing type is defined by \"structural type\",
                which includes single detached house, semi-detached and row houses, and a variety of apartment categories."),
        tags$p("This report gives insights into diversity of the housing types in an area."),
        fluidRow(
          column(12, verbatimTextOutput("c_location_label"))
        ),
        fluidRow(
          column(
            6,
            fluidRow(
              selectizeInput('c_housing_types', choices = housingTypesList, label = "Housing Type")
            ),
            fluidRow(
              leafletOutput("mapCensusHousingType", height = mapHeight) %>% withSpinner(color="#0dc5c1")
            )
          ),
          conditionalPanel(
            condition = "input.c_location != ''",
            column(6, d3tree3Output("housingTypeTreemap", height = chartHeight) %>% withSpinner(color="#0dc5c1"))
          )
        ),
        fluidRow(
          column(12, plotlyOutput("c16dwellType", height = chartHeight) %>% withSpinner(color="#0dc5c1"))
        ),
        fluidRow(
          column(12, dataTableOutput("housingTypesDT") %>% withSpinner(color="#0dc5c1"))
        )
      ),

      # Shelter-to-Income Ratio
      tabPanel(
        "Shelter-to-Income Ratio",
        tags$p("Housing is considered affordable when spending on all shelter costs is below 30% of pre-tax income
          and measured through the Shelter-cost-To-Income Ratio (STIR)."),
        tags$p("The reports shows proportion of households with greater than 30% of pre-tax income spent on shelter."),
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
