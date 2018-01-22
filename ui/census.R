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
        column(
          1,
          selectInput("c_year", "Period", c("2016", "2011", "2006"), multiple = FALSE)),
        column(
          2,
          selectizeInput('c_view', choices = geoLevels, label = "Geographical Level")),
        column(
          3,
          disabled(
            textInput("c_location_name", label = "Location", placeholder = "Click map region to select location")
          ),
          tags$div(
            class = "hidden",
            textInput("c_location", label = "Location")
          )
        )
      )
    ),

    fluidRow(
      column(
        5,
        leafletOutput("mapCensus", height = mapHeight * 1.5) %>% withSpinner(color="#0dc5c1")
      ),
      column(
        7,
        # Population pyramid
        tabsetPanel(
          tabPanel(
            "Population Age & Sex",
            tags$p("The age profile of an area has a significant impact on the type of housing that is required."),
            tags$p("An abundance of children suggests a need for family housing, while a greater proportion of seniors
                   may indicate a need for “downsized” housing."),
            fluidRow(
              conditionalPanel(
                condition = "input.c_location != ''",
                column(
                  12,
                  fluidRow(
                    selectizeInput("c_location_pp_compare", label = "Compare with", choices = NULL, options = list(placeholder = "Select a location")),
                    plotlyOutput("popPyr", height = chartHeight, width = "100%") %>% withSpinner(color="#0dc5c1"))
                )
              )
            )
          ),

          # Mobility
          tabPanel(
            "Mobility",
            tags$p("This report shows number of people who had moved to the current location in the previous year."),
            fluidRow(
              conditionalPanel(
                condition = "input.c_location != ''",
                column(12, plotOutput("c16mobilityTree", height = chartHeight) %>% withSpinner(color="#0dc5c1"))
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
              selectizeInput('c_housing_types', choices = housingTypesList, label = "Housing Type")
            ),
            fluidRow(
              conditionalPanel(
                condition = "input.c_location != ''",
                column(12, plotOutput("housingTypeTreemap", height = chartHeight) %>% withSpinner(color="#0dc5c1"))
              )
            ),
            fluidRow(
              column(12, plotlyOutput("c16dwellType", height = chartHeight) %>% withSpinner(color="#0dc5c1"))
            )
          ),

          # Shelter-to-Income Ratio
          tabPanel(
            "Shelter-to-Income Ratio",
            tags$p("Housing is considered affordable when spending on all shelter costs is below 30% of pre-tax income
              and measured through the Shelter-cost-To-Income Ratio (STIR)."),
            tags$p("The reports shows proportion of households with greater than 30% of pre-tax income spent on shelter."),
            fluidRow(
              column(12,
                plotlyOutput("stirLollipop", height = chartHeight) %>% withSpinner(color="#0dc5c1")
              ),
              column(
                12,
                plotlyOutput("stirStacked", height = chartHeight) %>% withSpinner(color="#0dc5c1")
              )
            )
          )
        )
      )
    )
  )
)
