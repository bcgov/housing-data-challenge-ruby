tabPanel(
  'Census Topics',
  fluidPage(
    titlePanel("Census Data Visualization"),
    useShinyjs(),

    # bsCollapsePanel(title = paste("Introduction >"),
      tags$p(
        "The 2016 Census and National Household Survey present a wealth of granular and
        detailed information about the socio-demographic characteristics of households in Canada.
        Because the Census is conducted every five years, it is possible to compare these measures over time.
        Relevant to housing are a number of measures captured by the Census and National Household Survey 2011."
      ),
    # ),
    wellPanel(
      bsTooltip(
        "c_view_help", placement = "right", trigger = "hover", options = NULL,
        title = "Changing geographical level will redraw the map and all charts to populate them with data relevant for the selected geographical level."
      ),
      bsTooltip(
        "c_housing_types_help", placement = "right", trigger = "hover", options = NULL,
        title = "Selecting a housing type will redraw the map and shade the areas for selected geographical level based on the ratio of selected housing type compared in all dwellings."
      ),
      bsTooltip(
        "c_location_pp_compare_help", placement = "right", trigger = "hover", options = NULL,
        title = "Selecting a location in this drop-down will draw a trace on the population pyramid chart based on the data for selected location, to allow comparison between primary location clicked on the map and this selected location."
      ),
      bsTooltip(
        "c_pp_compare_2011_help", placement = "right", trigger = "hover", options = NULL,
        title = "Check this box to draw a population pyramid trace based on data from 2011 census."
      ),
      bsTooltip(
        "c_pp_compare_2006_help", placement = "right", trigger = "hover", options = NULL,
        title = "Check this box to draw a population pyramid trace based on data from 2006 census."
      ),
      fluidRow(
        # column(
        #   1,
        #   selectInput("c_year", "Period", c("2016", "2011", "2006"), multiple = FALSE)
        # ),
        column(
          2,
          selectizeInput('c_view', choices = geoLevels, label = HTML('Geographical Level <i id="c_view_help" class="fa fa-question-circle-o"></i>'))#,
        ),
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
          id = "censusTopicsTabs",
          tabPanel(
            "Population Age & Sex",
            value = "Population",
            icon = icon("venus-mars"),
            tags$p("The age profile of an area has a significant impact on the type of housing that is required.
                   An abundance of children suggests a need for family housing, while a greater proportion of seniors
                   may indicate a need for “downsized” housing."),
            fluidRow(
              conditionalPanel(
                condition = "input.c_location != ''",
                column(
                  12,
                  fluidRow(
                    column(
                      3,
                      tags$div(
                        align = 'left',
                        class = 'multicol',
                        checkboxInput("c_pp_compare_2011", label = HTML('Census 2011 <i id="c_pp_compare_2011_help" class="fa fa-question-circle-o"></i>'), value = FALSE),
                        checkboxInput("c_pp_compare_2006", label = HTML('Census 2006 <i id="c_pp_compare_2006_help" class="fa fa-question-circle-o"></i>'), value = FALSE)
                      )
                    ),
                    column(
                      4,
                      selectizeInput("c_location_pp_compare",
                                     label = HTML('Compare with <i id="c_location_pp_compare_help" class="fa fa-question-circle-o"></i>'),
                                     choices = NULL, options = list(placeholder = "Select a location"))
                    )
                  ),
                  fluidRow(
                    plotlyOutput("popPyr", height = chartHeight, width = "100%") %>% withSpinner(color="#0dc5c1")
                  )
                )
              ),
              conditionalPanel(
                condition = "input.c_location == ''",
                bsAlert("c_location_alert_pp")
              )
            )
          ),

          # Mobility
          tabPanel(
            "Mobility",
            value = "Mobility",
            icon = icon("truck"),
            tags$p("This report shows number of people who had moved to the current location in the previous year."),
            fluidRow(
              conditionalPanel(
                condition = "input.c_location != ''",
                column(12, sunburstOutput("mobilitySunburst", height = chartHeight) %>% withSpinner(color="#0dc5c1")),
                column(12, plotOutput("c16mobilityTree", height = chartHeight) %>% withSpinner(color="#0dc5c1"))
              ),
              conditionalPanel(
                condition = "input.c_location == ''",
                bsAlert("c_location_alert_m")
              )

            )
          ),

          # Housing Type
          tabPanel(
            "Housing Type",
            value = "Housing",
            icon = icon("home"),
            tags$p("For the purpose of the Census, housing type is defined by \"structural type\",
                   which includes single detached house, semi-detached and row houses, and a variety of apartment categories."),
            tags$p("This report gives insights into diversity of the housing types in an area."),
            fluidRow(
              selectizeInput('c_housing_types', choices = housingTypesList,
                             label = HTML('Housing Type <i id="c_housing_types_help" class="fa fa-question-circle-o"></i>'))
            ),
            fluidRow(
              conditionalPanel(
                condition = "input.c_location != ''",
                column(12, plotOutput("housingTypeTreemap", height = chartHeight) %>% withSpinner(color="#0dc5c1"))
              ),
              conditionalPanel(
                condition = "input.c_location == ''",
                bsAlert("c_location_alert_ht")
              )

            ),
            fluidRow(
              column(12, plotlyOutput("c16dwellType", height = chartHeight) %>% withSpinner(color="#0dc5c1"))
            )
          ),

          # Shelter-to-Income Ratio
          tabPanel(
            "Shelter-to-Income Ratio",
            value = "STIR",
            icon = icon("money"),
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
    ),
    tags$p("Data source: Statistics Canada (Statistics Canada Open Data License)")
  )
)
