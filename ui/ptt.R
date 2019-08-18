tabPanel(
  'Property Transfer Tax',
  fluidPage(
    titlePanel("Property Sales Monthly Overview"),
    tags$p("Property Transfer Tax data gives accurate measures of the total number of
           market transactions and changes in time, by a variety of levels
           of geography (e.g. municipality and regional district),
           property types (residential, commercial, farm, etc.),
           the average prices and tax amount paid, and the proportion of
           foreign participation in those transactions."),

    wellPanel(
      bsTooltip(
        "pt_trans_period_help", placement = "right", trigger = "hover", options = NULL,
        title = "Changing transaction period will redraw the map and all charts to populate them with data relevant for the selected transaction period."
      ),
      bsTooltip(
        "pt_geo_level_help", placement = "right", trigger = "hover", options = NULL,
        title = "Changing geographical level will redraw the map and all charts to populate them with data relevant for the selected geographical level."
      ),
      bsTooltip(
        "pt_metric_help", placement = "right", trigger = "hover", options = NULL,
        title = "Selecting a different variable in this drop-down will redraw the map and selected charts to highlight selectec variable."
      ),
      fluidRow(
        column(
          2,
          selectInput(
            "pt_view",
            label = HTML('View <i id="pt_geo_level_help" class="fa fa-question-circle-o"></i>'),
            c(
              "Regional District" = "regdis",
              "Development Region" = "devreg",
              "Municipality" = "mun"
            )
          )
        ),
        column(
          2,
          selectizeInput(
            "pt_trans_period",
            label = HTML('Period <i id="pt_trans_period_help" class="fa fa-question-circle-o"></i>'),
            choices = periodSelection,
            multiple = FALSE,
            selected = maxTransPeriod
          )
        ),
        column(
          2,
          selectInput("pt_metric", label = HTML('Variable <i id="pt_metric_help" class="fa fa-question-circle-o"></i>'),
                      choices = selectionMetrics)
        ),
        column(
          3,
          disabled(
            textInput("pt_location_name", label = "Location", placeholder = "Click map region to select location")
          ),
          tags$div(
            class = "hidden",
            textInput("pt_location", label = "Location")
          )
        )
      )
    ),

    fluidRow(
      column(5, leafletOutput("mapPtt", height = mapHeightPtt)),
      column(7,
        tabsetPanel(
          tabPanel(
            "FMV",
            icon = icon("briefcase"),
            tags$p("Total fair market value by month."),
            conditionalPanel(
              condition = "input.pt_location != ''",
              plotlyOutput("pt_mothly_fmv", height = chartHeight) %>% withSpinner(color="#0dc5c1")
            ),
            conditionalPanel(
              condition = "input.pt_location == ''",
              bsAlert("pt_location_alert_fmv")
            )
          ),
          tabPanel(
            "Average FMV",
            icon = icon("calculator"),
            tags$p("Mean and Median fair market value by month, total and foreign."),
            conditionalPanel(
              condition = "input.pt_location != ''",
              plotlyOutput("pt_mothly_mnd_fmv", height = chartHeight) %>% withSpinner(color="#0dc5c1")
            ),
            conditionalPanel(
              condition = "input.pt_location == ''",
              bsAlert("pt_location_alert_fmv_avg")
            )
          ),
          tabPanel(
            "Tax Paid",
            icon = icon("money"),
            tags$p("Total property transfer tax paid by month."),
            conditionalPanel(
              condition = "input.pt_location != ''",
              plotlyOutput("pt_mothly_ptt", height = chartHeight) %>% withSpinner(color="#0dc5c1")
            ),
            conditionalPanel(
              condition = "input.pt_location == ''",
              bsAlert("pt_location_alert_tax")
            )
          ),
          tabPanel(
            "Property Types",
            icon = icon("building-o"),
            tags$p("Number of market transactions for different
                   property types (residential, commercial, farms, etc) by month."),
            conditionalPanel(
              condition = "input.pt_location != ''",
              plotlyOutput("pt_mothly", height = chartHeight) %>% withSpinner(color="#0dc5c1")
            ),
            conditionalPanel(
              condition = "input.pt_location == ''",
              bsAlert("pt_location_alert_types")
            )
          ),
          tabPanel(
            "Residential",
            icon = icon("home"),
            tags$p("Number of market transactions for residential properties by month."),
            conditionalPanel(
              condition = "input.pt_location != ''",
              plotlyOutput("pt_mothly_res", height = chartHeight) %>% withSpinner(color="#0dc5c1")
            ),
            conditionalPanel(
              condition = "input.pt_location == ''",
              bsAlert("pt_location_alert_res")
            )
          ),
          tabPanel(
            "Commercial",
            icon = icon("building"),
            tags$p("Number of market transactions for commercial properties by month."),
            conditionalPanel(
              condition = "input.pt_location != ''",
              plotlyOutput("pt_mothly_comm", height = chartHeight) %>% withSpinner(color="#0dc5c1")
            ),
            conditionalPanel(
              condition = "input.pt_location == ''",
              bsAlert("pt_location_alert_comm")
            )
          ),
          tabPanel(
            "All Locations",
            icon = icon("map"),
            tags$p("Selected variable for each location available in selected geographical level."),
            plotlyOutput("interactive", height = mapHeightPtt)
          )
        )
      )
    )
  )
)
