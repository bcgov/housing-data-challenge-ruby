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
            "pt_trans_period",
            label = HTML('Period <i id="pt_trans_period_help" class="fa fa-question-circle-o"></i>'),
            choices = levels(propertyTax$trans_period),
            multiple = FALSE
          )
        ),
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
          3,
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
      column(5, leafletOutput("mapPtt", height = mapHeight)),
      column(7,
        tabsetPanel(
          tabPanel(
            "FMV",
            tags$p("Total fair market value by month."),
            plotlyOutput("pt_mothly_fmv", height = chartHeight) %>% withSpinner(color="#0dc5c1")
          ),
          tabPanel(
            "Average FMV",
            tags$p("Average fair market value by month."),
            plotlyOutput("pt_mothly_mnd_fmv", height = chartHeight) %>% withSpinner(color="#0dc5c1")
          ),
          tabPanel(
            "Tax Paid",
            tags$p("Total property transfer tax paid by month."),
            plotlyOutput("pt_mothly_ptt", height = chartHeight) %>% withSpinner(color="#0dc5c1")
          ),
          tabPanel(
            "Property Types",
            tags$p("Number of market transactions for different
                   property types (residential, commercial, famrs, etc) by month."),
            plotlyOutput("pt_mothly", height = chartHeight) %>% withSpinner(color="#0dc5c1")
          ),
          tabPanel(
            "Residential",
            tags$p("Number of market transactions for residential properties by month."),
            plotlyOutput("pt_mothly_res", height = chartHeight) %>% withSpinner(color="#0dc5c1")
          ),
          tabPanel(
            "Commercial",
            tags$p("Number of market transactions for commercial properties by month."),
            plotlyOutput("pt_mothly_comm", height = chartHeight) %>% withSpinner(color="#0dc5c1")
          ),
          tabPanel(
            "Variable",
            tags$p("Charts below show the participation of foreign citizens
                   in number of market transactions, average and median sales price (Fair Market Value)
                   for different locations, for chosen period and geogprahical level."),
            fluidRow(
              plotlyOutput("interactive", height = mapHeight)
            )
          )
        )
      )
    )
  )
)
