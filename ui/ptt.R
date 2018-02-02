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
    tabsetPanel(
      tabPanel(
        "Monthly",
        tags$p("These charts show how total number of market transactions and
               average prices and tax amounts paid changes in time."),
        column(4, plotlyOutput("pt_mothly_fmv", height = chartHeight) %>% withSpinner(color="#0dc5c1")),
        column(4, plotlyOutput("pt_mothly_mnd_fmv", height = chartHeight) %>% withSpinner(color="#0dc5c1")),
        column(4, plotlyOutput("pt_mothly_ptt", height = chartHeight) %>% withSpinner(color="#0dc5c1"))
        ),
      tabPanel(
        "Property Types",
        tags$p("Charts below show how number of market transactions for different
               property types (residential, commercial, famrs, etc) change over time."),
        column(4, plotlyOutput("pt_mothly", height = chartHeight) %>% withSpinner(color="#0dc5c1")),
        column(4, plotlyOutput("pt_mothly_res", height = chartHeight) %>% withSpinner(color="#0dc5c1")),
        column(4, plotlyOutput("pt_mothly_comm", height = chartHeight) %>% withSpinner(color="#0dc5c1"))
        ),
      tabPanel(
        "Foreign Involvement",
        tags$p("Charts below show the participation of foreign citizens
               in number of market transactions, average and median sales price (Fair Market Value)
               for different locations, for chosen period and geogprahical level."),
        fluidRow(
          sidebarLayout(
            sidebarPanel(
              width = 2,
              ctrlPeriodInput(
                "pt_trans_period",
                levels(propertyTax$trans_period),
                "Period"
              ),
              # selectInput(
              #   "pt_trans_period",
              #   "Transaction Period",
              #   levels(propertyTax$trans_period),
              #   multiple = FALSE
              # ),
              selectInput(
                "pt_view",
                "View",
                c(
                  "Regional District" = "regdis",
                  "Development Region" = "devreg",
                  "Municipality" = "mun"
                )
              ),
              selectInput("pt_metric", "Metric", selectionMetrics)
            ),
            mainPanel(width = 10,
                      column(
                        7, leafletOutput("map", height = mapHeight)
                      ),
                      column(
                        5, plotlyOutput("interactive", height = mapHeight)
                      ))
          )
        ),
        fluidRow(
          column(4, plotlyOutput("no_foreign_period", height = chartHeight) %>% withSpinner(color="#0dc5c1")),
          column(4, plotlyOutput("foreign_period_mn", height = chartHeight) %>% withSpinner(color="#0dc5c1")),
          column(4, plotlyOutput("foreign_period_md", height = chartHeight) %>% withSpinner(color="#0dc5c1"))
        )
        )
    )
))
