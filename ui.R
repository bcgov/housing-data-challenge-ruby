ui <- navbarPage(
    theme = "css/bcgov.css",
    title = "Housing Market",
    tabPanel("Home",
             fluidPage(
                 jumbotron(
                     header = "BC Housing Market Data Visualization project",
                     popPerc = c16Prov$Population.Change,
                     popInc = TRUE,
                     dwellPerc = c16Prov$Total.Private.Dwellings.Change,
                     dwellInc = TRUE,
                     trans_period = maxTransPeriod,
                     no_mkt_trans = ptProvMth[ptProvMth$trans_period == maxTransPeriod, "no_mkt_trans"],
                     no_foreign_perc = ptProvMth[ptProvMth$trans_period == maxTransPeriod, "no_foreign_perc"] ,
                     sum_FMV = ptProvMth[ptProvMth$trans_period == maxTransPeriod, "sum_FMV"],
                     sum_FMV_foreign_perc = ptProvMth[ptProvMth$trans_period == maxTransPeriod, "sum_FMV_foreign_perc"]
                 )
             )),
    tabPanel('Overview',
             fluidPage(
                 titlePanel("Property Sales Monthly Overview"),
                 fluidRow(
                     column(4, plotlyOutput("pt_mothly_fmv", height = chartHeight)),
                     column(4, plotlyOutput("pt_mothly_mnd_fmv", height = chartHeight)),
                     column(4, plotlyOutput("pt_mothly_ptt", height = chartHeight))
                 ),
                 fluidRow(
                     column(4, plotlyOutput("pt_mothly", height = chartHeight)),
                     column(4, plotlyOutput("pt_mothly_res", height = chartHeight)),
                     column(4, plotlyOutput("pt_mothly_comm", height = chartHeight))
                 )
             )),
    tabPanel(
        'Monthly Data',
        fluidPage(
            titlePanel("BC Housing Data Visualization"),
            tags$p(
                "Current map is based on census division boundaries and property transfer tax data"
            ),
            sidebarLayout(
                sidebarPanel(
                    width = 2,
                    # selectInput("pt_trans_period", "Transaction Period", transPeriods),
                    selectInput(
                        "pt_trans_period",
                        "Transaction Period",
                        levels(propertyTax$trans_period),
                        multiple = FALSE
                    ),
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
            ),
            tabsetPanel(
                tabPanel(
                    "Foreign Involvement",
                    column(4, plotlyOutput("no_foreign_period", height = chartHeight)),
                    column(4, plotlyOutput("foreign_period_mn", height = chartHeight)),
                    column(4, plotlyOutput("foreign_period_md", height = chartHeight))
                ),
                tabPanel("Tabular Data", dataTableOutput("dt")),
                tabPanel(
                    "Population and Dwellings",
                    column(4, plotlyOutput("c16pop", height = chartHeight)),
                    column(4, plotlyOutput("c16dwell", height = chartHeight)),
                    column(4, plotlyOutput("c16change", height = chartHeight))
                )
            )
        )
    )
)