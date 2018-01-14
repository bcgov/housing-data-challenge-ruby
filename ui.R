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
          column(4, plotlyOutput("pt_mothly_fmv", height = chartHeight)),
          column(4, plotlyOutput("pt_mothly_mnd_fmv", height = chartHeight)),
          column(4, plotlyOutput("pt_mothly_ptt", height = chartHeight))
        ),
        tabPanel(
          "Property Types",
          tags$p("Charts below show how number of market transactions for different
                 property types (residential, commercial, famrs, etc) change over time."),
          column(4, plotlyOutput("pt_mothly", height = chartHeight)),
          column(4, plotlyOutput("pt_mothly_res", height = chartHeight)),
          column(4, plotlyOutput("pt_mothly_comm", height = chartHeight))
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
            column(4, plotlyOutput("no_foreign_period", height = chartHeight)),
            column(4, plotlyOutput("foreign_period_mn", height = chartHeight)),
            column(4, plotlyOutput("foreign_period_md", height = chartHeight))
          )
        )
      )
    )),
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
      wellPanel(
        fluidRow(
          column(1, selectInput("c_year", "Period", c("2016", "2011", "2006"), multiple = FALSE)),
          column(2, selectizeInput('c_view', choices = geoLevels, label = "Geographical Level")),
          column(2, selectizeInput("c_location", label = "Location",
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
          "Population and Dwellings",
          column(4, plotlyOutput("c16pop", height = chartHeight)),
          column(4, plotlyOutput("c16dwell", height = chartHeight)),
          column(4, plotlyOutput("c16change", height = chartHeight))
        ),
        tabPanel("Age",
          column(6, plotlyOutput("c16avgAge", height = chartHeight)),
          column(6, plotlyOutput("c16ageDist", height = chartHeight))
        ),
        tabPanel("Type of Dwelling",
          column(12, plotlyOutput("c16dwellType", height = chartHeight))
        ),
        tabPanel("Families & Households",
          column(6, plotlyOutput("c16incomeAvgFamSize", height = chartHeight))
        ),
        tabPanel("Income",
          column(6, plotlyOutput("c16incomeTotalMed", height = chartHeight)),
          column(6, plotlyOutput("c16incomeTotalMedaT", height = chartHeight))
        ),
        tabPanel(
          "Mobility",
          sidebarLayout(
            sidebarPanel(
              width = 2,
              selectInput(
                "pop_pyr_geo",
                "View",
                setNames(geoCMA$CMAuid,geoCMA$CMAname)
              )
            ),
            mainPanel(
              width = 10,
              column(6, d3tree3Output("c16mobilityTree", height = chartHeight)),
              column(6, sunburstOutput("c16mobilitySunburst", height = chartHeight))
            )
          )
        ),
        tabPanel("Population Pyramid",
                 plotlyOutput("popPyr", height = chartHeight, width = "100%")
        ),
        tabPanel("STIR",
                 fluidRow(
                   column(6, leafletOutput("mapCensusStir", height = mapHeight)),
                   column(6,
                     fluidRow(
                      plotlyOutput("stirLollipop", height = chartHeight)
                     ),
                     fluidRow(
                      plotlyOutput("stirStacked", height = chartHeight)
                     )
                   )
                 )
        )
      )
    )
  ),
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
  ),
  tabPanel('BC Assessment',
           fluidPage(
             titlePanel("BC Assessment Data Visualization"),
             tags$p(
               "Current map is based on census division boundaries and BC Assessment data"
             ),
             tabsetPanel(tabPanel("Tab 1"),
                         tabPanel("Tab 2"),
                         tabPanel("Tab 3"))
           ))
)
