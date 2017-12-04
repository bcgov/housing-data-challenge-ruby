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
  tabPanel('PTT Overview',
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
    'PTT Monthly',
    fluidPage(
      titlePanel("BC Housing Data Visualization"),
      tags$p(
        "Current map is based on census division boundaries and property transfer tax data."
      ),
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
      ),
      tabsetPanel(
        tabPanel(
          "Foreign Involvement",
          column(4, plotlyOutput("no_foreign_period", height = chartHeight)),
          column(4, plotlyOutput("foreign_period_mn", height = chartHeight)),
          column(4, plotlyOutput("foreign_period_md", height = chartHeight))
        ),
        tabPanel("Tabular Data", dataTableOutput("dt")) #,
        # tabPanel(
        #     "Population and Dwellings",
        #     column(4, plotlyOutput("c16pop", height = chartHeight)),
        #     column(4, plotlyOutput("c16dwell", height = chartHeight)),
        #     column(4, plotlyOutput("c16change", height = chartHeight))
        # )
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
           )),
  tabPanel(
    'Census',
    fluidPage(
      titlePanel("Census Data Visualization"),
      tags$p(
        "Current map is based on census division boundaries and Census Canada data"
      ),
      sidebarLayout(
        sidebarPanel(
          width = 2,
          selectInput("c_year",
                      "Period",
                      c("2016", "2011", "2006"),
                      multiple = FALSE),
          ctrlPeriodInput(
            "c_view",
            c(
              "Regions" = "reg",
              "CMA" = "cma",
              "CD" = "cd",
              "CSD" = "csd",
              "CT" = "ct",
              "CDA" = "cda"
            ),
            "View"
          ),
          selectInput("c_metric", "Metric", censusCategoriesList)
          # selectInput("c_metric", "Metric", selectionMetrics)
        ),
        mainPanel(
          width = 10,
          column(
            7, leafletOutput("mapCensus", height = mapHeight)
          ),
          column(
            5, plotlyOutput("pyramid", height = mapHeight)
          )
        )
      ),
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
        tabPanel("Population Pyramid",
                 sidebarLayout(
                   sidebarPanel(
                     width = 2,
                     selectInput(
                       "pop_pyr_view",
                       "View",
                       c("CMA" = "CMA")
                     ),
                     selectInput(
                       "pop_pyr_geo",
                       "View",
                       setNames(geoCMA$CMAuid,geoCMA$CMAname)
                     )
                   ),
                   mainPanel(width = 10)
                 )
        )
      )
    )
  ),
  tabPanel(
    'NHS',
    fluidPage(
      titlePanel("National Household Survey 2011"),
      fluidRow(
        column(6, 
          tags$h3("Shelter cost to income ratio by Census Metropolitan Area (CMA)"),
          tags$p("Shelter-cost-to-income ratio refers to the proportion of average total income of household which is spent on shelter costs."),
          tags$p("It is calculated by dividing the average monthly shelter costs by the average monthly total household income and multiplying the result by 100."),
          plotlyOutput("nhs_shelter_cost_ratio", height = 1.5*chartHeight),
          tags$p(tags$a(href="http://www23.statcan.gc.ca/imdb/p3Var.pl?Function=DEC&Id=103407", "StatCan Reference"))
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
              actionButton("c_search_data", "Search")
            ),
            mainPanel(
              width = 10,
              dataTableOutput("c_dt_data")
            )
          )
        )
      )
    )
  )
)
