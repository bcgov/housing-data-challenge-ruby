#' @title Shiny UI function
#'
#' @import shiny
#' @importFrom shinyjs useShinyjs
#' @importFrom shinyjs disabled
#' @importFrom leaflet leafletOutput
#' @import magrittr
#' @import markdown
#' @import rmarkdown
#' @importFrom lubridate year
#' @importFrom lubridate month
#' @import htmlwidgets
#' @importFrom plotly plotlyOutput
#' @importFrom sunburstR sunburstOutput
#' @importFrom shinycssloaders withSpinner
#' @import bsplus
#'
#' @export
app_ui <- function() {
  data("ptt_prov_dash", package = "bchousing")
  data("ptt_dr_sf", package = "bchousing")
  data("ptt_rd_sf", package = "bchousing")
  data("ptt_mun_sf", package = "bchousing")

  # Selection of metrics, variables and options ----
  selectionMetrics <- list(
    "FMV (Fair Market Value)" = c(
      "Median FMV" = "md_FMV",
      "Total FMV" = "sum_FMV"
    ),
    "PTT (Property Transfer Tax)" = c(
      "Median PTT paid" = "md_PPT",
      "Total PTT paid" = "sum_PPT_paid"
    ),
    "Number of transactions" = c(
      "Total" = "tot_mkt_trans",
      "Residential" = "no_res_trans",
      "Single-family homes" = "n_res_1fam_dwelling"
    )
  )

  # Initialize variables
  maxTransPeriod <- ptt_prov_dash$max_trans_period
  propertyTax <- ptt_rd_sf
  chartHeight <- 600
  mapHeightPtt <- 600
  mapHeightCensus <- 600

  periodSelection <- as.data.frame(propertyTax) %>%
    select(trans_period) %>%
    distinct() %>%
    mutate(label = paste(lubridate::year(trans_period), lubridate::month(trans_period, label = TRUE))) %>%
    rename(value = trans_period) %>%
    arrange(desc(value))
  periodSelection <- setNames(periodSelection$value, periodSelection$label)

  geoLevels <- c(
    "Census Division" = "CD",
    "Census Subdivision" = "CSD",
    "Census Metropolitan Area" = "CMA",
    "Census Tract" = "CT"#,
    # "Census Dissimination Area" = "DA"
  )

  housingTypesList <- c(
    "Single-family detached house" = "Single detached house ratio",
    "Semi-detached house" = "Semi detached house ratio",
    "Apartment in duplex" = "Apartment in duplex ratio",
    "Row house" = "Row house ratio",
    "Apartment in small building" = "Apartment in small building ratio",
    "Apartment in tall building" = "Apartment in tall building ratio",
    "Other single attached house" = "Other single attached house ratio",
    "Movable dwelling" = "Movable dwelling ratio"
  )

  ui <- navbarPage(
    # theme = "css/bcgov.css",
    title = "BC Housing Market",

    # 01. Home page ----
    tabPanel(
      "Home",
      fluidPage(
        Jumbotron(
          header = "BC Housing Market",
          popPerc = c_16_prov$population_change,
          popInc = TRUE,
          dwellPerc = c_16_prov$usual_res_dwellings_change,
          dwellInc = TRUE,
          trans_period = maxTransPeriod,
          no_mkt_trans = no_mkt_trans,
          no_foreign_perc = no_foreign_perc,
          sum_FMV = sum_FMV,
          sum_FMV_foreign_perc = sum_FMV_foreign_perc
        ),
        fluidRow(
          id = "splash-intro",
          tags$div(
            id = "splash-intro-text",
            HTML(
              paste0(
                "This platform is the result of the ",
                tags$strong("Data Visualization Challenge"),
                " launched by the ",
                tags$a(href="https://innovatebc.ca", "Innovate BC"),
                " in partnership with ",
                tags$a(href = "https://www2.gov.bc.ca/gov/content/data/about-data-management/bc-stats", "BC Stats"),
                " in December 2016. The challenge aimed to develop a tool to
                visualize BC Stats’ housing data in a more meaningful and
                impactful way. This platform allows BC Stats to better use their
                data and at the same time, offers interested British Columbians
                the opportunity to interact with housing data, and use it to
                make informed decisions. While the platform is currently in
                its beta version, users are invited to provide feedback on
                this iteration to help make it more effective."
              )
            )
          ),
          tags$hr(),
          tags$div(
            id = "splash-intro-credit",
            HTML(
              paste("Photo by", tags$a(
                href = "https://unsplash.com/photos/596V9_X_naI?utm_source=unsplash&utm_medium=referral&utm_content=creditCopyText",
                "Spencer Watson"), "on", tags$a(
                  href = "https://unsplash.com/search/photos/vancouver?utm_source=unsplash&utm_medium=referral&utm_content=creditCopyText", "Unsplash.")
              )
            )
          )
        )
      )
    ),

    # 02. Census ----
    tabPanel(
      'Census Topics',
      fluidPage(
        titlePanel("Census Data Visualization"),
        shinyjs::useShinyjs(),

        # bsCollapsePanel(title = paste("Introduction >"),
        tags$p(
          "The 2016 Census and National Household Survey present a wealth of granular and
        detailed information about the socio-demographic characteristics of households in Canada.
        Because the Census is conducted every five years, it is possible to compare these measures over time.
        Relevant to housing are a number of measures captured by the Census and National Household Survey 2011."
        ),
        # ),
        wellPanel(
          fluidRow(
            column(
              2,
              selectizeInput(
                'c_view',
                choices = geoLevels,
                label = HTML('Geographical Level')
              ) %>%
                bsplus::shinyInput_label_embed(
                  shiny::icon('question-circle-o') %>%
                    bsplus::bs_embed_tooltip(
                      title = "Changing geographical level will redraw the map and all charts to populate them with data relevant for the selected geographical level.",
                      placement = "right"
                    )
                )
            ),
            column(
              3,
              shinyjs::disabled(
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
            leaflet::leafletOutput("mapCensus", height = mapHeightCensus) %>% shinycssloaders::withSpinner(color="#0dc5c1")
          ),
          column(
            7,
            tabsetPanel(
              id = "censusTopicsTabs",

              # Housing Type
              tabPanel(
                "Housing type",
                value = "Housing",
                # icon = icon("home"),
                selectizeInput(
                  'c_housing_types',
                  choices = housingTypesList,
                  label = HTML('Housing type')
                ) %>%
                  bsplus::shinyInput_label_embed(
                    shiny::icon('question-circle-o') %>%
                      bsplus::bs_embed_tooltip(
                        title = "Changing a housing type selection will redraw the map and shade the areas based on the ratio of selected housing type compared to all types.",
                        placement = "right"
                      )
                  ),
                conditionalPanel(
                  condition = "input.c_location != ''",
                  column(12, plotOutput("housingTypeTreemap", height = chartHeight) %>% shinycssloaders::withSpinner(color = "#0dc5c1"))
                ),
                conditionalPanel(
                  condition = "input.c_location == ''",
                  tags$div(
                    "Click an area on the map to draw a chart showing ratio of different housing types for that location.",
                    class = 'alert alert-info'
                  )
                ),
                tags$p("The Census and National Household Survey divides housing into \"structural types\",
                    which include single detached houses, semi-detached and row houses, and a variety of apartment categories."),
                tags$p("This report gives insights into diversity of the housing types in an area."),
                tags$p("Select different housing types options above to redraw the map and highlight regions by the selected housing type ratios.")
              ),

              # Population pyramid
              tabPanel(
                "Population age & sex",
                value = "Population",
                # icon = icon("venus-mars"),
                conditionalPanel(
                  condition = "input.c_location != ''",
                  column(
                    12,
                    fluidRow(
                      plotly::plotlyOutput("popPyr", height = chartHeight, width = "100%") %>% shinycssloaders::withSpinner(color="#0dc5c1")
                    ),
                    fluidRow(
                      column(
                        3,
                        tags$div(
                          align = 'left',
                          class = 'multicol',
                          checkboxInput(
                            "c_pp_compare_2011",
                            label = HTML('Census 2011'),
                            value = FALSE
                          ) %>%
                            bsplus::shinyInput_label_embed(
                              shiny::icon('question-circle-o') %>%
                                bsplus::bs_embed_tooltip(
                                  title = "Check this box to draw a population pyramid trace based on data from 2011 census.",
                                  placement = "right"
                                )
                            ),
                          checkboxInput(
                            "c_pp_compare_2006",
                            label = HTML('Census 2006'),
                            value = FALSE
                          ) %>%
                            bsplus::shinyInput_label_embed(
                              shiny::icon('question-circle-o') %>%
                                bsplus::bs_embed_tooltip(
                                  title = "Check this box to draw a population pyramid trace based on data from 2006 census.",
                                  placement = "right"
                                )
                            )
                        )
                      ),
                      column(
                        4,
                        selectizeInput(
                          "c_location_pp_compare",
                          label = HTML('Compare with'),
                          choices = NULL,
                          options = list(
                            placeholder = "Select a location",
                            onInitialize = I('function() { this.setValue(""); }')
                          )
                        ) %>%
                          bsplus::shinyInput_label_embed(
                            shiny::icon('question-circle-o') %>%
                              bsplus::bs_embed_tooltip(
                                title = "Selecting a location in this drop-down will draw a trace on the population pyramid chart based on the data for selected location, to allow comparison between primary location clicked on the map and this selected location.",
                                placement = "right"
                              )
                          )
                      )
                    )
                  )
                ),
                conditionalPanel(
                  condition = "input.c_location == ''",
                  tags$div(
                    "Click an area on the map to draw a population distribution by age and sex for that location.",
                    class = 'alert alert-info'
                  )
                ),
                tags$p("The age profile of an area has a significant impact on the type of housing that is required.
             An abundance of children suggests a need for family housing, while a greater proportion of seniors
             may indicate a need for \"downsized\" housing.")
              ),

              # Mobility
              tabPanel(
                "Mobility",
                value = "Mobility",
                # icon = icon("truck"),
                conditionalPanel(
                  condition = "input.c_location != ''",
                  textOutput("mobility_sunburst_title"),
                  sunburstR::sunburstOutput("mobilitySunburst", height = chartHeight) %>% shinycssloaders::withSpinner(color="#0dc5c1")
                ),
                conditionalPanel(
                  condition = "input.c_location == ''",
                  tags$div(
                    "Click an area on the map to draw a chart showing ratios of different population mobility categories for that location.",
                    class = 'alert alert-info'
                  )
                ),
                tags$p("This report shows the number of people who moved to the current location in the previous year.")
              ),

              # Shelter-cost-To-Income Ratio
              tabPanel(
                "Affordability",
                value = "STIR",
                # icon = icon("money"),
                plotly::plotlyOutput("stirStacked", height = chartHeight) %>% shinycssloaders::withSpinner(color="#0dc5c1"),
                tags$p("Housing is considered affordable when total shelter costs are less than 30% of pre-tax income,
                       as measured through the Shelter-cost-To-Income Ratio (STIR)"),
                tags$p("The report shows the proportion of households that spend more than 30% of pre-tax income on shelter.")
              )
            )
          )
        ),
        tags$p("Data source: Statistics Canada (Statistics Canada Open Data Licence)")
      )
    ),

    # 03. PTT ----
    tabPanel(
      'Property Transfer Tax',
      fluidPage(
        titlePanel("Property Sales Monthly Overview"),
        tags$p("Property Transfer Tax data gives accurate measures of the total number of
           market transactions and changes over time, by a variety of levels
           of geography (for example, municipality and regional district),
           property types (for example, residential, commercial, farm),
           the average prices and tax amounts paid, and the proportion of
           foreign participation in those transactions."),

        wellPanel(
          fluidRow(
            column(
              3,
              selectInput(
                "pt_view",
                label = HTML('View'),
                c(
                  "Regional District" = "regdis",
                  "Development Region" = "devreg",
                  "Municipality" = "mun"
                )
              ) %>%
                bsplus::shinyInput_label_embed(
                  shiny::icon('question-circle-o') %>%
                    bsplus::bs_embed_tooltip(
                      title = "Changing geographical level will redraw the map and all charts to populate them with data relevant for the selected geographical level.",
                      placement = "right"
                    )
                )
            ),
            column(
              3,
              selectizeInput(
                "pt_trans_period",
                label = HTML('Period'),
                choices = periodSelection,
                multiple = FALSE,
                selected = maxTransPeriod
              ) %>%
                bsplus::shinyInput_label_embed(
                  shiny::icon('question-circle-o') %>%
                    bsplus::bs_embed_tooltip(
                      title = "Changing transaction period will redraw the map and all charts to populate them with data relevant for the selected transaction period.",
                      placement = "right"
                    )
                )
            ),
            column(
              3,
              selectInput(
                "pt_metric",
                label = HTML('Variable'),
                choices = selectionMetrics
              ) %>%
                bsplus::shinyInput_label_embed(
                  shiny::icon('question-circle-o') %>%
                    bsplus::bs_embed_tooltip(
                      title = "Selecting a different variable in this drop-down will redraw the map and selected charts to highlight selected variable.",
                      placement = "right"
                    )
                )
            ),
            column(
              3,
              shinyjs::disabled(
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
          column(
            5, leaflet::leafletOutput("mapPtt", height = mapHeightPtt)
          ),
          column(
            7,
            tabsetPanel(
              tabPanel(
                "FMV",
                # icon = icon("briefcase"),
                conditionalPanel(
                  condition = "input.pt_location != ''",
                  fluidRow(
                    column(
                      3,
                      bchousing:::ValueBox(
                        value = "fmv_perc_month",
                        location = "fmv_perc_loc"
                      )
                    ),
                    column(
                      3,
                      bchousing:::ValueBox(
                        value = "fmv_perc_res",
                        subtitle = "Residential FMV"
                      )
                    ),
                    column(
                      3,
                      bchousing:::ValueBox(
                        value = "fmv_perc_foreign",
                        subtitle = "Foreign FMV"
                      )
                    ),
                    column(
                      3,
                      bchousing:::ValueBox(
                        value = "fmv_perc_foreign_res",
                        subtitle = "Foreign residential FMV"
                      )
                    )
                  ),
                  plotly::plotlyOutput("pt_mothly_fmv", height = chartHeight) %>% shinycssloaders::withSpinner(color="#0dc5c1")
                ),
                conditionalPanel(
                  condition = "input.pt_location == ''",
                  tags$div(
                    "Click an area on the map to draw a time series chart showing monthly total sales values for that location.",
                    class = 'alert alert-info'
                  )
                ),
                tags$p("Total fair market value by month.")
              ),
              tabPanel(
                "Mean FMV",
                # icon = icon("calculator"),
                # tags$p(cat(getwd())),
                conditionalPanel(
                  condition = "input.pt_location != ''",
                  fluidRow(
                    column(
                      4,
                      bchousing:::ValueBox(
                        value = "fmv_perc_month_mn",
                        location = "fmv_perc_loc_mn"
                      )
                    ),
                    column(
                      4,
                      bchousing:::ValueBox(
                        value = "mn_fmv",
                        subtitle = "Mean FMV"
                      )
                    ),
                    column(
                      4,
                      bchousing:::ValueBox(
                        value = "md_fmv",
                        subtitle = "Median FMV"
                      )
                    )
                  ),
                  plotly::plotlyOutput("pt_mothly_mnd_fmv", height = chartHeight) %>% shinycssloaders::withSpinner(color="#0dc5c1")
                ),
                conditionalPanel(
                  condition = "input.pt_location == ''",
                  tags$div(
                    "Click an area on the map to draw a time series chart showing monthly average sales values for that location.",
                    class = 'alert alert-info'
                  )
                ),
                tags$p("Mean and median fair market value by month, total and foreign.")
              ),
              tabPanel(
                "Tax paid",
                # icon = icon("money"),
                conditionalPanel(
                  condition = "input.pt_location != ''",
                  fluidRow(
                    column(
                      3,
                      bchousing:::ValueBox(
                        value = "fmv_perc_month_ptt",
                        location = "fmv_perc_loc_ptt"
                      )
                    ),
                    column(
                      3,
                      bchousing:::ValueBox(
                        value = "sum_ptt_paid",
                        subtitle = "Total PTT paid"
                      )
                    ),
                    column(
                      3,
                      bchousing:::ValueBox(
                        value = "add_ptt_paid",
                        subtitle = "Additional PTT paid"
                      )
                    ),
                    column(
                      3,
                      bchousing:::ValueBox(
                        value = "md_ppt_paid",
                        subtitle = "Median PPT paid"
                      )
                    )
                  ),
                  plotly::plotlyOutput("pt_mothly_ptt", height = chartHeight) %>% shinycssloaders::withSpinner(color="#0dc5c1")
                ),
                conditionalPanel(
                  condition = "input.pt_location == ''",
                  tags$div(
                    "Click an area on the map to draw a time series chart showing monthly property transfer tax paid for that location.",
                    class = 'alert alert-info'
                  )
                ),
                tags$p("Total property transfer tax paid by month.")
              ),
              tabPanel(
                "Property types",
                # icon = icon("building-o"),
                conditionalPanel(
                  condition = "input.pt_location != ''",
                  fluidRow(
                    column(
                      3,
                      bchousing:::ValueBox(
                        value = "fmv_perc_month_n",
                        location = "fmv_perc_loc_n"
                      )
                    ),
                    column(
                      3,
                      bchousing:::ValueBox(
                        value = "tot_mkt_trans",
                        subtitle = "No. of transactions"
                      )
                    ),
                    column(
                      3,
                      bchousing:::ValueBox(
                        value = "no_res_trans",
                        subtitle = "Residential transactions"
                      )
                    ),
                    column(
                      3,
                      bchousing:::ValueBox(
                        value = "n_comm_tran",
                        subtitle = "Commercial transactions"
                      )
                    )
                  ),
                  plotly::plotlyOutput("pt_mothly", height = chartHeight) %>% shinycssloaders::withSpinner(color="#0dc5c1")
                ),
                conditionalPanel(
                  condition = "input.pt_location == ''",
                  tags$div(
                    "Click an area on the map to draw a time series chart showing monthly numbers of transactions for different property types for that location.",
                    class = 'alert alert-info'
                  )
                ),
                tags$p("Number of market transactions for different property types (residential, commercial, farms, etc) by month.")
              ),
              tabPanel(
                "Residential",
                # icon = icon("home"),
                conditionalPanel(
                  condition = "input.pt_location != ''",
                  fluidRow(
                    column(
                      3,
                      bchousing:::ValueBox(
                        value = "fmv_perc_month_res",
                        location = "fmv_perc_loc_res"
                      )
                    ),
                    column(
                      3,
                      bchousing:::ValueBox(
                        value = "n_res_1fam_dwelling",
                        subtitle = "Single-family Homes"
                      )
                    ),
                    column(
                      3,
                      bchousing:::ValueBox(
                        value = "n_res_strata",
                        subtitle = "Strata"
                      )
                    ),
                    column(
                      3,
                      bchousing:::ValueBox(
                        value = "no_other_res",
                        subtitle = "Other transactions"
                      )
                    )
                  ),
                  plotly::plotlyOutput("pt_mothly_res", height = chartHeight) %>% shinycssloaders::withSpinner(color="#0dc5c1")
                ),
                conditionalPanel(
                  condition = "input.pt_location == ''",
                  tags$div(
                    "Click an area on the map to draw a time series chart showing monthly number of transactions for residential properties for that location.",
                    class = 'alert alert-info'
                  )
                ),
                tags$p("Number of market transactions for residential properties by month.")
              ),
              tabPanel(
                "Commercial",
                # icon = icon("building"),
                conditionalPanel(
                  condition = "input.pt_location != ''",
                  plotly::plotlyOutput("pt_mothly_comm", height = chartHeight) %>% shinycssloaders::withSpinner(color="#0dc5c1")
                ),
                conditionalPanel(
                  condition = "input.pt_location == ''",
                  tags$div(
                    "Click an area on the map to draw a time series chart showing monthly number of transactions for commerction properties for that location.",
                    class = 'alert alert-info'
                  )
                ),
                tags$p("Number of market transactions for commercial properties by month.")
              )
            )
          )
        )
      )
    ),

    # 04. About ----
    navbarMenu(
      "About",
      tabPanel(
        "Project background",
        fluidRow(
          column(6, shiny::includeMarkdown("inst/app/about/project_background.Rmd"))
        )
      ),
      tabPanel(
        "Data sources",
        fluidRow(
          column(6, shiny::includeMarkdown("inst/app/about/data_sources.Rmd"))
        )
      ),
      tabPanel(
        "Code repository",
        fluidRow(
          column(6, shiny::includeMarkdown("inst/app/about/code_repository.Rmd"))
        )
      ),
      tabPanel(
        "Glossary",
        fluidRow(
          column(6, shiny::includeMarkdown("inst/app/about/glossary.Rmd"))
        )
      )
    ),

    #
    #tabPanel("Feedback", id = "feedback"),
    footer = HTML('
<div id="footer">
  <div id="footerWrapper">
    <div id="footerAdminSection">
      <div id="footerAdminLinksContainer" class="container">
        <div id="footerAdminLinks" class="row">
          <ul class="inline">
            <li data-order="0">
              <a href="/" target="_self">Home</a>
            </li>
            <li data-order="1">
              <a href="#tab-3898-1" target="_self" class="about-project">About</a>
            </li>
            <li data-order="2">
              <a href="//www2.gov.bc.ca/gov/content/home/disclaimer" target="_self">Disclaimer</a>
            </li>
            <li data-order="3">
              <a href="//www2.gov.bc.ca/gov/content/home/privacy" target="_self">Privacy</a>
            </li>
            <li data-order="4">
              <a href="//www2.gov.bc.ca/gov/content/home/accessibility" target="_self">Accessibility</a>
            </li>
            <li data-order="5">
              <a href="//www2.gov.bc.ca/gov/content/home/copyright" target="_self">Copyright</a>
            </li>
            <li data-order="6">
              <a href="//www2.gov.bc.ca/gov/content/home/contact-us" target="_self">Contact Us</a>
            </li>
          </ul>
        </div>
      </div>
    </div>
  </div>
</div>'),

    golem_add_external_resources()
  )


}

#' @import shiny
golem_add_external_resources <- function(){

  addResourcePath(
    'www', system.file('app/www', package = 'bchousing')
  )

  addResourcePath(
    'about', system.file('app/about', package = 'bchousing')
  )

  tags$head(

    # golem::activate_js(),
    # golem::favicon(),
    # Add here all the external resources
    # If you have a custom.css in the inst/app/www
    # Or for example, you can add shinyalert::useShinyalert() here

    tags$link(rel = "stylesheet", type = "text/css", href = "www/css/bcgov.css"),
    shinyjs::useShinyjs(),
    tags$script(src = "www/srcjs/bcgov.js"),
    tags$head(tags$link(rel = "shortcut icon", href = "www/images/favicon.ico"))
  )
}

