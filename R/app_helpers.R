#' @title BC Housing home page jumbotron
#'
#' @param header Header
#' @param popPerc Population change percentage
#' @param popInc Population increase
#' @param dwellPerc Dwellings change percentage
#' @param dwellInc Dwellings increase
#' @param trans_period Transaction period
#' @param no_mkt_trans Number of transactions
#' @param no_foreign_perc Foreign transactions percentage
#' @param sum_FMV Total Fair Market Value
#' @param sum_FMV_foreign_perc Percentage of foreign Fair Market Value
#'
#' @return HTML Code
#' @export
#'
#' @examples
Jumbotron <- function(header, popPerc = 0, popInc = TRUE, dwellPerc = 0, dwellInc = TRUE,
                      trans_period, no_mkt_trans = 0, no_foreign_perc = 0,
                      sum_FMV = 0, sum_FMV_foreign_perc = 0) {

  maxTransPeriod <- ptt_prov_dash$max_trans_period

  htSummary <- as_tibble(housingTypesCd) %>%
    mutate("PRUID" = "59") %>%
    group_by(`PRUID`) %>%
    summarise(
      "Single detached house" = sum(`Single detached house`),
      "Apartment in tall building" = sum(`Apartment in tall building`),
      "Semi detached house" = sum(`Semi detached house`),
      "Row house" = sum(`Row house`),
      "Apartment in duplex" = sum(`Apartment in duplex`),
      "Apartment in small building" = sum(`Apartment in small building`),
      "Other single attached house" = sum(`Other single attached house`),
      "Movable dwelling" = sum(`Movable dwelling`)
    ) %>%
    mutate("Single detached house ratio" = round(`Single detached house` * 100 / (
      `Single detached house` +
        `Apartment in tall building` +
        `Semi detached house` +
        `Row house` +
        `Apartment in duplex` +
        `Apartment in small building` +
        `Other single attached house` +
        `Movable dwelling`
    ), digits = 2)) %>%
    select("Single detached house ratio")

  mSummary <- as_tibble(censusMobilityCsd) %>%
    mutate("PRUID" = "59") %>%
    group_by(`PRUID`, `Region`) %>%
    summarise("Movers Ratio" = max(`Movers Ratio`)) %>%
    ungroup() %>%
    arrange(desc(`Movers Ratio`)) %>%
    top_n(1) %>%
    select(`Region`, `Movers Ratio`)

  ageSummary <- as_tibble(census2016aaCsd) %>%
    mutate("PRUID" = "59") %>%
    group_by(`PRUID`, `Region`) %>%
    summarise("Average Age" = max(`Average Age`)) %>%
    filter(!str_detect(`Region`, 'IRI')) %>%
    ungroup() %>%
    arrange(`Average Age`) %>%
    top_n(1) %>%
    select(`Region`, `Average Age`)

  stirSummary <- as_tibble(census2016CsdStir) %>%
    mutate("PRUID" = "59") %>%
    group_by(`PRUID`, `Region`) %>%
    summarise("percent_more_than_30" = max(`percent_more_than_30`)) %>%
    filter(!str_detect(`Region`, 'IRI')) %>%
    ungroup() %>%
    arrange(`percent_more_than_30`) %>%
    top_n(1) %>%
    select(`Region`, `percent_more_than_30`)

  popChange <- "increased"
  if (popInc == FALSE) {
    popChange <- "decreased"
  }

  dwellChange <- "increased"
  if (dwellInc == FALSE) {
    dwellChange <- "decreased"
  }

  boxHousingType <- paste0("At the province level, <strong>", htSummary$`Single detached house ratio`, "%</strong> of dwellings are
                          <strong>single-family homes</strong>.")
  boxMobility <- paste0(mSummary$Region, " region has the highest ratio of movers in the last year - <strong>",
                        mSummary$`Movers Ratio`, "%</strong>.")
  boxStir <- paste0("In ", stirSummary$Region, ", <strong>", stirSummary$percent_more_than_30, "%</strong> of households
                    spend more than 30% of their income on shelter cost.")
  boxPp <- paste0("Municipality with the highest average age (<strong>", ageSummary$`Average Age`,"</strong>) is ",
                  ageSummary$Region, ".")

  no_mkt_trans <- ptt_prov_dash$tot_mkt_trans
  no_foreign_perc <- ptt_prov_dash$no_foreign_perc
  sum_FMV <- ptt_prov_dash$sum_FMV
  sum_FMV_foreign_perc <- ptt_prov_dash$sum_FMV_foreign_perc

  html_code <- HTML(paste0(
    "<div class=\"jumbotron\">
  <h1> ", header, "</h1>
  <div class=\"container-fluid\">
    <div class=\"row\">
      <div class=\"col-sm-12\">
        <div class=\"row quick-fact-container\">
          <div class=\"col-lg-4 col-lg-offset-1 col-md-5 col-md-offset-1 col-sm-6\">
            <div class=\"quick-fact\">
              <div class=\"splash-text\">
                <h3><i class=\"fa fa-users\"></i>&nbsp;Population</h3>
              </div>
              <div class=\"splash-text\">
                Between 2011 and 2016 censuses, BC&nbsp;population
                has ", popChange ," by <strong>", popPerc , "%</strong>.
              </div>
              <div class=\"splash-text\">",
    boxPp,"
              </div>
              <div class=\"splash-text\">
                <p>
                  <a href=\"#\" class=\"btn btn-bcgov explore-population\">
                    <i class=\"fa fa-users\"></i>&nbsp;Explore population
                  </a>
                </p>
              </div>
            </div>
          </div>
          <div class=\"col-lg-4 col-lg-offset-2 col-md-5 col-sm-6\">
            <div class=\"quick-fact\">
              <div class=\"splash-text\">
                <h3><i class=\"fa fa-money\"></i>&nbsp;Property Sales</h3>
              </div>
              <div class=\"splash-text\">
                In ", paste(lubridate::month(ymd(maxTransPeriod), label = TRUE, abbr = FALSE),
                            lubridate::year(maxTransPeriod)), ", there were <strong>",
    format(no_mkt_trans, big.mark=","),
    "</strong> housing market transactions, <strong>",
    format(no_foreign_perc, big.mark=","),
    "%</strong> of which involved foreign citizens.
              </div>
              <div class=\"splash-text\">
                The volume of these transactions was <strong>",
    paste("$", format(sum_FMV, big.mark=","), sep="") ,
    "</strong> (<strong>", sum_FMV_foreign_perc , "%</strong> foreign).
              </div>
              <div class=\"splash-text\">
                <a href=\"#\" class=\"btn btn-bcgov explore-ptt\">
                  <i class=\"fa fa-money\"></i>&nbsp;Explore property sales
                </a>
              </div>
            </div>
          </div>
        </div>
      </div>
    </div>

    <div class=\"row\">
      <div class=\"col-sm-12\">
        <div class=\"row quick-fact-container\">
          <div class=\"col-lg-4 col-lg-offset-1 col-md-5 col-md-offset-1 col-sm-6\">
            <div class=\"quick-fact\">
              <div class=\"splash-text\">
                <h3><i class=\"fa fa-home\"></i>&nbsp;Housing</h3>
              </div>
              <div class=\"splash-text\">
                At the same time, number of private
                dwellings has ", dwellChange ," by <strong>", dwellPerc ,
    "%</strong>
              </div>
              <div class=\"splash-text\">",
    boxHousingType,"
              </div>
              <div class=\"splash-text\">
                <p>
                  <a href=\"#\" class=\"btn btn-bcgov explore-housing\">
                    <i class=\"fa fa-home\"></i>&nbsp;Explore housing types
                  </a>
                </p>
              </div>
            </div>
          </div>
          <div class=\"col-lg-4 col-lg-offset-2 col-md-5 col-sm-6\">
            <div class=\"quick-fact\">
              <div class=\"splash-text\">
                <h3><i class=\"fa fa-truck\"></i>&nbsp;Shelter Cost and Mobility</h3>
              </div>
              <div class=\"splash-text\">",
    boxStir,"
              </div>
              <div class=\"splash-text\">
                <a href=\"#\" class=\"btn btn-bcgov explore-stir\">
                  <i class=\"fa fa-bullhorn\"></i>&nbsp;Explore shelter cost
                </a>
              </div>
              <div class=\"splash-text\">",
    boxMobility,"
              </div>
              <div class=\"splash-text\">
                <a href=\"#\" class=\"btn btn-bcgov explore-mobility\">
                  <i class=\"fa fa-truck\"></i>&nbsp;Explore mobility
                </a>
              </div>
            </div>
          </div>
        </div>
      </div>
    </div>

  </div>
</div>") )

  return(html_code)
}

#' @title Population Pyramid data
#'
#' @param c16 Census 2016 data
#' @param c11 Census 2011 data
#' @param c06 Census 2006 data
#'
#' @return Census population pyramid data
#' @export
#'
#' @examples
GetJoinedPp <- function(c16, c11, c06) {
  censusPp <-
    dplyr::left_join(c16, c11, by = c("GeoUID", "Region", "sex", "age")) %>%
    rename(
      "percentage_2016" = "percentage.x",
      "percentage_2011" = "percentage.y"
    ) %>%
    left_join(c06, by = c("GeoUID", "Region", "sex", "age")) %>%
    rename(
      "percentage_2006" = "percentage"
    )
  return(censusPp)
}

#' @title Format title for plotly charts
#'
#' @param title_text Chart title text
#' @param x Horizontal alignment
#' @param font_family Font family
#' @param font_color Font color
#' @param font_size Font size
#'
#' @return Formatted chart title
#' @export
#'
#' @examples
PlotlyChartTitle <- function(title_text = "Chart title", x = 0, font_family = "Arial", font_color = "#393939", font_size = 16) {
  plotly_title <- list(
    text = title_text,
    x = 0,
    font = list(
      family = font_family,
      color = font_color,
      size = font_size
    )
  )
  return(plotly_title)
}

#' Add annotation for Plotly charts
#'
#' @param annotation_text Annotation text
#'
#' @return Formatted annotation
#' @export
#'
#' @examples
PlotlyChartAnnotation <- function(annotation_text) {
  annotations = list(
    text = annotation_text,
    font = list(size = 12, color = '#393939'),
    showarrow = FALSE,
    xref = 'paper', x = -0.0075,
    yref = 'paper', y = 1.075,
    xanchor = 'left',
    yanchor = 'auto',
    xshift = 0.5,
    yshift = 0.5
  )

  return(annotations)
}

