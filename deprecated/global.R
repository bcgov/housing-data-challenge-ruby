data("ptt_prov_dash", package = "bchousing")
data("ptt_dr_sf", package = "bchousing")
data("ptt_rd_sf", package = "bchousing")
data("ptt_mun_sf", package = "bchousing")

# Chart formatting
fontFamily <- "Myriad-Pro, Calibri, Arial, 'sans serif'"

tickfontBl <- list(family = fontFamily,
                   size = 12,
                   color = "black")

tickfontRd = list(family = fontFamily,
                  size = 12,
                  color = "#C40C0C")

axisFormat <- list(title = "",
                   showticklabels = TRUE,
                   # tickangle = 45,
                   tickfont = tickfontBl)

# save(fontFamily, tickfontBl, tickfontRd, file = paste0("data", "/", "font_family.rda"))

legendFormat <- list(
  font = list(
    family = fontFamily,
    size = 11,
    color = "#696969"
  ),
  bordercolor = "#e6e6e6",
  borderwidth = 1,
  bgcolor = "rgba(255, 255, 255, 0.5)",
  orientation = 'h',
  # xanchor = "center",  # use center of legend as anchor
  x = 0
)

marginFormat <- list(
  l = 50,
  r = 50,
  b = 100,
  t = 50,
  pad = 4
)

marginFormatMonthly <- list(
  l = 150,
  r = 50,
  b = 50,
  t = 50,
  pad = 4
)

# color schema
colResidential <- "#80b1d3"
colSingleFam <- "#4292c6"
colMultiFam <- "#9ecae1"
colStrata <- "#abdda4"
colNonStrataRental <- "#c7eae5"
colCommercial <- "#fdae61"
colRecreational <- "#80cdc1"
colFarms <- "#fee08b"
colUnknown <- "#d9d9d9"
colAcreage <- "#dfc27d"
colC16 <- colCanadian <- "#c40c0c"
colC11 <- colForeign <- "#3eb4f0"

# Read data objects ----

# boundaries shapefiles
bcCensusDivs <- readRDS("./data/bc2011CensusDivisions.rds")
usethis::use_data(bcCensusDivs, overwrite = TRUE)
bcCensusEconRegs <- readRDS("./data/bc2011EconomicRegions.rds")
usethis::use_data(bcCensusEconRegs, overwrite = TRUE)
bcCensusTracts <- readRDS("./data/bc2011Tracts.rds")
usethis::use_data(bcCensusTracts, overwrite = TRUE)
bcCensusMetroAreas <- readRDS(("./data/bc2011MetropolitanAreas.rds"))
usethis::use_data(bcCensusMetroAreas, overwrite = TRUE)

# census 2016 - used for home page info boxes
c16Prov <- readRDS("./data/census2016-province.rds")
usethis::use_data(c16Prov, overwrite = TRUE)

# Average Age
census2016aaCma <- readr::read_rds(file.path("data", "census2016-avg-age-CMA.rds"))
usethis::use_data(census2016aaCma, overwrite = TRUE)
census2016aaCd <- readr::read_rds(file.path("data", "census2016-avg-age-CD.rds"))
usethis::use_data(census2016aaCd, overwrite = TRUE)
census2016aaCsd <- readr::read_rds(file.path("data", "census2016-avg-age-CSD.rds"))
usethis::use_data(census2016aaCsd, overwrite = TRUE)
census2016aaCt <- readr::read_rds(file.path("data", "census2016-avg-age-CT.rds"))
usethis::use_data(census2016aaCt, overwrite = TRUE)
# census2016aaDa <- readr::read_rds(file.path("data", "census2016-avg-age-DA.rds"))


# Not used
# census2016ppPr <- readr::read_rds(file.path("data", "population_pyramid", "census2016-pp-PR.rds"))
# census2011ppPr <- readr::read_rds(file.path("data", "population_pyramid", "census2011-pp-PR.rds"))
# census2006ppPr <- readr::read_rds(file.path("data", "population_pyramid", "census2006-pp-PR.rds"))
# censusPpPr <- GetJoinedPp(census2016ppPr, census2011ppPr, census2006ppPr)

census2016ppCma <- readr::read_rds(file.path("data", "population_pyramid", "census2016-pp-CMA.rds"))
usethis::use_data(census2016ppCma, overwrite = TRUE)
census2011ppCma <- readr::read_rds(file.path("data", "population_pyramid", "census2011-pp-CMA.rds"))
usethis::use_data(census2011ppCma, overwrite = TRUE)
census2006ppCma <- readr::read_rds(file.path("data", "population_pyramid", "census2006-pp-CMA.rds"))
usethis::use_data(census2006ppCma, overwrite = TRUE)
censusPpCma <- GetJoinedPp(census2016ppCma, census2011ppCma, census2006ppCma)
usethis::use_data(censusPpCma, overwrite = TRUE)

census2016ppCd <- readr::read_rds(file.path("data", "population_pyramid", "census2016-pp-CD.rds"))
usethis::use_data(census2016ppCd, overwrite = TRUE)
census2011ppCd <- readr::read_rds(file.path("data", "population_pyramid", "census2011-pp-CD.rds"))
usethis::use_data(census2011ppCd, overwrite = TRUE)
census2006ppCd <- readr::read_rds(file.path("data", "population_pyramid", "census2006-pp-CD.rds"))
usethis::use_data(census2006ppCd, overwrite = TRUE)
censusPpCd <- GetJoinedPp(census2016ppCd, census2011ppCd, census2006ppCd)
usethis::use_data(censusPpCd, overwrite = TRUE)

census2016ppCsd <- readr::read_rds(file.path("data", "population_pyramid", "census2016-pp-CSD.rds"))
usethis::use_data(census2016ppCsd, overwrite = TRUE)
census2011ppCsd <- readr::read_rds(file.path("data", "population_pyramid", "census2011-pp-CSD.rds"))
usethis::use_data(census2011ppCsd, overwrite = TRUE)
census2006ppCsd <- readr::read_rds(file.path("data", "population_pyramid", "census2006-pp-CSD.rds"))
usethis::use_data(census2006ppCsd, overwrite = TRUE)
censusPpCsd <- GetJoinedPp(census2016ppCsd, census2011ppCsd, census2006ppCsd)
usethis::use_data(censusPpCsd, overwrite = TRUE)

census2016ppCt <- readr::read_rds(file.path("data", "population_pyramid", "census2016-pp-CT.rds"))
usethis::use_data(census2016ppCt, overwrite = TRUE)
census2011ppCt <- readr::read_rds(file.path("data", "population_pyramid", "census2011-pp-CT.rds"))
usethis::use_data(census2011ppCt, overwrite = TRUE)
census2006ppCt <- readr::read_rds(file.path("data", "population_pyramid", "census2006-pp-CT.rds"))
usethis::use_data(census2006ppCt, overwrite = TRUE)
censusPpCt <- GetJoinedPp(census2016ppCt, census2011ppCt, census2006ppCt)
usethis::use_data(censusPpCt, overwrite = TRUE)

# census2016ppDa <- readr::read_rds(file.path("data", "population_pyramid", "census2016-pp-DA.rds"))
# census2006ppDa <- readr::read_rds(file.path("data", "population_pyramid", "census2006-pp-DA.rds"))
# census2011ppDa <- readr::read_rds(file.path("data", "population_pyramid", "census2011-pp-DA.rds"))
# censusPpDa <- GetJoinedPp(census2016ppDa, census2011ppDa, census2006ppDa)

# Mobility
censusMobilityCma <- readr::read_rds(file.path("data", "census2016-mobility-CMA.rds"))
usethis::use_data(censusMobilityCma, overwrite = TRUE)
censusMobilityCd <- readr::read_rds(file.path("data", "census2016-mobility-CD.rds"))
usethis::use_data(censusMobilityCd, overwrite = TRUE)
censusMobilityCsd <- readr::read_rds(file.path("data", "census2016-mobility-CSD.rds"))
usethis::use_data(censusMobilityCsd, overwrite = TRUE)
censusMobilityCt <- readr::read_rds(file.path("data", "census2016-mobility-CT.rds"))
usethis::use_data(censusMobilityCt, overwrite = TRUE)
# censusMobilityDa <- readr::read_rds(file.path("data", "census2016-mobility-DA.rds"))
censusMobilityCmaGathered <- readr::read_rds(file.path("data", "census2016-mobility-CMA-gathered.rds"))
usethis::use_data(censusMobilityCmaGathered, overwrite = TRUE)
censusMobilityCdGathered <- readr::read_rds(file.path("data", "census2016-mobility-CD-gathered.rds"))
usethis::use_data(censusMobilityCdGathered, overwrite = TRUE)
censusMobilityCsdGathered <- readr::read_rds(file.path("data", "census2016-mobility-CSD-gathered.rds"))
usethis::use_data(censusMobilityCsdGathered, overwrite = TRUE)
censusMobilityCtGathered <- readr::read_rds(file.path("data", "census2016-mobility-CT-gathered.rds"))
usethis::use_data(censusMobilityCtGathered, overwrite = TRUE)
# censusMobilityDa <- readr::read_rds(file.path("data", "census2016-mobility-DA.rds"))
censusMobilityCmaSeq <- readr::read_rds(file.path("data", "census2016-mobility-CMA-seq.rds"))
usethis::use_data(censusMobilityCmaSeq, overwrite = TRUE)
censusMobilityCdSeq <- readr::read_rds(file.path("data", "census2016-mobility-CD-seq.rds"))
usethis::use_data(censusMobilityCdSeq, overwrite = TRUE)
censusMobilityCsdSeq <- readr::read_rds(file.path("data", "census2016-mobility-CSD-seq.rds"))
usethis::use_data(censusMobilityCsdSeq, overwrite = TRUE)
censusMobilityCtSeq <- readr::read_rds(file.path("data", "census2016-mobility-CT-seq.rds"))
usethis::use_data(censusMobilityCtSeq, overwrite = TRUE)
# censusMobilityDa <- readr::read_rds(file.path("data", "census2016-mobility-DA.rds"))

# Housing Type
housingTypesCma <- readRDS(file.path("data", "housing", "census2016-housing-CMA.rds")) %>% na.omit()
usethis::use_data(housingTypesCma, overwrite = TRUE)
housingTypesCsd <- readRDS(file.path("data", "housing", "census2016-housing-CSD.rds")) %>% na.omit()
usethis::use_data(housingTypesCsd, overwrite = TRUE)
housingTypesCd <- readRDS(file.path("data", "housing", "census2016-housing-CD.rds")) %>% na.omit()
usethis::use_data(housingTypesCd, overwrite = TRUE)
housingTypesCt <- readRDS(file.path("data", "housing", "census2016-housing-CT.rds")) %>% na.omit()
usethis::use_data(housingTypesCt, overwrite = TRUE)
# housingTypesDa <- readRDS(file.path("data", "housing", "census2016-housing-DA.rds"))

# Shelter-Cost-to-Income Ratio data
census2016CmaStir <- readr::read_rds(file.path("data", "census2016Spatial-stir-CMA.rds"))
usethis::use_data(census2016CmaStir, overwrite = TRUE)
census2016CdStir <- readr::read_rds(file.path("data", "census2016Spatial-stir-CD.rds"))
usethis::use_data(census2016CdStir, overwrite = TRUE)
census2016CsdStir <- readr::read_rds(file.path("data", "census2016Spatial-stir-CSD.rds"))
usethis::use_data(census2016CsdStir, overwrite = TRUE)
census2016CtStir <- readr::read_rds(file.path("data", "census2016Spatial-stir-CT.rds"))
usethis::use_data(census2016CtStir, overwrite = TRUE)
# census2016DaStir <- readr::read_rds(file.path("data", "census2016Spatial-stir-DA.rds"))

# Selection of metrics, variables and options ----
selectionMetrics <- c(
  "Average FMV" = "mn_FMV",
  "Average Foreign FMV" = "mn_FMV_foreign",
  "% of Foreign Transactions" = "no_foreign_perc"
)

selectionMetricsDF <- data.frame(
  value =
    c("no_mkt_trans", "sum_FMV", "sum_PPT_paid", "no_foreign", "no_foreign_perc",
      "sum_FMV_foreign", "add_tax_paid", "mn_FMV", "mn_FMV_foreign"),
  label =
    c("Number of Transactions", "Total FMV", "PTT Paid", "Number of Foreign Transactions",
      "Percentage of Foreign Transactions", "Total FMV of Foreign Transactions",
      "Additional Tax Paid", "Average FMV", "Average Foreign FMV")
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
  "Single detached house ratio",
  "Semi detached house ratio",
  "Apartment in duplex ratio",
  "Row house ratio",
  "Apartment in small building ratio",
  "Apartment in tall building ratio",
  "Other single attached house ratio",
  "Movable dwelling ratio"
)

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
jumbotron <- function(header, popPerc = 0, popInc = TRUE, dwellPerc = 0, dwellInc = TRUE,
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
