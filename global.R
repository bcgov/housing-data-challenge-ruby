# devtools::install_github('andrewsali/shinycssloaders')
library(here)
library(shiny)
library(shinyjs)
# library(rgdal)
# library(rgeos)
# library(maps)
library(sf)
# library(rmapshaper)
library(leaflet)
library(readr)
library(stringr)
library(magrittr)
library(lubridate)
library(dplyr)
library(htmlwidgets)
library(DT)
library(tidyr)
# library(crosstalk)
library(plotly)
# library(cancensus)
# library(sankeyD3)
library(sunburstR)
library(treemap)
# library(data.tree)
# library(d3treeR)
# library(RColorBrewer)
library(shinycssloaders)
library(shinyBS)

# source files
source("modules/controls.R")
source("modules/chartFormat.R")
# source("get_geography.R")

options(stringsAsFactors = F)
Sys.setenv(TZ = "America/Vancouver")

# Read objects
# Property transfer tax
ptRegDisMth <- readRDS("./data/pt-regional-district-monthly.rds")
ptMunMth <- readRDS("./data/pt-municipal-monthly.rds")
ptDevRegMth <- readRDS("./data/pt-development-region-monthly.rds")
ptProvMth <- readRDS("./data/pt-provincial-monthly.rds")

# boundaries shapefiles
bcCensusDivs <- readRDS("./data/bc2011CensusDivisions.rds")
bcCensusEconRegs <- readRDS("./data/bc2011EconomicRegions.rds")
bcCensusTracts <- readRDS("./data/bc2011Tracts.rds")
bcCensusMetroAreas <- readRDS(("./data/bc2011MetropolitanAreas.rds"))

# census 2016
c16Divs <- readRDS("./data/census2016-divisions.rds")
c16EconRegs <- readRDS("./data/census2016-economic-regions.rds")
c16Tracts <- readRDS("./data/census2016-tracts.rds")
c16MetroAreas <- readRDS("./data/census2016-metro-areas.rds")
c16Prov <- readRDS("./data/census2016-province.rds")

# Average Age
census2016aaCma <- read_rds(file.path("data", "census2016-avg-age-CMA.rds"))
census2016aaCd <- read_rds(file.path("data", "census2016-avg-age-CD.rds"))
census2016aaCsd <- read_rds(file.path("data", "census2016-avg-age-CSD.rds"))
census2016aaCt <- read_rds(file.path("data", "census2016-avg-age-CT.rds"))
# census2016aaDa <- read_rds(file.path("data", "census2016-avg-age-DA.rds"))

# Population Pyramid data
getJoinedPp <- function(c16, c11, c06) {
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

# census2016ppPr <- read_rds(file.path("data", "population_pyramid", "census2016-pp-PR.rds"))
# census2011ppPr <- read_rds(file.path("data", "population_pyramid", "census2011-pp-PR.rds"))
# census2006ppPr <- read_rds(file.path("data", "population_pyramid", "census2006-pp-PR.rds"))
# censusPpPr <- getJoinedPp(census2016ppPr, census2011ppPr, census2006ppPr)

census2016ppCma <- read_rds(file.path("data", "population_pyramid", "census2016-pp-CMA.rds"))
census2011ppCma <- read_rds(file.path("data", "population_pyramid", "census2011-pp-CMA.rds"))
census2006ppCma <- read_rds(file.path("data", "population_pyramid", "census2006-pp-CMA.rds"))
censusPpCma <- getJoinedPp(census2016ppCma, census2011ppCma, census2006ppCma)

census2016ppCd <- read_rds(file.path("data", "population_pyramid", "census2016-pp-CD.rds"))
census2011ppCd <- read_rds(file.path("data", "population_pyramid", "census2011-pp-CD.rds"))
census2006ppCd <- read_rds(file.path("data", "population_pyramid", "census2006-pp-CD.rds"))
censusPpCd <- getJoinedPp(census2016ppCd, census2011ppCd, census2006ppCd)

census2016ppCsd <- read_rds(file.path("data", "population_pyramid", "census2016-pp-CSD.rds"))
census2011ppCsd <- read_rds(file.path("data", "population_pyramid", "census2011-pp-CSD.rds"))
census2006ppCsd <- read_rds(file.path("data", "population_pyramid", "census2006-pp-CSD.rds"))
censusPpCsd <- getJoinedPp(census2016ppCsd, census2011ppCsd, census2006ppCsd)

census2016ppCt <- read_rds(file.path("data", "population_pyramid", "census2016-pp-CT.rds"))
census2011ppCt <- read_rds(file.path("data", "population_pyramid", "census2011-pp-CT.rds"))
census2006ppCt <- read_rds(file.path("data", "population_pyramid", "census2006-pp-CT.rds"))
censusPpCt <- getJoinedPp(census2016ppCt, census2011ppCt, census2006ppCt)

# census2016ppDa <- read_rds(file.path("data", "population_pyramid", "census2016-pp-DA.rds"))
# census2006ppDa <- read_rds(file.path("data", "population_pyramid", "census2006-pp-DA.rds"))
# census2011ppDa <- read_rds(file.path("data", "population_pyramid", "census2011-pp-DA.rds"))
# censusPpDa <- getJoinedPp(census2016ppDa, census2011ppDa, census2006ppDa)

# Mobility
censusMobilityCma <- read_rds(file.path("data", "census2016-mobility-CMA.rds"))
censusMobilityCd <- read_rds(file.path("data", "census2016-mobility-CD.rds"))
censusMobilityCsd <- read_rds(file.path("data", "census2016-mobility-CSD.rds"))
censusMobilityCt <- read_rds(file.path("data", "census2016-mobility-CT.rds"))
# censusMobilityDa <- read_rds(file.path("data", "census2016-mobility-DA.rds"))
censusMobilityCmaGathered <- read_rds(file.path("data", "census2016-mobility-CMA-gathered.rds"))
censusMobilityCdGathered <- read_rds(file.path("data", "census2016-mobility-CD-gathered.rds"))
censusMobilityCsdGathered <- read_rds(file.path("data", "census2016-mobility-CSD-gathered.rds"))
censusMobilityCtGathered <- read_rds(file.path("data", "census2016-mobility-CT-gathered.rds"))
# censusMobilityDa <- read_rds(file.path("data", "census2016-mobility-DA.rds"))
censusMobilityCmaSeq <- read_rds(file.path("data", "census2016-mobility-CMA-seq.rds"))
censusMobilityCdSeq <- read_rds(file.path("data", "census2016-mobility-CD-seq.rds"))
censusMobilityCsdSeq <- read_rds(file.path("data", "census2016-mobility-CSD-seq.rds"))
censusMobilityCtSeq <- read_rds(file.path("data", "census2016-mobility-CT-seq.rds"))
# censusMobilityDa <- read_rds(file.path("data", "census2016-mobility-DA.rds"))

# Housing Type
housingTypesCma <- readRDS(file.path("data", "housing", "census2016-housing-CMA.rds"))
housingTypesCsd <- readRDS(file.path("data", "housing", "census2016-housing-CSD.rds"))
housingTypesCd <- readRDS(file.path("data", "housing", "census2016-housing-CD.rds"))
housingTypesCt <- readRDS(file.path("data", "housing", "census2016-housing-CT.rds"))
# housingTypesDa <- readRDS(file.path("data", "housing", "census2016-housing-DA.rds"))

# Shelter-Cost-to-Income Ratio data
census2016CmaStir <- read_rds(file.path("data", "census2016Spatial-stir-CMA.rds"))
census2016CdStir <- read_rds(file.path("data", "census2016Spatial-stir-CD.rds"))
census2016CsdStir <- read_rds(file.path("data", "census2016Spatial-stir-CSD.rds"))
census2016CtStir <- read_rds(file.path("data", "census2016Spatial-stir-CT.rds"))
# census2016DaStir <- read_rds(file.path("data", "census2016Spatial-stir-DA.rds"))

# options(cancensus.api_key = "CensusMapper_f17c13c7fc5e60de7cdd341d5d4db866")
# dir.create('./cache', showWarnings = TRUE, recursive = FALSE, mode = "0777")
# Sys.chmod(list.dirs("."), "777")
# options(cancensus.cache_path = "./cache/")

#  Cleanup - @TODO Move this to getdata.R
# c16EconRegs$ERNAME <-
#   gsub("LOWER MAINLAND--SOUTHWEST", "MAINLAND/SOUTHWEST", c16EconRegs$ERNAME)
# c16EconRegs$ERNAME <-
#   gsub("THOMPSON--OKANAGAN", "THOMPSON/OKANAGAN", c16EconRegs$ERNAME)
# c16EconRegs$ERNAME <-
#   gsub("VANCOUVER ISLAND AND COAST", "VANCOUVER ISLAND/COAST", c16EconRegs$ERNAME)
# c16EconRegs$Total.Private.Dwellings.2011 <- 0

ptDevRegMth$DevelopmentRegion <-
  gsub("MAINLAND/SOUTHWEST", "LOWER MAINLAND/SOUTHWEST", ptDevRegMth$DevelopmentRegion)
ptDevRegMth$DevelopmentRegion <-
  gsub("VANCOUVER ISLAND/COAST", "VANCOUVER ISLAND AND COAST", ptDevRegMth$DevelopmentRegion)
ptDevRegMth$Total.Private.Dwellings.Change <- 0

ptMunMth$Municipality <-
  gsub("ABBOTSFORD", "ABBOTSFORD - MISSION", ptMunMth$Municipality)


# addRatioColumn <- function(df, ratioCol, dividend, divisor, decimals = 3) {
#     df <- df %>%
#         mutate_(ratioCol = round(dividend / divisor, decimals)) %>%
#         filter(!is.na(dividend)) %>%
#         filter(!is.na(divisor))
#     return(df)
# }

# Add Percentage of Foreign Transactions column
ptRegDisMth <- ptRegDisMth %>%
  mutate(no_foreign_perc = round(no_foreign / no_mkt_trans, 4) * 100) %>%
  mutate(sum_FMV_foreign_perc = round(sum_FMV_foreign / sum_FMV, 4) * 100)

ptDevRegMth <- ptDevRegMth %>%
  mutate(no_foreign_perc = round(no_foreign / no_mkt_trans, 4) * 100) %>%
  mutate(sum_FMV_foreign_perc = round(sum_FMV_foreign / sum_FMV, 4) * 100)

ptMunMth <- ptMunMth %>%
  mutate(no_foreign_perc = round(no_foreign / no_mkt_trans, 4) * 100) %>%
  mutate(sum_FMV_foreign_perc = round(sum_FMV_foreign / sum_FMV, 4) * 100)

ptProvMth <- ptProvMth %>%
  mutate(no_foreign_perc = round(no_foreign / no_mkt_trans, 4) * 100) %>%
  mutate(sum_FMV_foreign_perc = round(sum_FMV_foreign / sum_FMV, 4) * 100)

allMetrics <- c("Total Market Transactions #" = "no_mkt_trans",
                "Res. Total #" = "no_resid_trans",
                # "Res. - Acreage #" = "no_resid_acreage_trans",
                # "Res. - Commerce #" = "resid_comm_count",
                # "Res. - Farm #" = "no_resid_farm",
                "Res. - Multi-family #" = "no_resid_fam",
                "Res. - Single-family Res. #" = "no_res_1fam",
                "Res. - Strata Res. #" = "no_resid_strata",
                # "Res. - Strata Non- Res. or Rental #" = "no_resid_non_strata",
                # "Res. - Other #" = "no_resid_other",
                "Comm. Total #" = "no_comm_tot",
                # "Comm. - Comm. #" = "no_comm_comm",
                # "Comm. - Strata Non-Res. #" = "no_comm_strata_nores",
                # "Comm. - Other #" = "no_comm_other",
                # "Recr. Total #" = "no_recr_tot",
                # "Farm Total #" = "no_farm_tot",
                # "Other/Unknown Total #" = "no_unkn_tot",
                "FMV Sum" = "sum_FMV",
                "FMV Average" = "mn_FMV",
                # "FMV Median" = "md_FMV",
                "PTT Paid" = "sum_PPT_paid",
                # "PTT Paid Median" = "md_PPT",
                "Foreign Transactions #" = "no_foreign",
                # "Foreign Involvement transactions - Res. #" = "no_foreign_res",
                # "Foreign Involvement transactions - Comm. #" = "no_foreign_comm",
                # "Foreign Involvement transactions - Other #" = "no_foreign_comm",
                "FMV sum of Foreign Transactions" = "sum_FMV_foreign",
                "FMV Mean of Foreign Transactions" = "mn_FMV_foreign",
                # "FMV Median of Foreign Transactions" = "md_FMV_foreign",
                # "Under $1 million (count, foreign involvement transactions)" = "no_lt1M_foreign",
                # "$1 million - $3 million (count, foreign involvement transactions)" = "no_gt1M_foreign",
                # "Over $3 million (count, foreign involvement transactions)" = "no_gt3M_foreign",
                "Additional Tax Paid" = "add_tax_paid",
                "Foreign Transactions %" = "no_foreign_perc",
                "FMV Sum of Foreign %" = "sum_FMV_foreign_perc"
)

# Selection of metrics
selectionMetrics <- c("Transactions #" = "no_mkt_trans",
                      "FMV Sum" = "sum_FMV",
                      "PTT Paid" = "sum_PPT_paid",
                      "Foreign Transactions #" = "no_foreign",
                      "FMV Sum of Foreign Transactions" = "sum_FMV_foreign",
                      "Additional Tax Paid" = "add_tax_paid")

selectionMetricsDF <- data.frame(
  Metric =
    c("no_mkt_trans", "sum_FMV", "sum_PPT_paid", "no_foreign",
      "sum_FMV_foreign", "add_tax_paid"),
  MetricName =
    c("Transactions #", "FMV Sum", "PTT Paid", "Foreign Transactions #",
      "FMV Sum of Foreign Transactions", "Additional Tax Paid")
)

maxTransPeriod <- max(ptProvMth$trans_period)
propertyTax <- ptRegDisMth
chartHeight <- 600
mapHeight <- 600

pt_view <- 'devreg'
pt_trans_period <- '2016-12-01'
pt_metric <- 'no_mkt_trans'

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
  "Appartment in duplex ratio",
  "Row house ratio",
  "Appartment in small building ratio",
  "Appartment in tall building ratio",
  "Other single attached house ratio",
  "Movable dwelling ratio"
)

propertyTaxPeriod <- ptRegDisMth %>%
  filter(trans_period %in% maxTransPeriod)

propertyTaxPeriod$geoUnit <- propertyTaxPeriod$Regional.District

# For use in overview charts
propertyTax <- ptRegDisMth
propertyTax$geoUnit <- ptRegDisMth$Regional.District

# Convert join columns to uppercase to avoid mismatches due to case sensitivity
bcCensusDivs@data$CDNAME <- toupper(bcCensusDivs@data$CDNAME)
geoUnit <- as.character(bcCensusDivs$CDNAME)
byY <- "Regional.District"
shapesDF <-
  merge(
    bcCensusDivs,
    propertyTaxPeriod,
    by.x = "CDNAME",
    by.y = "Regional.District",
    sort = FALSE,
    by = ALL
  )

# Color palette
pal <-
  colorQuantile("YlGnBu", n = 9, as.integer(shapesDF$no_mkt_trans))
data <- shapesDF@data

# viridis
getPal <- function(pal, dom, bins) {
  colorBin(palette = pal,
           domain = dom,
           bins = bins)
}

# palViridis <-
#   getPal(
#     viridis::viridis(4),
#     censusData$v_CA16_2447,
#     c(0, 30000, 60000, 75000, 90000, 100000, 120000)
#   )

htSummary <- as_tibble(housingTypesCd) %>%
  mutate("PRUID" = "59") %>%
  group_by(`PRUID`) %>%
  summarise(
    "Single detached house" = sum(`Single detached house`),
    "Appartment in tall building" = sum(`Appartment in tall building`),
    "Semi detached house" = sum(`Semi detached house`),
    "Row house" = sum(`Row house`),
    "Appartment in duplex" = sum(`Appartment in duplex`),
    "Appartment in small building" = sum(`Appartment in small building`),
    "Other single attached house" = sum(`Other single attached house`),
    "Movable dwelling" = sum(`Movable dwelling`)
  ) %>%
  mutate("Single detached house ratio" = round(`Single detached house` * 100 / (
    `Single detached house` +
      `Appartment in tall building` +
      `Semi detached house` +
      `Row house` +
      `Appartment in duplex` +
      `Appartment in small building` +
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

# Add a homepage Jumbotron
jumbotron <- function(header, popPerc = 0, popInc = TRUE, dwellPerc = 0, dwellInc = TRUE,
                      trans_period, no_mkt_trans = 0, no_foreign_perc = 0,
                      sum_FMV = 0, sum_FMV_foreign_perc = 0) {

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
  boxMobility <- paste0(mSummary$Region, " census division has the highest ratio of movers in the last year - <strong>",
                       mSummary$`Movers Ratio`, "%</strong>.")
  boxStir <- paste0("In ", stirSummary$Region, ", <strong>", stirSummary$percent_more_than_30, "%</strong> of households
                    spend more than 30% of their income on shelter cost.")
  boxPp <- paste0("Census subdivision with the highest average age (<strong>", ageSummary$`Average Age`,"</strong>) is ",
                 ageSummary$Region, ".")

  HTML(paste0("<div class=\"jumbotron\">
              <h1> ", header, "</h1>
              <div class=\"container-fluid\">
              <div class=\"row\">
              <div class=\"col-sm-5 col-sm-offset-1\">
              <div class=\"quick-fact\">
              <div class=\"splash-icon\">
              <i class=\"fa fa-users\"></i>
              </div>
              <div class=\"splash-text\">
              Between 2011 and 2016 census, BC&nbsp;population
              has ", popChange ," by <strong>", popPerc , "%</strong>.
              </div>
              </div>
              </div>
              <div class=\"col-sm-5\">
              <div class=\"quick-fact\">
              <div class=\"splash-icon\">
              <i class=\"fa fa-building\"></i>
              </div>
              <div class=\"splash-text\">
              At the same time, number of private
              dwellings has ", dwellChange ," by <strong>", dwellPerc ,
              "%</strong>.
              </div>
              </div>
              </div>
              </div>
              <div class=\"row\">
              <div class=\"col-sm-5 col-sm-offset-1\">
              <div class=\"quick-fact\">
              <div class=\"splash-icon\">
              <i class=\"fa fa-briefcase\"></i>
              </div>
              <div class=\"splash-text\">
              In ", paste(month(ymd(maxTransPeriod), label = TRUE, abbr = FALSE),
              year(maxTransPeriod)), ", there were <strong>",
              format(no_mkt_trans, big.mark=","),
              "</strong> housing market transactions, <strong>",
              format(no_foreign_perc, big.mark=","),
              "%</strong> of which involved foreign citizens.
              </div>
              </div>
              </div>
              <div class=\"col-sm-5\">
              <div class=\"quick-fact\">
              <div class=\"splash-icon\">
              <i class=\"fa fa-money\"></i>
              </div>
              <div class=\"splash-text\">
              The volume of these transactions was <strong>",
              paste("$", format(sum_FMV, big.mark=","), sep="") ,
              "</strong> (<strong>", sum_FMV_foreign_perc , "%</strong> foreign).
              </div>
              </div>
              </div>
              </div>

              <div class=\"row\">
              <div class=\"col-sm-5 col-sm-offset-1\">
              <div class=\"quick-fact\">
              <div class=\"splash-icon\">
              <i class=\"fa fa-home\"></i>
              </div>
              <div class=\"splash-text\">",
              boxHousingType ,"
              </div>
              </div>
              </div>
              <div class=\"col-sm-5\">
              <div class=\"quick-fact\">
              <div class=\"splash-icon\">
              <i class=\"fa fa-truck\"></i>
              </div>
              <div class=\"splash-text\">",
              boxMobility,
              "</div>
              </div>
              </div>
              </div>

              <div class=\"row\">
              <div class=\"col-sm-5 col-sm-offset-1\">
              <div class=\"quick-fact\">
              <div class=\"splash-icon\">
              <i class=\"fa fa-street-view\"></i>
              </div>
              <div class=\"splash-text\">",
              boxPp,
              "</div>
              </div>
              </div>
              <div class=\"col-sm-5\">
              <div class=\"quick-fact\">
              <div class=\"splash-icon\">
              <i class=\"fa fa-bullhorn\"></i>
              </div>
              <div class=\"splash-text\">",
              boxStir,
              "</div>
              </div>
              </div>
              </div>

              </div>
              </div>") )
}

# Function to dynamically create plotly charts
plotmy <-
  function(pmData,
           pmxAxis,
           pmyAxis,
           pmName,
           pmType,
           pmMarker,
           pmTitle,
           pmTraces) {
    # configure chart
    plotme <- plot_ly(
      pmData %>% arrange(desc(pmyAxis)),
      x = pmxAxis,
      y = pmyAxis,
      name = pmName,
      type = pmType,
      marker = list(color = pmMarker)#,
      # titlefont = list(size = 12)
    ) %>%
      layout(
        title = pmTitle,
        xaxis = axisFormat,
        yaxis = axisFormat,
        margin = marginFormat,
        legend = legendFormat
      ) %>%
      config(displayModeBar = F)

    # add traces dynamically
    for (pmTrace in pmTraces) {
      plotme <- add_trace(
        plotme,
        y = pmTrace[['y']],
        x = pmTrace[['x']],
        name = pmTrace[['name']],
        marker = list(color = pmTrace[['color']]) #,
        #evaluate = TRUE
      )
    }

    plotme
  }
