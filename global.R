# Detach any lodaded packages ----
suppressPackageStartupMessages(library(attempt))

detach_all <- function() {
  all_attached <- paste(
    "package:",
    names(sessionInfo()$otherPkgs),
    sep = ""
  )
  attempt(
    suppressWarnings(
      lapply(
        all_attached,
        detach,
        character.only = TRUE,
        unload = TRUE
      )
    ),
    silent = TRUE
  )
}

detach_all()

# Load packages ----
library(here)
library(shiny)
library(shinyjs)
library(sf)
library(leaflet)
library(leaflet.extras)
library(readr)
library(stringr)
library(magrittr)
library(lubridate)
library(dplyr)
library(htmlwidgets)
library(DT)
library(tidyr)
library(plotly)
library(sunburstR)
library(treemap)
library(shinycssloaders)
library(shinyBS)
library(bchousing)

# Source relevant files ----
source("helpers/chartFormat.R")

# Set app options ----
options(stringsAsFactors = F)
Sys.setenv(TZ = "America/Vancouver")

# Read data objects ----

# boundaries shapefiles
bcCensusDivs <- readRDS("./data/bc2011CensusDivisions.rds")
bcCensusEconRegs <- readRDS("./data/bc2011EconomicRegions.rds")
bcCensusTracts <- readRDS("./data/bc2011Tracts.rds")
bcCensusMetroAreas <- readRDS(("./data/bc2011MetropolitanAreas.rds"))

# census 2016 - used for home page info boxes
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

# Not used
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
housingTypesCma <- readRDS(file.path("data", "housing", "census2016-housing-CMA.rds")) %>% na.omit()
housingTypesCsd <- readRDS(file.path("data", "housing", "census2016-housing-CSD.rds")) %>% na.omit()
housingTypesCd <- readRDS(file.path("data", "housing", "census2016-housing-CD.rds")) %>% na.omit()
housingTypesCt <- readRDS(file.path("data", "housing", "census2016-housing-CT.rds")) %>% na.omit()
# housingTypesDa <- readRDS(file.path("data", "housing", "census2016-housing-DA.rds"))

# Shelter-Cost-to-Income Ratio data
census2016CmaStir <- read_rds(file.path("data", "census2016Spatial-stir-CMA.rds"))
census2016CdStir <- read_rds(file.path("data", "census2016Spatial-stir-CD.rds"))
census2016CsdStir <- read_rds(file.path("data", "census2016Spatial-stir-CSD.rds"))
census2016CtStir <- read_rds(file.path("data", "census2016Spatial-stir-CT.rds"))
# census2016DaStir <- read_rds(file.path("data", "census2016Spatial-stir-DA.rds"))

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

# Add a homepage Jumbotron ----
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

  HTML(paste0(
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
}
