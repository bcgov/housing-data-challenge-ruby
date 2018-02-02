library(shiny)
library(rgdal)
library(sf)
library(leaflet)
library(maps)
library(readr)
library(stringr)
library(magrittr)
library(dplyr)
library(rlang)
library(htmlwidgets)
library(DT)
library(rgeos)
library(tidyr)
library(crosstalk)
library(plotly)
library(ggplot2)
library(cancensus)
library(sankeyD3)
library(sunburstR)
library(treemap)
library(data.tree)
library(d3treeR)
library(RColorBrewer)
library(shinycssloaders)

censusMobility <- censusMobilityCma
censusMobility %<>% rename(
  # `Non-movers` = v_CA16_6695,
  Movers = v_CA16_6698,
  # `Non-migrants` = v_CA16_6701,
  Migrants = v_CA16_6704,
  `Internal migrants` = v_CA16_6707#,
  # `External migrants` = v_CA16_6716,
  # `Intraprovincial migrants` = v_CA16_6710,
  # `Interprovincial migrants` = v_CA16_6713,
)


vectorsMobility <- search_census_vectors(' Mobility status 1 year ago', "CA16", type = "Total") %>%
  child_census_vectors(leaves_only = FALSE)

for (censusLevel in c("CMA", "CD", "CSD", "CT", "DA")) {
  censusData <-
    get_census(
      "CA16",
      level = censusLevel,
      # regions = cdRegions,
      regions = regions,
      vectors = vectorsMobility %>% pull("vector"),
      use_cache = TRUE,
      labels = "short",
      geo_format = NA
    )
  censusData %<>%
    rename(
      `Non-movers` = v_CA16_6695,
      # Movers = v_CA16_6698,
      `Non-migrants` = v_CA16_6701,
      # Migrants = v_CA16_6704,
      # `Internal migrants` = v_CA16_6707,
      `External migrants` = v_CA16_6716,
      `Intraprovincial migrants` = v_CA16_6710,
      `Interprovincial migrants` = v_CA16_6713,
    )
  censusData %<>% filter(Type == censusLevel)
  saveRDS(censusData, here::here("data", paste0("census2016-mobility-", censusLevel, ".rds")))
}
