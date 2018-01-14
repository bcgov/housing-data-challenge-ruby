devtools::install_github("mountainmath/cancensus")
library(cancensus)
library(dplyr)
library(magrittr)
library(here)

options(cancensus.api_key = "CensusMapper_f17c13c7fc5e60de7cdd341d5d4db866")
options(cancensus.cache_path = here::here("cache"))

getRegions <- function() {
  regions <- list_census_regions("CA16", use_cache = TRUE)
  return(as_census_region_list(regions %>% filter(PR_UID == "59")))
}

censusForYear <- function(year, level = "CD") {
  year <- 2016
  censusYear <- paste0('CA', substr(paste0(year), 3, 4))

  regions <- list_census_regions(censusYear, use_cache = TRUE)
  bc <- regions %>% filter(PR_UID == "59")

  cma <- bc %>% filter(level == "CMA")
  cmaRegions <- as_census_region_list(cma)

  cd <- bc %>% filter(level == "CD")
  cdRegions <- as_census_region_list(cd)

  csd <- bc %>% filter(level == "CSD")
  csdRegions <- as_census_region_list(csd)

  regions = as_census_region_list(rbind(cma, cd, csd))

  allVectors <-
    dplyr::na_if(list_census_vectors(censusYear, use_cache = TRUE), "")

  parents <-
    allVectors %>% filter(is.na(parent_vector) == TRUE, type == "Total")

  # Income vectors
  vectorsIncome <-
    search_census_vectors(
      # 'Household total income groups in 2015 for private households',
      "Income statistics in 2015 for economic families",
      censusYear,
      type = "Total",
      use_cache = TRUE
    )  %>%
    child_census_vectors(leaves_only = FALSE) # %>% # pull("vector") %>%
  # filter(grepl("family", label))

  # Search age data
  vectorsAge <-
    search_census_vectors(' Age', censusYear, type = "Total", use_cache =  TRUE) %>%
    # child_census_vectors(leaves_only = TRUE) %>%
    filter(vector %in% c("v_CA16_379", "v_CA16_382"))

  # Search dwelling data
  vectorsDwelling <-
    search_census_vectors('dwelling', censusYear, type = "Total") %>%
    child_census_vectors(leaves_only = TRUE)

  vectors = do.call(rbind, list(vectorsAge, vectorsDwelling, vectorsIncome))

  censusDataSpatial <-
    get_census(
      censusYear,
      level = level,
      # regions = cdRegions,
      regions = regions,
      vectors = vectors %>% pull("vector"),
      use_cache = TRUE,
      labels = "short",
      geo_format = "sp"
    )

  saveRDS(censusDataSpatial,
          paste0("./data/censusSpatial",  year, "-", level, ".rds"))

  # censusData <- as.data.frame(censusDataSpatial)
  censusData <-
    get_census(
      censusYear,
      level = level,
      # regions = cdRegions,
      regions = regions,
      vectors = vectors %>% pull("vector"),
      use_cache = TRUE,
      labels = "short",
      geo_format = NA
    )
  saveRDS(censusData, paste0("./data/census",  year, "-", level, ".rds"))
}

for (censusYear in c("2006", "2011", "2016")) {
  for (censusLevel in c("CMA", "CD", "CSD", "CT", "DA")) {
    censusForYear(censusYear, censusLevel)
  }
}


# mobility
vectorsMobility <- search_census_vectors(' Mobility status 1 year ago', "CA16", type = "Total") %>%
  child_census_vectors(leaves_only = FALSE)

regions <- getRegions()

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

# Shelter-Cost-to-Income Ratio
vectorsStir <- c("v_CA16_4886", "v_CA16_4887", "v_CA16_4888")
for (censusLevel in c("CMA", "CD", "CSD", "CT", "DA")) {
  censusStirData <-
    get_census(
      "CA16",
      level = censusLevel,
      regions = regions,
      vectors = vectorsStir,# %>% pull("vector"),
      use_cache = TRUE,
      labels = "short",
      geo_format = "sf"
    )

  # sp
  # censusStirData <- censusStirData[censusStirData$Type == censusLevel,]
  # names(censusStirData)[names(censusStirData) == "v_CA16_4886"] <- "total_households_with_income"
  # names(censusStirData)[names(censusStirData) == "v_CA16_4887"] <- "stir_less_than_30"
  # names(censusStirData)[names(censusStirData) == "v_CA16_4888"] <- "stir_more_than_30"
  # censusStirData$percent_less_than_30 = round(censusStirData$stir_less_than_30 / censusStirData$total_households_with_income * 100, digits = 2)
  # censusStirData$percent_more_than_30 = round(censusStirData$stir_more_than_30 / censusStirData$total_households_with_income * 100, digits = 2)
  # censusStirData$Region = paste(censusStirData$`Region Name`, str_sub(censusStirData$GeoUID, -2))
  # censusStirData$Region = factor(
  #       censusStirData$Region,
  #       levels = unique(censusStirData$Region)[order(censusStirData$percent_more_than_30, decreasing = FALSE)]
  #     )
  #   # top_n(25, percent_more_than_30) %<>%
  #   # )

  # sf
  censusStirData %<>%
    filter(Type == censusLevel) %<>%
    rename(
      `total_households_with_income` = v_CA16_4886,
      `stir_less_than_30` = v_CA16_4887,
      `stir_more_than_30` = v_CA16_4888
    ) %<>%
    mutate(
      Region = ifelse(censusLevel %in% c("CSD", "CT", "DA"),
                      paste(`Region Name`, str_sub(GeoUID, -2)), `Region Name`),
      percent_less_than_30 =
        round(stir_less_than_30 / total_households_with_income * 100, digits = 2),
      percent_more_than_30 =
        round(stir_more_than_30 / total_households_with_income * 100, digits = 2)
    ) %<>%
    arrange(desc(percent_more_than_30))

  saveRDS(censusStirData, here::here("data", paste0("census2016Spatial-stir-", censusLevel, ".rds")))
}

