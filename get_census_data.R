devtools::install_github("mountainmath/cancensus")
library(cancensus)
library(dplyr)

options(cancensus.api_key = "CensusMapper_f17c13c7fc5e60de7cdd341d5d4db866")
options(cancensus.cache_path = "./cache/")

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
