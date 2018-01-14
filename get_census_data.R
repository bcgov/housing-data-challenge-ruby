devtools::install_github("mountainmath/cancensus")
library(cancensus)
library(dplyr)
library(magrittr)
library(tidyr)
library(here)

# cancensus global config
options(cancensus.api_key = "CensusMapper_f17c13c7fc5e60de7cdd341d5d4db866")
options(cancensus.cache_path = here::here("cache"))

# get only BC regions
getRegions <- function() {
  regions <- list_census_regions("CA16", use_cache = TRUE)
  return(as_census_region_list(regions %>% filter(PR_UID == "59")))
}
regions <- getRegions()

# General census data
# @TODO: Remove vectors that will not be in use
censusForYear <- function(year, level = "CMA") {
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

# Loop through year and geographical levels and save general census-related data
for (censusYear in c("2006", "2011", "2016")) {
  for (censusLevel in c("CMA", "CD", "CSD", "CT", "DA")) {
    censusForYear(censusYear, censusLevel)
  }
}


# mobility
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

  # sp format
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

  # sf format
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

# Age and Sex - Population pyramid
# StatCan
# CMA, CT
# unzip("98-400-X2016005_English_CSV_data.csv", zipfile = here::here("data", "98-400-X2016005_ENG_CSV.ZIP"))
# agesexCmaCt <- read_csv(here::here("98-400-X2016005_English_CSV_data.csv"))
# file.remove(here::here("98-400-X2016005_English_CSV_data.csv"))

# CD, CSD, DA
# unzip("98-400-X2016003_English_CSV_data.csv", zipfile = here::here("data", "98-400-X2016003_ENG_CSV.ZIP"))
# agesexCdCsdDa <- read_csv(here::here("98-400-X2016003_English_CSV_data.csv"))
# file.remove(here::here("98-400-X2016003_English_CSV_data.csv"))

# cancensus
## Census 2016 has 5 year bands up to 100 years of age and over,
## while census 2011 and 2006 bands go up to 85 and over
## We will group 2016 data in the same bands for comparison purposes
getPopulationPyramidData <- function(year = 2016, level = "CMA", vectorsFemale, vectorsMale, regions) {
  censusYear <- paste0('CA', substr(paste0(year), 3, 4))

  # age <- c(
  #   "0 to 4 years", "5 to 9 years", "10 to 14 years", "15 to 19 years", "20 to 24 years",
  #   "25 to 29 years", "30 to 34 years", "35 to 39 years", "40 to 44 years", "45 to 49 years",
  #   "50 to 54 years", "55 to 59 years", "60 to 64 years", "65 to 69 years", "70 to 74 years",
  #   "75 to 79 years", "80 to 84 years", "85 to 89 years", "90 to 94 years", "95 to 99 years",
  #   "100 years and over"
  # )

  # Female population
  censusPPFemale <-
    get_census(
      censusYear,
      level = censusLevel,
      regions = regions,
      vectors = vectorsFemale,
      use_cache = TRUE,
      labels = "short",
      geo_format = NA
    )
  # for (i in 1:length(vectorsFemale)) {
  #   vectorsFemale[i] <- enquo(vectorsFemale[i])
  # }
  censusPPFemale %<>%
    filter(Type == censusLevel) %<>%
    mutate(
      `Area (sq km)` = as.character(`Area (sq km)`),
      Population = as.character(Population),
      Dwellings = as.character(Dwellings),
      Households = as.character(Households),
      sex = "female"
    ) %<>%
    select(-one_of(c("Area (sq km)", "Population", "Dwellings", "Households"))) %<>%
    # Using expressions in rename https://github.com/tidyverse/dplyr/issues/1600
    # rename(
    #   !! vectorsFemale[1] := "0 to 4 years",
    #   !! vectorsFemale[2] := "5 to 9 years",
    #   !! vectorsFemale[3] := "10 to 14 years",
    #   !! vectorsFemale[4] := "15 to 19 years",
    #   !! vectorsFemale[5] := "20 to 24 years",
    #   !! vectorsFemale[6] := "25 to 29 years",
    #   !! vectorsFemale[7] := "30 to 34 years",
    #   !! vectorsFemale[8] := "35 to 39 years",
    #   !! vectorsFemale[9] := "40 to 44 years",
    #   !! vectorsFemale[10] := "45 to 49 years",
    #   !! vectorsFemale[11] := "50 to 54 years",
    #   !! vectorsFemale[12] := "55 to 59 years",
    #   !! vectorsFemale[13] := "60 to 64 years",
    #   !! vectorsFemale[14] := "65 to 69 years",
    #   !! vectorsFemale[15] := "70 to 74 years",
    #   !! vectorsFemale[16] := "75 to 79 years",
    #   !! vectorsFemale[17] := "80 to 84 years"
    #   # "0 to 4 years" = vectorsFemale[1], "5 to 9 years" = vectorsFemale[2], "10 to 14 years" = vectorsFemale[3],
    #   # "15 to 19 years" = vectorsFemale[4], "20 to 24 years" = vectorsFemale[5], "25 to 29 years" = vectorsFemale[6],
    #   # "30 to 34 years" = vectorsFemale[7], "35 to 39 years" = vectorsFemale[8], "40 to 44 years" = vectorsFemale[9],
    #   # "45 to 49 years" = vectorsFemale[10], "50 to 54 years" = vectorsFemale[11], "55 to 59 years" = vectorsFemale[12],
    #   # "60 to 64 years" = vectorsFemale[13], "65 to 69 years" = vectorsFemale[14], "70 to 74 years" = vectorsFemale[15],
    #   # "75 to 79 years" = vectorsFemale[16], "80 to 84 years" = vectorsFemale[17]
    # )
    # select(
    #   GeoUID, Type, `Region Name`, sex,
    #   "0 to 4 years" = vectorsFemale[1], "5 to 9 years" = vectorsFemale[2], "10 to 14 years" = vectorsFemale[3],
    #   "15 to 19 years" = vectorsFemale[4], "20 to 24 years" = vectorsFemale[5], "25 to 29 years" = vectorsFemale[6],
    #   "30 to 34 years" = vectorsFemale[7], "35 to 39 years" = vectorsFemale[8], "40 to 44 years" = vectorsFemale[9],
    #   "45 to 49 years" = vectorsFemale[10], "50 to 54 years" = vectorsFemale[11], "55 to 59 years" = vectorsFemale[12],
    #   "60 to 64 years" = vectorsFemale[13], "65 to 69 years" = vectorsFemale[14], "70 to 74 years" = vectorsFemale[15],
    #   "75 to 79 years" = vectorsFemale[16], "80 to 84 years" = vectorsFemale[17]
    # )
  # rename(
  #   "0 to 4 years" = (!! vectorsFemale[1]),
  #   "5 to 9 years" = (!! vectorsFemale[2]),
  #   "10 to 14 years" = (!! vectorsFemale[3]),
  #   "15 to 19 years" = (!! vectorsFemale[4]),
  #   "20 to 24 years" = (!! vectorsFemale[5]),
  #   "25 to 29 years" = (!! vectorsFemale[6]),
  #   "30 to 34 years" = (!! vectorsFemale[7]),
  #   "35 to 39 years" = (!! vectorsFemale[8]),
  #   "40 to 44 years" = (!! vectorsFemale[9]),
  #   "45 to 49 years" = (!! vectorsFemale[10]),
  #   "50 to 54 years" = (!! vectorsFemale[11]),
  #   "55 to 59 years" = (!! vectorsFemale[12]),
  #   "60 to 64 years" = (!! vectorsFemale[13]),
  #   "65 to 69 years" = (!! vectorsFemale[14]),
  #   "70 to 74 years" = (!! vectorsFemale[15]),
  #   "75 to 79 years" = (!! vectorsFemale[16]),
  #   "80 to 84 years" = (!! vectorsFemale[17])
  # )
  rename_(
    "0 to 4 years" = vectorsFemale[1],
    "5 to 9 years" = vectorsFemale[2],
    "10 to 14 years" = vectorsFemale[3],
    "15 to 19 years" = vectorsFemale[4],
    "20 to 24 years" = vectorsFemale[5],
    "25 to 29 years" = vectorsFemale[6],
    "30 to 34 years" = vectorsFemale[7],
    "35 to 39 years" = vectorsFemale[8],
    "40 to 44 years" = vectorsFemale[9],
    "45 to 49 years" = vectorsFemale[10],
    "50 to 54 years" = vectorsFemale[11],
    "55 to 59 years" = vectorsFemale[12],
    "60 to 64 years" = vectorsFemale[13],
    "65 to 69 years" = vectorsFemale[14],
    "70 to 74 years" = vectorsFemale[15],
    "75 to 79 years" = vectorsFemale[16],
    "80 to 84 years" = vectorsFemale[17]
  )
  if (year == 2016) {
    censusPPFemale %<>%
      # rename(
      #   !! vectorsFemale[18] := "85 to 89 years",
      #   !! vectorsFemale[19] := "90 to 94 years",
      #   !! vectorsFemale[20] := "95 to 99 years",
      #   !! vectorsFemale[21] := "100 years and over"
      #   # "85 to 89 years" = vectorsFemale[18], "90 to 94 years" = vectorsFemale[19],
      #   # "95 to 99 years" = vectorsFemale[20], "100 years and over" = vectorsFemale[21]
      # )
      # select(
      #   GeoUID, Type, `Region Name`, sex,
      #   "0 to 4 years", "5 to 9 years", "10 to 14 years",
      #   "15 to 19 years", "20 to 24 years", "25 to 29 years",
      #   "30 to 34 years", "35 to 39 years", "40 to 44 years",
      #   "45 to 49 years", "50 to 54 years", "55 to 59 years",
      #   "60 to 64 years", "65 to 69 years", "70 to 74 years",
      #   "75 to 79 years", "80 to 84 years",
      #   "85 to 89 years" = vectorsFemale[18], "90 to 94 years" = vectorsFemale[19],
      #   "95 to 99 years" = vectorsFemale[20], "100 years and over" = vectorsFemale[21]
      # )
    # rename(
    #   "85 to 89 years" = (!! vectorsFemale[18]),
    #   "90 to 94 years" = (!! vectorsFemale[19]),
    #   "95 to 99 years" = (!! vectorsFemale[20]),
    #   "100 years and over" = (!! vectorsFemale[21])
    # )
    rename_(
      "85 to 89 years" = vectorsFemale[18],
      "90 to 94 years" = vectorsFemale[19],
      "95 to 99 years" = vectorsFemale[20],
      "100 years and over" = vectorsFemale[21]
    )
  }
  censusPPFemale %<>% gather("age", "population", 4:(length(vectorsFemale) - 3))

  censusPPFemale%<>%
    mutate(age = factor(
        censusPPFemale$age,
        levels = unique(censusPPFemale$age)[order(censusPPFemale$age, decreasing = FALSE)]
      )
    )

  # Male population
  censusPPMale <-
    get_census(
      censusYear,
      level = censusLevel,
      regions = regions,
      vectors = vectorsMale,
      use_cache = TRUE,
      labels = "short",
      geo_format = NA
    )
  # for (i in 1:length(vectorsMale)) {
  #   vectorsMale[i] <- enquo(vectorsMale[i])
  # }
  censusPPMale <- censusPPMale %<>%
    filter(Type == censusLevel) %<>%
    mutate(
      `Area (sq km)` = as.character(`Area (sq km)`),
      Population = as.character(Population),
      Dwellings = as.character(Dwellings),
      Households = as.character(Households),
      sex = "male"
    ) %<>%
    select(-one_of(c("Area (sq km)", "Population", "Dwellings", "Households"))) %<>%
    # rename(
    #   !! vectorsMale[1] := "0 to 4 years",
    #   !! vectorsMale[2] := "5 to 9 years",
    #   !! vectorsMale[3] := "10 to 14 years",
    #   !! vectorsMale[4] := "15 to 19 years",
    #   !! vectorsMale[5] := "20 to 24 years",
    #   !! vectorsMale[6] := "25 to 29 years",
    #   !! vectorsMale[7] := "30 to 34 years",
    #   !! vectorsMale[8] := "35 to 39 years",
    #   !! vectorsMale[9] := "40 to 44 years",
    #   !! vectorsMale[10] := "45 to 49 years",
    #   !! vectorsMale[11] := "50 to 54 years",
    #   !! vectorsMale[12] := "55 to 59 years",
    #   !! vectorsMale[13] := "60 to 64 years",
    #   !! vectorsMale[14] := "65 to 69 years",
    #   !! vectorsMale[15] := "70 to 74 years",
    #   !! vectorsMale[16] := "75 to 79 years",
    #   !! vectorsMale[17] := "80 to 84 years"
    #   # "0 to 4 years" = vectorsMale[1], "5 to 9 years" = vectorsMale[2], "10 to 14 years" = vectorsMale[3],
    #   # "15 to 19 years" = vectorsMale[4], "20 to 24 years" = vectorsMale[5], "25 to 29 years" = vectorsMale[6],
    #   # "30 to 34 years" = vectorsMale[7], "35 to 39 years" = vectorsMale[8], "40 to 44 years" = vectorsMale[9],
    #   # "45 to 49 years" = vectorsMale[10], "50 to 54 years" = vectorsMale[11], "55 to 59 years" = vectorsMale[12],
    #   # "60 to 64 years" = vectorsMale[13], "65 to 69 years" = vectorsMale[14], "70 to 74 years" = vectorsMale[15],
    #   # "75 to 79 years" = vectorsMale[16], "80 to 84 years" = vectorsMale[17]
    # )
  # select(
  #   GeoUID, Type, `Region Name`, sex,
  #   "0 to 4 years" = vectorsMale[1], "5 to 9 years" = vectorsMale[2], "10 to 14 years" = vectorsMale[3],
  #   "15 to 19 years" = vectorsMale[4], "20 to 24 years" = vectorsMale[5], "25 to 29 years" = vectorsMale[6],
  #   "30 to 34 years" = vectorsMale[7], "35 to 39 years" = vectorsMale[8], "40 to 44 years" = vectorsMale[9],
  #   "45 to 49 years" = vectorsMale[10], "50 to 54 years" = vectorsMale[11], "55 to 59 years" = vectorsMale[12],
  #   "60 to 64 years" = vectorsMale[13], "65 to 69 years" = vectorsMale[14], "70 to 74 years" = vectorsMale[15],
  #   "75 to 79 years" = vectorsMale[16], "80 to 84 years" = vectorsMale[17]
  # )
  # rename(
  #   "0 to 4 years" = (!! vectorsMale[1]),
  #   "5 to 9 years" = (!! vectorsMale[2]),
  #   "10 to 14 years" = (!! vectorsMale[3]),
  #   "15 to 19 years" = (!! vectorsMale[4]),
  #   "20 to 24 years" = (!! vectorsMale[5]),
  #   "25 to 29 years" = (!! vectorsMale[6]),
  #   "30 to 34 years" = (!! vectorsMale[7]),
  #   "35 to 39 years" = (!! vectorsMale[8]),
  #   "40 to 44 years" = (!! vectorsMale[9]),
  #   "45 to 49 years" = (!! vectorsMale[10]),
  #   "50 to 54 years" = (!! vectorsMale[11]),
  #   "55 to 59 years" = (!! vectorsMale[12]),
  #   "60 to 64 years" = (!! vectorsMale[13]),
  #   "65 to 69 years" = (!! vectorsMale[14]),
  #   "70 to 74 years" = (!! vectorsMale[15]),
  #   "75 to 79 years" = (!! vectorsMale[16]),
  #   "80 to 84 years" = (!! vectorsMale[17])
  # )
  rename_(
    "0 to 4 years" = vectorsMale[1],
    "5 to 9 years" = vectorsMale[2],
    "10 to 14 years" = vectorsMale[3],
    "15 to 19 years" = vectorsMale[4],
    "20 to 24 years" = vectorsMale[5],
    "25 to 29 years" = vectorsMale[6],
    "30 to 34 years" = vectorsMale[7],
    "35 to 39 years" = vectorsMale[8],
    "40 to 44 years" = vectorsMale[9],
    "45 to 49 years" = vectorsMale[10],
    "50 to 54 years" = vectorsMale[11],
    "55 to 59 years" = vectorsMale[12],
    "60 to 64 years" = vectorsMale[13],
    "65 to 69 years" = vectorsMale[14],
    "70 to 74 years" = vectorsMale[15],
    "75 to 79 years" = vectorsMale[16],
    "80 to 84 years" = vectorsMale[17]
  )
  if (year == 2016) {
    censusPPMale %<>%
      # rename(
        # !! vectorsMale[18] := "85 to 89 years",
        # !! vectorsMale[19] := "90 to 94 years",
        # !! vectorsMale[20] := "95 to 99 years",
        # !! vectorsMale[21] := "100 years and over"
        # "85 to 89 years" = vectorsMale[18], "90 to 94 years" = vectorsMale[19],
        # "95 to 99 years" = vectorsMale[20], "100 years and over" = vectorsMale[21]
      # )
      # select(
      #   GeoUID, Type, `Region Name`, sex,
      #   "0 to 4 years", "5 to 9 years", "10 to 14 years",
      #   "15 to 19 years", "20 to 24 years", "25 to 29 years",
      #   "30 to 34 years", "35 to 39 years", "40 to 44 years",
      #   "45 to 49 years", "50 to 54 years", "55 to 59 years",
      #   "60 to 64 years", "65 to 69 years", "70 to 74 years",
      #   "75 to 79 years", "80 to 84 years",
      #   "85 to 89 years" = vectorsMale[18], "90 to 94 years" = vectorsMale[19],
      #   "95 to 99 years" = vectorsMale[20], "100 years and over" = vectorsMale[21]
      # )
    rename_(
      "85 to 89 years" = vectorsMale[18],
      "90 to 94 years" = vectorsMale[19],
      "95 to 99 years" = vectorsMale[20],
      "100 years and over" = vectorsMale[21]
    )
  }

  censusPPMale %<>% gather("age", "population", 4:(length(vectorsMale) - 3))

  censusPPMale %<>%
    mutate(age = factor(
        censusPPMale$age,
        levels = unique(censusPPMale$age)[order(censusPPMale$age, decreasing = FALSE)]
      )
    )

  censusPP <- bind_rows(censusPPMale, censusPPFemale)
  censusPP %<>%
    mutate(
      Type = as.character(Type),
      `Region` = as.character(`Region Name`),
      sex = as.character(sex),
      age = as.character(age)
    )

  censusPP %<>%
    mutate(
      age = ifelse(
        age %in% (c("85 to 89 years", "90 to 94 years", "95 to 99 years", "100 years and over")),
        "85 years and over",
        age
      )
    ) %<>%
    group_by(GeoUID, Type, Region, sex, age) %<>%
    summarise(
      population = sum(population)
    ) %<>%
    mutate(percentage = population / sum(population) / 2) %<>%
    mutate(percentage = ifelse(sex == "male", percentage * -1, percentage))

  return(censusPP)
}

# Loop through year and geographical levels and save general census-related data
for (censusYear in c("2006", "2011", "2016")) {
# for (censusYear in c("2011")) {
  print(paste("Fetching data for ", censusYear))
  switch(
    censusYear,
    "2016" = {
  print(paste("Setting vectors inside 2016 "))
      ppVectorsFemale <- c(
        "v_CA16_9", "v_CA16_27", "v_CA16_45", "v_CA16_66", "v_CA16_84", "v_CA16_102", "v_CA16_120",
        "v_CA16_138", "v_CA16_156", "v_CA16_174", "v_CA16_192", "v_CA16_210", "v_CA16_228", "v_CA16_249",
        "v_CA16_267", "v_CA16_285", "v_CA16_303", "v_CA16_324", "v_CA16_342", "v_CA16_360", "v_CA16_378"
      )
      ppVectorsMale <- c(
        "v_CA16_8", "v_CA16_26", "v_CA16_44", "v_CA16_65", "v_CA16_83", "v_CA16_101", "v_CA16_119",
        "v_CA16_137", "v_CA16_155", "v_CA16_173", "v_CA16_191", "v_CA16_209", "v_CA16_227", "v_CA16_248",
        "v_CA16_266", "v_CA16_284", "v_CA16_302", "v_CA16_323", "v_CA16_341", "v_CA16_359", "v_CA16_377"
      )
    },
    "2011" = {
  print(paste("Setting vectors inside 2011 "))
      ppVectorsFemale <- c(
        "v_CA11F_10", "v_CA11F_13", "v_CA11F_16", "v_CA11F_19", "v_CA11F_28", "v_CA11F_40", "v_CA11F_43",
        "v_CA11F_46", "v_CA11F_49", "v_CA11F_52", "v_CA11F_55", "v_CA11F_58", "v_CA11F_61", "v_CA11F_64",
        "v_CA11F_67", "v_CA11F_70", "v_CA11F_73", "v_CA11F_76"
      )
      ppVectorsMale <- c(
        "v_CA11F_9", "v_CA11F_12", "v_CA11F_15", "v_CA11F_18", "v_CA11F_27", "v_CA11F_39", "v_CA11F_42",
        "v_CA11F_45", "v_CA11F_48", "v_CA11F_51", "v_CA11F_54", "v_CA11F_57", "v_CA11F_60", "v_CA11F_63",
        "v_CA11F_66", "v_CA11F_69", "v_CA11F_72", "v_CA11F_75"
      )
    },
    "2006" = {
  print(paste("Setting vectors inside 2006 "))
      ppVectorsFemale <- c(
        "v_CA06_23", "v_CA06_24", "v_CA06_25", "v_CA06_26", "v_CA06_27", "v_CA06_28", "v_CA06_29",
        "v_CA06_30", "v_CA06_31", "v_CA06_32", "v_CA06_33", "v_CA06_34", "v_CA06_35", "v_CA06_36",
        "v_CA06_37", "v_CA06_38", "v_CA06_39", "v_CA06_40"
      )
      ppVectorsMale <- c(
        "v_CA06_4", "v_CA06_5", "v_CA06_6", "v_CA06_7", "v_CA06_8", "v_CA06_9", "v_CA06_10",
        "v_CA06_11", "v_CA06_12", "v_CA06_13", "v_CA06_14", "v_CA06_15", "v_CA06_16", "v_CA06_17",
        "v_CA06_18", "v_CA06_19", "v_CA06_20", "v_CA06_21"
      )
    }
  )

  for (censusLevel in c("CMA", "CD", "CSD", "CT", "DA", "PR")) {
  print(paste("Now getting geo-level ", censusLevel, " for ", censusYear))
    ppData <- getPopulationPyramidData(censusYear, censusLevel, ppVectorsFemale, ppVectorsMale, regions)
    saveRDS(ppData, here::here("data", "population_pyramid", paste0("census", censusYear, "-pp-", censusLevel, ".rds")))
  }
}
