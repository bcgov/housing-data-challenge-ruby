# devtools::install_github("mountainmath/cancensus")
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

# Age and Sex - Population pyramid
# cancensus
## Census 2016 has 5 year bands up to 100 years of age and over,
## while census 2011 and 2006 bands go up to 85 and over
## We will group 2016 data in the same bands for comparison purposes
# getPopulationPyramidData <- function(year = 2016, level = "CMA", vectorsFemale, vectorsMale, regions) {
year = 2016
censusLevel = "CMA"
vectorsFemale <- c(
        "v_CA16_9", "v_CA16_27", "v_CA16_45", "v_CA16_66", "v_CA16_84", "v_CA16_102", "v_CA16_120",
        "v_CA16_138", "v_CA16_156", "v_CA16_174", "v_CA16_192", "v_CA16_210", "v_CA16_228", "v_CA16_249",
        "v_CA16_267", "v_CA16_285", "v_CA16_303", "v_CA16_324", "v_CA16_342", "v_CA16_360", "v_CA16_378"
      )
vectorsMale <- c(
        "v_CA16_8", "v_CA16_26", "v_CA16_44", "v_CA16_65", "v_CA16_83", "v_CA16_101", "v_CA16_119",
        "v_CA16_137", "v_CA16_155", "v_CA16_173", "v_CA16_191", "v_CA16_209", "v_CA16_227", "v_CA16_248",
        "v_CA16_266", "v_CA16_284", "v_CA16_302", "v_CA16_323", "v_CA16_341", "v_CA16_359", "v_CA16_377"
      )

censusYear <- paste0('CA', substr(paste0(year), 3, 4))

# Female population
censusPPFemale <- get_census(
    censusYear, level = censusLevel, regions = regions, vectors = vectorsFemale,
    use_cache = TRUE, labels = "short", geo_format = NA
  )
censusPPFemale %<>%
  filter(Type == censusLevel) %<>%
  mutate(sex = "female") %<>%
  select(
    GeoUID, Type, `Region Name`, sex,
    "0 to 4 years" = vectorsFemale[1], "5 to 9 years" = vectorsFemale[2], "10 to 14 years" = vectorsFemale[3],
    "15 to 19 years" = vectorsFemale[4], "20 to 24 years" = vectorsFemale[5], "25 to 29 years" = vectorsFemale[6],
    "30 to 34 years" = vectorsFemale[7], "35 to 39 years" = vectorsFemale[8], "40 to 44 years" = vectorsFemale[9],
    "45 to 49 years" = vectorsFemale[10], "50 to 54 years" = vectorsFemale[11], "55 to 59 years" = vectorsFemale[12],
    "60 to 64 years" = vectorsFemale[13], "65 to 69 years" = vectorsFemale[14], "70 to 74 years" = vectorsFemale[15],
    "75 to 79 years" = vectorsFemale[16], "80 to 84 years" = vectorsFemale[17],
    "85 to 89 years" = contains(vectorsFemale[18]), "90 to 94 years" = contains(vectorsFemale[19]),
    "95 to 99 years" = contains(vectorsFemale[20]), "100 years and over" = contains(vectorsFemale[21])
  )
censusPPFemale %<>%
  gather("age", "population", 5:(length(vectorsFemale) + 4))

# Male population
censusPPMale <-
  get_census(
    censusYear, level = censusLevel, regions = regions, vectors = vectorsMale,
    use_cache = TRUE, labels = "short", geo_format = NA
  )
censusPPMale <- censusPPMale %<>%
  filter(Type == censusLevel) %<>%
  mutate(sex = "male") %<>%
  select(
    GeoUID, Type, `Region Name`, sex,
    "0 to 4 years" = vectorsMale[1], "5 to 9 years" = vectorsMale[2], "10 to 14 years" = vectorsMale[3],
    "15 to 19 years" = vectorsMale[4], "20 to 24 years" = vectorsMale[5], "25 to 29 years" = vectorsMale[6],
    "30 to 34 years" = vectorsMale[7], "35 to 39 years" = vectorsMale[8], "40 to 44 years" = vectorsMale[9],
    "45 to 49 years" = vectorsMale[10], "50 to 54 years" = vectorsMale[11], "55 to 59 years" = vectorsMale[12],
    "60 to 64 years" = vectorsMale[13], "65 to 69 years" = vectorsMale[14], "70 to 74 years" = vectorsMale[15],
    "75 to 79 years" = vectorsMale[16], "80 to 84 years" = vectorsMale[17],
    "85 to 89 years" = contains(vectorsMale[18]), "90 to 94 years" = contains(vectorsMale[19]),
    "95 to 99 years" = contains(vectorsMale[20]), "100 years and over" = contains(vectorsMale[21])
  )
censusPPMale %<>%
  gather("age", "population", 5:(length(vectorsMale) + 4))

censusPP <- bind_rows(censusPPMale, censusPPFemale)
censusPP %<>%
  mutate(
    Type = as.character(Type),
    `Region` = as.character(`Region Name`),
    sex = as.character(sex),
    age = as.character(age)
  )

censusPP %<>%
  mutate(age = ifelse(age %in% (
    c(
      "85 to 89 years",
      "90 to 94 years",
      "95 to 99 years",
      "100 years and over"
    )
  ),
  "85 years and over",
  age)) %<>%
  group_by(GeoUID, Type, Region, sex, age) %<>%
  summarise(population = sum(population)) %<>%
  mutate(percentage = population / sum(population) / 2) %<>%
  mutate(percentage = ifelse(sex == "male", percentage * -1, percentage))

censusPP

#   return(censusPP)
# }
#
# # Loop through year and geographical levels and save general census-related data
# for (censusYear in c("2006", "2011", "2016")) {
#   # for (censusYear in c("2011")) {
#   print(paste("Fetching data for ", censusYear))
#   switch(
#     censusYear,
#     "2016" = {
#       print(paste("Setting vectors inside 2016 "))
#       ppVectorsFemale <- c(
#         "v_CA16_9", "v_CA16_27", "v_CA16_45", "v_CA16_66", "v_CA16_84", "v_CA16_102", "v_CA16_120",
#         "v_CA16_138", "v_CA16_156", "v_CA16_174", "v_CA16_192", "v_CA16_210", "v_CA16_228", "v_CA16_249",
#         "v_CA16_267", "v_CA16_285", "v_CA16_303", "v_CA16_324", "v_CA16_342", "v_CA16_360", "v_CA16_378"
#       )
#       ppVectorsMale <- c(
#         "v_CA16_8", "v_CA16_26", "v_CA16_44", "v_CA16_65", "v_CA16_83", "v_CA16_101", "v_CA16_119",
#         "v_CA16_137", "v_CA16_155", "v_CA16_173", "v_CA16_191", "v_CA16_209", "v_CA16_227", "v_CA16_248",
#         "v_CA16_266", "v_CA16_284", "v_CA16_302", "v_CA16_323", "v_CA16_341", "v_CA16_359", "v_CA16_377"
#       )
#     },
#     "2011" = {
#       print(paste("Setting vectors inside 2011 "))
#       ppVectorsFemale <- c(
#         "v_CA11F_10", "v_CA11F_13", "v_CA11F_16", "v_CA11F_19", "v_CA11F_28", "v_CA11F_40", "v_CA11F_43",
#         "v_CA11F_46", "v_CA11F_49", "v_CA11F_52", "v_CA11F_55", "v_CA11F_58", "v_CA11F_61", "v_CA11F_64",
#         "v_CA11F_67", "v_CA11F_70", "v_CA11F_73", "v_CA11F_76"
#       )
#       ppVectorsMale <- c(
#         "v_CA11F_9", "v_CA11F_12", "v_CA11F_15", "v_CA11F_18", "v_CA11F_27", "v_CA11F_39", "v_CA11F_42",
#         "v_CA11F_45", "v_CA11F_48", "v_CA11F_51", "v_CA11F_54", "v_CA11F_57", "v_CA11F_60", "v_CA11F_63",
#         "v_CA11F_66", "v_CA11F_69", "v_CA11F_72", "v_CA11F_75"
#       )
#     },
#     "2006" = {
#       print(paste("Setting vectors inside 2006 "))
#       ppVectorsFemale <- c(
#         "v_CA06_23", "v_CA06_24", "v_CA06_25", "v_CA06_26", "v_CA06_27", "v_CA06_28", "v_CA06_29",
#         "v_CA06_30", "v_CA06_31", "v_CA06_32", "v_CA06_33", "v_CA06_34", "v_CA06_35", "v_CA06_36",
#         "v_CA06_37", "v_CA06_38", "v_CA06_39", "v_CA06_40"
#       )
#       ppVectorsMale <- c(
#         "v_CA06_4", "v_CA06_5", "v_CA06_6", "v_CA06_7", "v_CA06_8", "v_CA06_9", "v_CA06_10",
#         "v_CA06_11", "v_CA06_12", "v_CA06_13", "v_CA06_14", "v_CA06_15", "v_CA06_16", "v_CA06_17",
#         "v_CA06_18", "v_CA06_19", "v_CA06_20", "v_CA06_21"
#       )
#     }
#   )
#
#   for (censusLevel in c("CMA", "CD", "CSD", "CT", "DA", "PR")) {
#     print(paste("Now getting geo-level ", censusLevel, " for ", censusYear))
#     ppData <- getPopulationPyramidData(censusYear, censusLevel, ppVectorsFemale, ppVectorsMale, regions)
#     saveRDS(ppData, here::here("data", "population_pyramid", paste0("census", censusYear, "-pp-", censusLevel, ".rds")))
#   }
# }
