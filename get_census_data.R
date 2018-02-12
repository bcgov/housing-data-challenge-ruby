# devtools::install_github("mountainmath/cancensus")
library(cancensus)
library(dplyr)
library(readr)
library(magrittr)
library(stringr)
library(tidyr)
library(here)
library(rmapshaper)

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
getHousingTypesData <- function(year, censusLevel = "CMA", regions) {
  censusYear <- paste0('CA', substr(paste0(year), 3, 4))

  # Housing Types
  vectors <- switch(
    year,
    "2016" = {
      c(
        "v_CA16_409", "v_CA16_410", "v_CA16_412", "v_CA16_413", "v_CA16_414",
        "v_CA16_415", "v_CA16_416", "v_CA16_417"
      )
    },
    "2011" = {
      c(
        "v_CA11F_200", "v_CA11F_201", "v_CA11F_204", "v_CA11F_205", "v_CA11F_206",
        "v_CA11F_207", "v_CA11F_208", "v_CA11F_202"
      )
    },
    "2006" = {
      c(
        "v_CA06_120", "v_CA06_124", "v_CA06_121", "v_CA06_122", "v_CA06_123",
        "v_CA06_125", "v_CA06_126", "v_CA06_127"
      )
    }
  )

  censusHousing <- get_census(
      censusYear,
      level = censusLevel,
      regions = regions,
      vectors = vectors,
      use_cache = TRUE,
      labels = "short",
      geo_format = "sf"
    )
  censusHousing %<>%
    filter(Type == censusLevel) %<>%
    mutate(
      `Region Name` = as.character(`Region Name`),
      Type = as.character(Type)
    ) %<>%
    select(GeoUID, Region = `Region Name`, Type, starts_with("v_")) %<>%
    ms_simplify(keep = 0.05, keep_shapes = TRUE) %<>%
    # TODO: cater to vectors of other years too
    rename(
      "Appartment in tall building" = v_CA16_410,
      "Semi detached house" = v_CA16_412,
      "Row house" = v_CA16_413,
      "Appartment in duplex" = v_CA16_414,
      "Appartment in small building" = v_CA16_415,
      "Other single attached house" = v_CA16_416,
      "Movable dwelling" = v_CA16_417,
      "Single detached house" = v_CA16_409
    ) %<>%
    replace_na(`Single detached house` = 0,
               `Appartment in tall building` = 0,
               `Semi detached house` = 0,
               `Row house` = 0,
               `Appartment in duplex` = 0,
               `Appartment in small building` = 0,
               `Other single attached house` = 0,
               `Movable dwelling` = 0) %<>%
    mutate(
      "Single detached house ratio" = round(`Single detached house` * 100 / (
        `Single detached house` +
          `Appartment in tall building` +
          `Semi detached house` +
          `Row house` +
          `Appartment in duplex` +
          `Appartment in small building` +
          `Other single attached house` +
          `Movable dwelling`
      ), digits = 2),
      "Appartment in tall building ratio" = round(`Appartment in tall building` * 100 / (
        `Single detached house` +
          `Appartment in tall building` +
          `Semi detached house` +
          `Row house` +
          `Appartment in duplex` +
          `Appartment in small building` +
          `Other single attached house` +
          `Movable dwelling`
      ), digits = 2),
      "Semi detached house ratio" = round(`Semi detached house` * 100 / (
        `Single detached house` +
          `Appartment in tall building` +
          `Semi detached house` +
          `Row house` +
          `Appartment in duplex` +
          `Appartment in small building` +
          `Other single attached house` +
          `Movable dwelling`
      ), digits = 2),
      "Row house ratio" = round(`Row house` * 100 / (
        `Single detached house` +
          `Appartment in tall building` +
          `Semi detached house` +
          `Row house` +
          `Appartment in duplex` +
          `Appartment in small building` +
          `Other single attached house` +
          `Movable dwelling`
      ), digits = 2),
      "Appartment in duplex ratio" = round(`Appartment in duplex` * 100 / (
        `Single detached house` +
          `Appartment in tall building` +
          `Semi detached house` +
          `Row house` +
          `Appartment in duplex` +
          `Appartment in small building` +
          `Other single attached house` +
          `Movable dwelling`
      ), digits = 2),
      "Appartment in small building ratio" = round(`Appartment in small building` * 100 / (
        `Single detached house` +
          `Appartment in tall building` +
          `Semi detached house` +
          `Row house` +
          `Appartment in duplex` +
          `Appartment in small building` +
          `Other single attached house` +
          `Movable dwelling`
      ), digits = 2),
      "Other single attached house ratio" = round(`Other single attached house` * 100 / (
        `Single detached house` +
          `Appartment in tall building` +
          `Semi detached house` +
          `Row house` +
          `Appartment in duplex` +
          `Appartment in small building` +
          `Other single attached house` +
          `Movable dwelling`
      ), digits = 2),
      "Movable dwelling ratio" = round(`Movable dwelling` * 100 / (
        `Single detached house` +
          `Appartment in tall building` +
          `Semi detached house` +
          `Row house` +
          `Appartment in duplex` +
          `Appartment in small building` +
          `Other single attached house` +
          `Movable dwelling`
      ), digits = 2)
    )

  saveRDS(censusHousing, here::here("data", "housing", paste0("census",  year, "-housing-", censusLevel, ".rds")))
}

# Loop through year and geographical levels and save housing types-related data
# for (censusYear in c("2006", "2011", "2016")) {
for (censusYear in c("2016")) {
  for (censusLevel in c("CMA", "CD", "CSD", "CT", "DA")) {
    getHousingTypesData(censusYear, censusLevel, regions)
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
      regions = regions,
      vectors = vectorsMobility %>% pull("vector"),
      use_cache = TRUE,
      labels = "short",
      geo_format = "sf"
    )

  censusData %<>%
    select(
      -one_of(c("Shape Area", "name", "Adjusted Population (previous Census)",
                "PR_UID", "Area (sq km)"))
    ) %>%
    filter(Type == censusLevel) %>%
    ms_simplify(keep = 0.1, keep_shapes = TRUE) %>%
    rename(
      Region = Region.Name,
      "Non-movers" = v_CA16_6695,
      Movers = v_CA16_6698,
      "Non-migrants" = v_CA16_6701,
      Migrants = v_CA16_6704,
      "Internal migrants" = v_CA16_6707,
      "External migrants" = v_CA16_6716,
      "Intraprovincial migrants" = v_CA16_6710,
      "Interprovincial migrants" = v_CA16_6713
    ) %>%
    filter(!is.na(Movers) & !is.na(`Non-movers`)) %>%
    replace_na(
      Movers = 0,
      `Non-movers` = 0,
      `Non-migrants` = 0,
      `Migrants` = 0,
      `Internal migrants` = 0,
      `Intraprovincial migrants` = 0,
      `Interprovincial migrants` = 0,
      `External migrants` = 0
    ) %>%
    mutate(
      `Movers Ratio` = round(`Movers` / (`Non-movers` + `Movers`) * 100, digits = 2),
      `Non-Movers Ratio` = round(`Non-movers` / (`Non-movers` + `Movers`) * 100, digits = 2),
      `Migrants Ratio` = round(`Migrants` / (`Non-movers` + `Movers`) * 100, digits = 2),
      `Non-Migrants Ratio` = round(`Non-migrants` / (`Non-movers` + `Movers`) * 100, digits = 2),
      `Internal Migrants Ratio` = round(`Internal migrants` / (`Non-movers` + `Movers`) * 100, digits = 2),
      `Intraprovincial Migrants Ratio` = round(`Intraprovincial migrants` / (`Non-movers` + `Movers`) * 100, digits = 2),
      `Interprovincial Migrants Ratio` = round(`Interprovincial migrants` / (`Non-movers` + `Movers`) * 100, digits = 2),
      `External Migrants Ratio` = round(`External migrants` / (`Non-movers` + `Movers`) * 100, digits = 2)
    )

  saveRDS(censusData, here::here("data", paste0("census2016-mobility-", censusLevel, ".rds")))

  censusDataGathered <- censusData %>%
    mutate(`Region` = as.character(`Region`), Type = as.character(Type)) %>%
    gather(
      "Non-Movers Ratio", "Non-Migrants Ratio", "External Migrants Ratio",
      "Intraprovincial Migrants Ratio", "Interprovincial Migrants Ratio",
      key = "Migration", value = "count") %>%
    select(GeoUID, Region, Migration, count, geometry)

  saveRDS(censusDataGathered, here::here("data", paste0("census2016-mobility-", censusLevel, "-gathered.rds")))

  censusMobilitySeq <- censusData %>%
  gather(
    "Non-Movers Ratio", "Non-Migrants Ratio", "External Migrants Ratio",
    "Intraprovincial Migrants Ratio", "Interprovincial Migrants Ratio",
    key = "Migration", value = "count") %>%
  select(GeoUID, Region, Migration, count) %>%
  mutate(
    "Movers" = ifelse(Migration == "Non-Movers Ratio", "", "Movers"),
    "Migrants" = ifelse(
      Migration %in% c("External Migrants Ratio", "Intraprovincial Migrants Ratio", "Interprovincial Migrants Ratio"),
      "Migrants",
      ""
    ),
    "Internal migrants" = ifelse(
      Migration %in% c("Intraprovincial Migrants Ratio", "Interprovincial Migrants Ratio"),
      "Internal migrants",
      ""
    # ),
    # Migration = str_replace(
    #   str_replace(Migration, " Ratio", ""),
    #   " Migrants",
    #   ""
    ),
    "sequence" = str_replace(
      str_replace(
        paste(Movers, Migrants, `Internal migrants`, str_replace(Migration, "-", " "), sep = "-"),
        "-{2,}", "-"
      ),
      "^-", ""
    )
  ) %>%
  select(GeoUID, sequence, count)
  st_geometry(censusMobilitySeq) <- NULL

  saveRDS(censusMobilitySeq, here::here("data", paste0("census2016-mobility-", censusLevel, "-seq.rds")))

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

  # sf format
  censusStirData %<>%
    filter(Type == censusLevel) %<>%
    ms_simplify(keep = 0.1, keep_shapes = TRUE) %<>%
    select(
      "Region" = as.character(ifelse("Type" %in% c("CSD", "CT", "DA"),
                                   paste("Region.Name", str_sub("GeoUID", -2)), "Region.Name")),
      "GeoUID", "Type", "geometry",
      "total_households_with_income" = "v_CA16_4886",
      "stir_less_than_30" = "v_CA16_4887",
      "stir_more_than_30" = "v_CA16_4888"
    ) %<>%
    mutate(
      percent_less_than_30 =
        round(stir_less_than_30 / (stir_less_than_30 + stir_more_than_30) * 100, digits = 2),
      percent_more_than_30 =
        round(stir_more_than_30 / (stir_less_than_30 + stir_more_than_30) * 100, digits = 2)
    ) %<>%
    arrange(desc(percent_more_than_30))

  censusStirData$Region <- as.character(censusStirData$`Region`)

  # Reorder data
  censusStirData$GeoUID <- factor(
    censusStirData$GeoUID,
    levels = unique(censusStirData$GeoUID)[order(
      censusStirData$percent_more_than_30, decreasing = FALSE
    )]
  )

  saveRDS(censusStirData, here::here("data", paste0("census2016Spatial-stir-", censusLevel, ".rds")))
}

# Age and Sex - Average Age
for (censusLevel in c("CMA", "CD", "CSD", "CT", "DA")) {
  censusData <-
    get_census(
      "CA16",
      level = censusLevel,
      regions = regions,
      vectors = c("v_CA16_379"),
      use_cache = TRUE,
      labels = "short",
      geo_format = "sf"
    )

  censusData %<>%
    select(
      -one_of(c("Shape Area", "name", "Adjusted Population (previous Census)",
                "ruid", "C_UID", "PR_UID", "Area (sq km)"))
    ) %<>%
    filter(Type == censusLevel) %<>%
    ms_simplify(keep = 0.1, keep_shapes = TRUE) %<>%
    rename("Average Age" = "v_CA16_379") %<>%
    replace_na("Average Age" = 0)

  censusData %<>%
    mutate(`Region` = as.character(`Region.Name`), Type = as.character(Type)) %<>%
    filter("Average Age" > 0)

  saveRDS(censusData, here::here("data", paste0("census2016-avg-age-", censusLevel, ".rds")))
}


# Age and Sex - Population pyramid
# cancensus
## Census 2016 has 5 year bands up to 100 years of age and over,
## while census 2011 and 2006 bands go up to 85 and over
## We will group 2016 data in the same bands for comparison purposes
# Loop through year and geographical levels and save general census-related data
for (year in c("2006", "2011", "2016")) {
  censusYear <- paste0('CA', substr(paste0(year), 3, 4))
  print(paste("Fetching data for ", censusYear))
  switch(
    year,
    "2016" = {
      print(paste("Setting vectors inside 2016 "))
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
    },
    "2011" = {
      print(paste("Setting vectors inside 2011 "))
      vectorsFemale <- c(
        "v_CA11F_10", "v_CA11F_13", "v_CA11F_16", "v_CA11F_19", "v_CA11F_28", "v_CA11F_40", "v_CA11F_43",
        "v_CA11F_46", "v_CA11F_49", "v_CA11F_52", "v_CA11F_55", "v_CA11F_58", "v_CA11F_61", "v_CA11F_64",
        "v_CA11F_67", "v_CA11F_70", "v_CA11F_73", "v_CA11F_76"
      )
      vectorsMale <- c(
        "v_CA11F_9", "v_CA11F_12", "v_CA11F_15", "v_CA11F_18", "v_CA11F_27", "v_CA11F_39", "v_CA11F_42",
        "v_CA11F_45", "v_CA11F_48", "v_CA11F_51", "v_CA11F_54", "v_CA11F_57", "v_CA11F_60", "v_CA11F_63",
        "v_CA11F_66", "v_CA11F_69", "v_CA11F_72", "v_CA11F_75"
      )
    },
    "2006" = {
      print(paste("Setting vectors inside 2006 "))
      vectorsFemale <- c(
        "v_CA06_23", "v_CA06_24", "v_CA06_25", "v_CA06_26", "v_CA06_27", "v_CA06_28", "v_CA06_29",
        "v_CA06_30", "v_CA06_31", "v_CA06_32", "v_CA06_33", "v_CA06_34", "v_CA06_35", "v_CA06_36",
        "v_CA06_37", "v_CA06_38", "v_CA06_39", "v_CA06_40"
      )
      vectorsMale <- c(
        "v_CA06_4", "v_CA06_5", "v_CA06_6", "v_CA06_7", "v_CA06_8", "v_CA06_9", "v_CA06_10",
        "v_CA06_11", "v_CA06_12", "v_CA06_13", "v_CA06_14", "v_CA06_15", "v_CA06_16", "v_CA06_17",
        "v_CA06_18", "v_CA06_19", "v_CA06_20", "v_CA06_21"
      )
    }
  )

  for (censusLevel in c("CMA", "CD", "CSD", "CT", "DA", "PR")) {
    print(paste("Now getting geo-level ", censusLevel, " for ", censusYear))
    # ppData <- getPopulationPyramidData(censusYear, censusLevel, ppVectorsFemale, ppVectorsMale, regions)

    # Female population
    censusPPFemale <-
      get_census(
        censusYear, level = censusLevel, regions = regions, vectors = vectorsFemale,
        use_cache = TRUE, labels = "short", geo_format = NA
      )
    censusPPFemale %<>%
      filter(Type == censusLevel) %<>%
      mutate(sex = "female")
    if (year == "2016") {
      censusPPFemale %<>% select(
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
    } else {
      censusPPFemale %<>% select(
        GeoUID, Type, `Region Name`, sex,
        "0 to 4 years" = vectorsFemale[1], "5 to 9 years" = vectorsFemale[2], "10 to 14 years" = vectorsFemale[3],
        "15 to 19 years" = vectorsFemale[4], "20 to 24 years" = vectorsFemale[5], "25 to 29 years" = vectorsFemale[6],
        "30 to 34 years" = vectorsFemale[7], "35 to 39 years" = vectorsFemale[8], "40 to 44 years" = vectorsFemale[9],
        "45 to 49 years" = vectorsFemale[10], "50 to 54 years" = vectorsFemale[11], "55 to 59 years" = vectorsFemale[12],
        "60 to 64 years" = vectorsFemale[13], "65 to 69 years" = vectorsFemale[14], "70 to 74 years" = vectorsFemale[15],
        "75 to 79 years" = vectorsFemale[16], "80 to 84 years" = vectorsFemale[17], "85 years and over" = contains(vectorsFemale[18])
      )
    }
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
      mutate(sex = "male")
    if (year == 2016) {
      censusPPMale %<>% select(
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
    } else {
      censusPPMale %<>% select(
        GeoUID, Type, `Region Name`, sex,
        "0 to 4 years" = vectorsMale[1], "5 to 9 years" = vectorsMale[2], "10 to 14 years" = vectorsMale[3],
        "15 to 19 years" = vectorsMale[4], "20 to 24 years" = vectorsMale[5], "25 to 29 years" = vectorsMale[6],
        "30 to 34 years" = vectorsMale[7], "35 to 39 years" = vectorsMale[8], "40 to 44 years" = vectorsMale[9],
        "45 to 49 years" = vectorsMale[10], "50 to 54 years" = vectorsMale[11], "55 to 59 years" = vectorsMale[12],
        "60 to 64 years" = vectorsMale[13], "65 to 69 years" = vectorsMale[14], "70 to 74 years" = vectorsMale[15],
        "75 to 79 years" = vectorsMale[16], "80 to 84 years" = vectorsMale[17], "85 years and over" = contains(vectorsMale[18])
      )
    }
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
      mutate(
        age = ifelse(
          age %in% (c("85 to 89 years", "90 to 94 years", "95 to 99 years", "100 years and over")),
          "85 years and over",
          age
        )
      ) %>%
      group_by(GeoUID, Type, Region, sex, age) %>%
      summarise(
        population = sum(population)
      ) %>%
      mutate(percentage = round(population / sum(population) / 2 * 100, digits = 2)) %>%
      mutate(percentage = ifelse(sex == "male", percentage * -1, percentage)) %>%
      mutate(ageStartYear = parse_number(age)) %>%
      ungroup() %>%
      arrange(GeoUID, sex, ageStartYear)

    # Rearrange for proper sorting when plotting
    # censusPP$age <- factor(censusPP$age, levels = unique(censusPP$age)[order(censusPP$ageStartYear, decreasing = FALSE)])
    censusPP$ageStartYear <- factor(censusPP$ageStartYear, levels = unique(censusPP$ageStartYear)[order(censusPP$ageStartYear, decreasing = FALSE)])
    censusPP$age <- factor(censusPP$age, levels = unique(censusPP$age)[order(censusPP$ageStartYear, decreasing = FALSE)])

    # Drop unnecessary columns
    censusPP %<>% select(-one_of("Type", "population"))

    saveRDS(censusPP, here::here("data", "population_pyramid", paste0("census", year, "-pp-", censusLevel, ".rds")))
  }
}
