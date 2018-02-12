library(here)
library(httr)
library(jsonlite)
library(dplyr)
library(tidyr)
library(stringr)
library(lubridate)
library(magrittr)
library(sf)
library(rmapshaper)

options(stringsAsFactors = FALSE)
Sys.setenv(TZ = "America/Vancouver")

# Function to split string by separator and return the first part
# used to preserve English names of geographical regions
split_by_separator <- function(v, sep) {
  splitV <- str_split(v, sep, simplify = TRUE)
  return(splitV[,1])
}

# Function to fix PTT data:
# - replace NA and string "NA" with 0
# - convert numeric columns from char to numeric
wranglePttData <- function(data, columns) {
  # replace NA with 0
  data[is.na(data)] <- 0
  # replace "NA" with 0
  data <- as_tibble(apply(data, 2, function(x) {x[x == "NA"] <- 0; x}))
  # cast numeric columns
  for(column in columns) {
    data[[column]] <- as.numeric(data[[column]])
  }
  # add ratio columns
  data %<>%
    mutate(no_foreign_perc = round(no_foreign / no_mkt_trans, 4) * 100) %>%
    mutate(sum_FMV_foreign_perc = round(sum_FMV_foreign / sum_FMV, 4) * 100)

  return(data)
}

# Read in shape files
# Economic regions
shapeFile <- here::here("source_data", "shapes", "2016", "ler_000a16a_e", "ler_000a16a_e.shp")
shapesEr <- st_as_sf(st_read(shapeFile, stringsAsFactors = FALSE))
shapesEr %<>%
  filter(PRUID == "59") %>%
  rename("GeoUID" = "ERUID") %>%
  mutate(
    "ERNAME" = split_by_separator(ERNAME, "/"),
    "PRNAME" = split_by_separator(PRNAME, "/")
  )
Ers <- shapesEr %>% pull(ERNAME)

# Province
shapesProv <- shapesEr %>% group_by(PRUID) %>% summarise()

# Regional Districts
shapeFile <- here::here("source_data", "shapes", "2016", "lcd_000a16a_e", "lcd_000a16a_e.shp")
shapesCd <- st_as_sf(st_read(shapeFile, stringsAsFactors = FALSE))
shapesCd %<>%
  filter(PRUID == "59") %>%
  rename("GeoUID" = "CDUID") %>%
  mutate(
    "CDNAME" = split_by_separator(CDNAME, "/"),
    "PRNAME" = split_by_separator(PRNAME, "/")
  )
Cds <- shapesCd %>% pull(CDNAME)

# Municipaltities
shapeFile <- here::here("source_data", "shapes", "2016", "lcsd000a16a_e", "lcsd000a16a_e.shp")
shapesCsd <- st_as_sf(st_read(shapeFile, stringsAsFactors = FALSE))
shapesCsd %<>%
  filter(PRUID == "59") %>%
  rename("GeoUID" = "CSDUID") %>%
  mutate(
    "CSDNAME" = split_by_separator(CSDNAME, "/"),
    "PRNAME" = split_by_separator(PRNAME, "/")
  )
Csds <- shapesCsd %>% pull(CSDNAME)

# Get Property Transfer Tax data
# 2016
# Provincial monthly
pttData <- httr::GET("https://catalogue.data.gov.bc.ca/api/action/datastore_search?resource_id=099288eb-4a09-43eb-8dc2-72586808f70f&limit=10000")
pttResult <- jsonlite::fromJSON(content(pttData, as="text"),  flatten=TRUE)
pttProv2016 <- pttResult$result$records

# Development Region monthly
pttData <- httr::GET("https://catalogue.data.gov.bc.ca/api/action/datastore_search?resource_id=8e38e8ee-a30b-4d7d-a2b4-9463a2b509d5&limit=10000")
pttResult <- jsonlite::fromJSON(content(pttData, as="text"),  flatten=TRUE)
pttDevReg2016 <- pttResult$result$records

# Regional District monthly
pttData <- httr::GET("https://catalogue.data.gov.bc.ca/api/action/datastore_search?resource_id=1ae2299f-8ce5-4af1-9dab-3d4e8586a77a&limit=10000")
pttResult <- jsonlite::fromJSON(content(pttData, as="text"),  flatten=TRUE)
pttRegDis2016 <- pttResult$result$records

# Municipality monthly
pttData <- httr::GET("https://catalogue.data.gov.bc.ca/api/action/datastore_search?resource_id=6abca1b9-2eab-4d0d-8350-d27d69af3258&limit=10000")
pttResult <- jsonlite::fromJSON(content(pttData, as="text"),  flatten=TRUE)
pttMun2016 <- pttResult$result$records

# 2017
# Provincial monthly
pttData <- httr::GET("https://catalogue.data.gov.bc.ca/api/action/datastore_search?resource_id=5624cec4-5574-4505-8225-e14814b84a33&limit=10000")
pttResult <- jsonlite::fromJSON(content(pttData, as="text"),  flatten=TRUE)
pttProv2017 <- pttResult$result$records

# Development Region monthly
pttData <- httr::GET("https://catalogue.data.gov.bc.ca/api/action/datastore_search?resource_id=00d2d1df-d0dd-4a34-86aa-5e3789d7b2da&limit=10000")
pttResult <- jsonlite::fromJSON(content(pttData, as="text"),  flatten=TRUE)
pttDevReg2017 <- pttResult$result$records

# Regional District monthly
pttData <- httr::GET("https://catalogue.data.gov.bc.ca/api/action/datastore_search?resource_id=60cf5065-710e-43da-b3ae-5bf38b78405e&limit=10000")
pttResult <- jsonlite::fromJSON(content(pttData, as="text"),  flatten=TRUE)
pttRegDis2017 <- pttResult$result$records

# Municipality monthly
pttData <- httr::GET("https://catalogue.data.gov.bc.ca/api/action/datastore_search?resource_id=1fc4ffad-b50a-4b41-bcbf-51d3d0f6beb8&limit=10000")
pttResult <- jsonlite::fromJSON(content(pttData, as="text"),  flatten=TRUE)
pttMun2017 <- pttResult$result$records

# Join columns to catch different column names
pttProv2016Cols <- pttResult$result$fields
pttDevReg2016Cols <- pttResult$result$fields
pttRegDis2016Cols <- pttResult$result$fields
pttMun2016Cols <- pttResult$result$fields
pttProv2017Cols <- pttResult$result$fields
pttDevReg2017Cols <- pttResult$result$fields
pttRegDis2017Cols <- pttResult$result$fields
pttMun2017Cols <- pttResult$result$fields
pttProvCols <- dplyr::full_join(pttProv2016Cols, pttProv2017Cols, by = c("id"))
pttDevRegCols <- dplyr::full_join(pttDevReg2016Cols, pttDevReg2017Cols, by = c("id"))
pttRegDisCols <- dplyr::full_join(pttRegDis2016Cols, pttRegDis2017Cols, by = c("id"))
pttMunCols <- dplyr::full_join(pttMun2016Cols, pttMun2017Cols, by = c("id"))

# Wrangle data to align column and region names
pttProv2016$trans_period <- substr(pttProv2016$trans_period, 1, 10)
pttDevReg2016$trans_period <- substr(pttDevReg2016$trans_period, 1, 10)
pttRegDis2016$trans_period <- substr(pttRegDis2016$trans_period, 1, 10)
pttMun2016$trans_period <- substr(pttMun2016$trans_period, 1, 10)

# Align 2016 and 2017 columns for clean row binding
pttProv2017 %<>%
  mutate("trans_period" = paste0(Year, "-", if_else(as.integer(Month) < 10, "0", ""), Month, "-01")) %>%
  rename(no_mkt_trans = tot_mkt_trans) %>%
  select(-one_of(c("Year", "Month", "perc_no_foreign_res", "sum_FMV_res", "sum_FMV_foreign_res", "perc_FMV_foreign_res")))

pttDevReg2017 %<>%
  mutate("trans_period" = paste0(Year, "-", if_else(as.integer(Month) < 10, "0", ""), Month, "-01")) %>%
  rename(no_mkt_trans = tot_mkt_trans) %>%
  select(-one_of(c("Year", "Month")))

pttRegDis2017 %<>%
  mutate("trans_period" = paste0(Year, "-", if_else(as.integer(Month) < 10, "0", ""), Month, "-01")) %>%
  rename(no_mkt_trans = tot_mkt_trans) %>%
  select(-one_of(c("Year", "Month", "no_foreign_res", "no_foreign_nonres", "perc_no_foreign_res", "sum_FMV_res",
                   "sum_FMV_foreign_res", "sum_invest_foreign_res", "perc_FMV_foreign_res", "perc_invest_foreign_res")))

pttMun2017 %<>%
  mutate("trans_period" = paste0(Year, "-", if_else(as.integer(Month) < 10, "0", ""), Month, "-01")) %>%
  rename(no_mkt_trans = tot_mkt_trans) %>%
  select(-one_of(c("Year", "Month")))

# Join 2016 and 2017 data
pttProv = dplyr::bind_rows(pttProv2016, pttProv2017)
pttDevReg = dplyr::bind_rows(pttDevReg2016, pttDevReg2017)
pttRegDis = dplyr::bind_rows(pttRegDis2016, pttRegDis2017)
pttMun = dplyr::bind_rows(pttMun2016, pttMun2017)

# Clean NAs and cast columns to numeric
numericColumns <- setdiff(colnames(pttDevReg), c("trans_period", "_id", "DevelopmentRegion"))
pttProv <- wranglePttData(pttProv, numericColumns)
pttDevReg <- wranglePttData(pttDevReg, numericColumns)
pttRegDis <- wranglePttData(pttRegDis, numericColumns)
pttMun <- wranglePttData(pttMun, numericColumns)

# Align region names with shapes files
pttDevRegLocations <- pttDevReg %>% select(DevelopmentRegion) %>% distinct() %>% pull(DevelopmentRegion)
pttRegDisLocations <- pttRegDis %>% select(RegionalDistrict) %>% distinct() %>% pull(RegionalDistrict)
pttMunLocations <- pttMun %>% select(Municipality) %>% distinct() %>% pull(Municipality)

shapesEr %<>%
  mutate(
    ERNAME = str_trim(ERNAME, side = c("both")),
    ERNAME = str_replace(ERNAME, "Lower Mainland--Southwest", "Lower Mainland - Southwest"),
    ERNAME = str_replace(ERNAME, "Thompson--Okanagan", "Thompson-Okanagan"),
    GeoUID = if_else(GeoUID == "5970", "5960", GeoUID),
    ERNAME = if_else(ERNAME == "North Coast" | ERNAME == "Nechako", "Nechako & North Coast", ERNAME)
  ) %>%
  group_by(GeoUID)
pttDevReg %<>%
  mutate(
    DevelopmentRegion = str_replace(DevelopmentRegion, "Mainland/Southwest", "Lower Mainland - Southwest"),
    DevelopmentRegion = str_replace(DevelopmentRegion, "Thompson/Okanagan", "Thompson-Okanagan"),
    DevelopmentRegion = str_replace(DevelopmentRegion, "Vancouver Island/Coast", "Vancouver Island and Coast")
  )

shapesCd %<>%
  mutate(
    GeoUID = if_else(GeoUID == "5943", "5945", GeoUID),
    CDNAME = if_else(CDNAME == "Central Coast" | CDNAME == "Mount Waddington", "Central Coast-Mount Waddington", CDNAME),
    GeoUID = if_else(GeoUID == "5955", "5959", GeoUID),
    CDNAME = if_else(CDNAME == "Northern Rockies" | CDNAME == "Peace River", "Northern Rockies-Peace River", CDNAME)
  ) %>%
  group_by(GeoUID)
pttRegDis %<>%
  mutate(
    RegionalDistrict = str_replace(RegionalDistrict, "COLUMBIA SHUSWAP", "Columbia-Shuswap"),
    RegionalDistrict = str_replace(RegionalDistrict, "METRO VANCOUVER", "Greater Vancouver"),
    RegionalDistrict = str_to_title(RegionalDistrict)
  )

pttMun %<>%
  mutate(
    Municipality = str_to_title(Municipality),
    Municipality = str_replace(Municipality, "City Of ", ""),
    Municipality = str_replace(Municipality, "District Of ", "")
  )

shapesProv %<>% mutate("PRUID" = "59", PRNAME = "British Columbia")
pttProv %<>% mutate("PRUID" = "59", PRNAME = "British Columbia")

# Join data with shape files
pttDevRegSf <- dplyr::inner_join(shapesEr, pttDevReg, by = c("ERNAME" = "DevelopmentRegion"))
pttRegDisSf <- dplyr::inner_join(shapesCd, pttRegDis, by = c("CDNAME" = "RegionalDistrict"))
pttMunSf <- dplyr::inner_join(shapesCsd, pttMun, by = c("CSDNAME" = "Municipality"))
pttProvSf <- dplyr::inner_join(shapesProv, pttProv, by = c("PRUID")) %>% select(-one_of(c("PRNAME.y"))) %>% rename(PRNAME = PRNAME.x)

# Rename location name column
pttDevRegSf %<>% ungroup() %>% rename(Location = ERNAME)
pttRegDisSf %<>% ungroup() %>% rename(Location = CDNAME)
pttMunSf %<>% ungroup() %>% rename(Location = CSDNAME)
pttProvSf %<>% ungroup() %>% rename(Location = PRNAME)

# Simplify and change projection
pttDevRegSf %<>% st_transform(crs=4326)# %>% ms_simplify(keep = 0.1, keep_shapes = TRUE)
pttRegDisSf %<>% sf::st_transform(ptData, crs=4326)
pttMunSf %<>% sf::st_transform(ptData, crs=4326)
pttProvSf %<>% sf::st_transform(ptData, crs=4326)

# Save RDS
saveRDS(pttProvSf, here::here("data", "ptt-province.rds"))
saveRDS(pttDevRegSf, here::here("data", "ptt-development-region.rds"))
saveRDS(pttRegDisSf, here::here("data", "ptt-regional-district.rds"))
saveRDS(pttMunSf, here::here("data", "ptt-municipality.rds"))

