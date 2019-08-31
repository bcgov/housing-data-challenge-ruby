# 01. Load packages ----
library(tidyverse)
library(here)
library(usethis)
library(bcdata)
library(bchousing)

# 02. Load prepared csv files ----
ptt_dr_raw <- read_csv(here::here("data-raw", "ptt", "DR_all_years.csv"), na = c("", "NA", "nr"))
ptt_rd_raw <- read_csv(here::here("data-raw", "ptt", "Regional_all_years.csv"), na = c("", "NA", "nr"))
ptt_mun_raw <- read_csv(
  file = here::here("data-raw", "ptt", "Municipal_all_years.csv"),
  na = c("", "NA", "nr"),
  guess_max = 7561
)

# 03. Load shape files ----
# Economic regions
shapes_dr_raw <- here::here("data-raw", "shapes", "2016", "ler_000a16a_e", "ler_000a16a_e.shp")
# Regional Districts
shapes_rd_raw <- here::here("data-raw", "shapes", "2016", "lcd_000a16a_e", "lcd_000a16a_e.shp")
# Municipaltities
shapes_mun_raw <- here::here("data-raw", "shapes", "2016", "lcsd000a16a_e", "lcsd000a16a_e.shp")

# 04. Wrangle PTT data files ----
ptt_dr <- bchousing::WranglePttData(ptt_dr_raw, calculate_percentages = TRUE)
ptt_rd <- bchousing::WranglePttData(ptt_rd_raw, calculate_percentages = TRUE)
ptt_mun <- bchousing::WranglePttData(ptt_mun_raw, calculate_percentages = TRUE)

# 05. Wrangle shape files ----
shapes_dr <- bchousing::WrangleShapeFiles(shapes_dr_raw, id_column = 'ERUID', name_column = ERNAME)
shapes_rd <- bchousing::WrangleShapeFiles(shapes_rd_raw, id_column = 'CDUID', name_column = CDNAME)
shapes_mun <- bchousing::WrangleShapeFiles(shapes_mun_raw, id_column = 'CSDUID', name_column = CSDNAME)

# Examine values of Municipality name and shape Municipality for records that match and can be joined
# Shapes, not PTT
# ptt_mun_snp <- dplyr::left_join(shapes_mun, ptt_mun, by = c("GeoName" = "Municipality")) %>%
#   filter(is.na(RegionalDistrict)) %>%
#   distinct(GeoUID, GeoName)
# st_geometry(ptt_mun_snp) <- NULL
#
# # PTT, not shapes
# ptt_mun_pns <- dplyr::right_join(shapes_mun, ptt_mun, by = c("GeoName" = "Municipality")) %>%
#   select(DevelopmentRegion, RegionalDistrict, GeoName)
# st_geometry(ptt_mun_pns) <- NULL
# ptt_mun_pns <- ptt_mun_pns %>%
#   distinct(DevelopmentRegion, RegionalDistrict, GeoName)
#
# # Both PTT and shapes
# ptt_mun_ps <- dplyr::inner_join(shapes_mun, ptt_mun, by = c("GeoName" = "Municipality")) %>%
#   select(DevelopmentRegion, RegionalDistrict, GeoName)
# st_geometry(ptt_mun_ps) <- NULL
# ptt_mun_ps <- ptt_mun_ps %>%
#   distinct(DevelopmentRegion, RegionalDistrict, GeoName)

# 06. Add geometries to PTT data ----
ptt_dr_sf <- bchousing::JoinPttShapes(ptt_data = ptt_dr, shapes = shapes_dr, geo_name = "DevelopmentRegion")
ptt_rd_sf <- bchousing::JoinPttShapes(ptt_data = ptt_rd, shapes = shapes_rd, geo_name = "RegionalDistrict")
ptt_mun_sf <- bchousing::JoinPttShapes(ptt_data = ptt_mun, shapes = shapes_mun, geo_name = "Municipality")

# 07. Save .rda files into data directory ----
usethis::use_data(ptt_dr_sf, overwrite = TRUE, compress = "gzip")
usethis::use_data(ptt_rd_sf, overwrite = TRUE, compress = "gzip")
usethis::use_data(ptt_mun_sf, overwrite = TRUE, compress = "gzip")

# 08. Save .rda files for boundaries, to be used for future data
usethis::use_data(shapes_dr, overwrite = TRUE, compress = "gzip")
usethis::use_data(shapes_rd, overwrite = TRUE, compress = "gzip")
usethis::use_data(shapes_mun, overwrite = TRUE, compress = "gzip")
