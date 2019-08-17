# 01. Load packages ----
library(tidyverse)
library(here)
library(usethis)
library(bcdata)
library(bchousing)

# 02. Load prepared csv files ----
# ptt_dr_raw <- read_csv(here::here("data_raw", "ptt", "DR_all_years.csv"))
# ptt_rd_raw <- read_csv(here::here("data_raw", "ptt", "Regional_all_years.csv"))
# ptt_mun_raw <- read_csv(
#   file = here::here("data_raw", "ptt", "Municipal_all_years.csv"),
#   na = c("", "NA", "nr"),
#   guess_max = 7561
# )

# 03. Load shape files ----
# Economic regions
# shapes_dr_raw <- here::here("source_data", "shapes", "2016", "ler_000a16a_e", "ler_000a16a_e.shp")

# Regional Districts
# shapes_rd_raw <- here::here("source_data", "shapes", "2016", "lcd_000a16a_e", "lcd_000a16a_e.shp")

# Municipaltities
# shapes_mun_raw <- here::here("source_data", "shapes", "2016", "lcsd000a16a_e", "lcsd000a16a_e.shp")

# 05. Wrangle PTT data files ----
# ptt_dr <- WranglePttData(ptt_dr_raw)
# ptt_rd <- WranglePttData(ptt_rd_raw)
# ptt_mun <- WranglePttData(ptt_mun_raw)

# 06. Wrangle shape files ----
# shapes_dr <- WrangleShapeFiles(shapes_dr_raw, id_column = 'ERUID', name_column = ERNAME)
# shapes_rd <- WrangleShapeFiles(shapes_rd_raw, id_column = 'CDUID', name_column = CDNAME)
# shapes_mun <- WrangleShapeFiles(shapes_mun_raw, id_column = 'CSDUID', name_column = CSDNAME)

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

# 07. Add geometries to PTT data ----
# ptt_dr_sf <- JoinPttShapes(ptt_data = ptt_dr, shapes = shapes_dr, geo_name = "DevelopmentRegion")
# ptt_rd_sf <- JoinPttShapes(ptt_data = ptt_rd, shapes = shapes_rd, geo_name = "RegionalDistrict")
# ptt_mun_sf <- JoinPttShapes(ptt_data = ptt_mun, shapes = shapes_mun, geo_name = "Municipality")

# Remove unnecessary objects
# rm(list = c(
#     'ptt_dr', 'ptt_dr_raw', 'ptt_rd', 'ptt_rd_raw', 'ptt_mun', 'ptt_mun_raw', 'shapes_dr',
#     'shapes_rd', 'shapes_mun', 'shapes_dr_raw', 'shapes_mun_raw', 'shapes_rd_raw'
#   )
# )

# Get Province-level data using bcdata package
bcdata::bcdc_search("property transfer tax 2019")
ptt2019_record_id <- 'b35d45d1-e468-4aec-b8af-e1c1cb24bc07'
# bcdata::bcdc_get_data(ptt2019_record_id)
ptt_prov_bcdc <- bcdc_get_data('b35d45d1-e468-4aec-b8af-e1c1cb24bc07', resource = '622b9cf1-6278-419d-94a1-986da85b27fe')
ptt_prov <- ptt_prov_bcdc %>%
  dplyr::rename(
    year = Year,
    month = Month
  ) %>%
    bchousing::AddTransactionPeriodColumns() %>%
  mutate(
    no_foreign_perc = round(n_foreign_tran / tot_mkt_trans * 100, 2),
    sum_FMV_foreign_perc = round(sum_FMV_foreign / sum_FMV * 100, 2)
  )

# Last month for which PTT data is available
max_trans_period <- max(ptt_prov$trans_period)

ptt_prov_dash <- ptt_prov %>%
  filter(trans_period == max_trans_period) %>%
  select(tot_mkt_trans, no_foreign_perc, sum_FMV, sum_FMV_foreign_perc) %>%
  mutate(max_trans_period = max_trans_period)

# 08. Save .rda files into data directory ----
# usethis::use_data(ptt_dr_sf, overwrite = TRUE, compress = "gzip")
# usethis::use_data(ptt_rd_sf, overwrite = TRUE, compress = "gzip")
# usethis::use_data(ptt_mun_sf, overwrite = TRUE, compress = "gzip")
usethis::use_data(ptt_prov_dash, overwrite = TRUE)

