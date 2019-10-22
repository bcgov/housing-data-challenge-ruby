# 01. Load packages ----
library(tidyverse)
library(here)
library(usethis)
library(bcdata)
library(bchousing)
library(sf)

# Set to TRUE after initial discovery. It saves the data as .rds files into /data-raw dir
# so that they are not downloaded each time while doing dev work with the script.
# Set to FALSE when new data needs to be retrieved from the BC Data Catalogue.
use_cached_data = FALSE

# 02. Discover data in BC Open Data Catalogue ----
# bcdata::bcdc_search("property transfer tax 2019")
ptt2019_record_id <- 'b35d45d1-e468-4aec-b8af-e1c1cb24bc07'
# bcdata::bcdc_get_data(ptt2019_record_id)

# ****************************
# RELEVANT DATASETS DISCOVERED
# ****************************
#
# 7) PROVINCIAL_MONTHLY_2019
# format: csv
# url: https://catalogue.data.gov.bc.ca/dataset/b35d45d1-e468-4aec-b8af-e1c1cb24bc07/resource/622b9cf1-6278-419d-94a1-986da85b27fe/download/provincial_monthly_2019.csv
# resource: 622b9cf1-6278-419d-94a1-986da85b27fe
# code: bcdc_get_data(record = 'b35d45d1-e468-4aec-b8af-e1c1cb24bc07', resource = '622b9cf1-6278-419d-94a1-986da85b27fe')

# 11) DEVELOPMENT_REGION_MONTHLY_2019
# format: csv
# url: https://catalogue.data.gov.bc.ca/dataset/b35d45d1-e468-4aec-b8af-e1c1cb24bc07/resource/48a2b056-3a9f-4264-9bc7-73ae1835889d/download/development_region_monthly_2019.csv
# resource: 48a2b056-3a9f-4264-9bc7-73ae1835889d
# code: bcdc_get_data(record = 'b35d45d1-e468-4aec-b8af-e1c1cb24bc07', resource = '48a2b056-3a9f-4264-9bc7-73ae1835889d')

# 9) REGIONAL_DISTRICT_MONTHLY_2019
# format: csv
# url: https://catalogue.data.gov.bc.ca/dataset/b35d45d1-e468-4aec-b8af-e1c1cb24bc07/resource/a9ed06f8-ab11-40f7-aa4f-748e45229566/download/regional_district_monthly_2019.csv
# resource: a9ed06f8-ab11-40f7-aa4f-748e45229566
# code: bcdc_get_data(record = 'b35d45d1-e468-4aec-b8af-e1c1cb24bc07', resource = 'a9ed06f8-ab11-40f7-aa4f-748e45229566')

# 14) MUNICIPAL_MONTHLY_2019
# format: csv
# url: https://catalogue.data.gov.bc.ca/dataset/b35d45d1-e468-4aec-b8af-e1c1cb24bc07/resource/75ed8fec-f7ff-4d74-9257-fe36f697de8f/download/municipal_monthly_2019.csv
# resource: 75ed8fec-f7ff-4d74-9257-fe36f697de8f
# code: bcdc_get_data(record = 'b35d45d1-e468-4aec-b8af-e1c1cb24bc07', resource = '75ed8fec-f7ff-4d74-9257-fe36f697de8f')

# 03. Get data from Open Data Catalogue ----
if (use_cached_data == TRUE & file.exists("data-raw/ptt_pr_2019.rds")) {
  ptt_pr_bcdc <- readRDS("data-raw/ptt_pr_2019.rds")
} else {
  ptt_pr_bcdc <- bcdata::bcdc_get_data(
    record = ptt2019_record_id,
    resource = '622b9cf1-6278-419d-94a1-986da85b27fe',
    na = c("", "NA", "nr")
  )
  saveRDS(ptt_pr_bcdc, "data-raw/ptt_pr_2019.rds")
}

if (use_cached_data == TRUE & file.exists("data-raw/ptt_dr_2019.rds")) {
  ptt_dr_bcdc <- readRDS("data-raw/ptt_dr_2019.rds")
} else {
  ptt_dr_bcdc <- bcdata::bcdc_get_data(
    record = ptt2019_record_id,
    resource = '48a2b056-3a9f-4264-9bc7-73ae1835889d',
    na = c("", "NA", "nr")
  )
  saveRDS(ptt_dr_bcdc, "data-raw/ptt_dr_2019.rds")
}

if (use_cached_data == TRUE & file.exists("data-raw/ptt_rd_2019.rds")) {
  ptt_rd_bcdc <- readRDS("data-raw/ptt_rd_2019.rds")
} else {
  ptt_rd_bcdc <- bcdata::bcdc_get_data(
    record = ptt2019_record_id,
    resource = 'a9ed06f8-ab11-40f7-aa4f-748e45229566',
    na = c("", "NA", "nr")
  )
  saveRDS(ptt_rd_bcdc, "data-raw/ptt_rd_2019.rds")
}

if (use_cached_data == TRUE & file.exists("data-raw/ptt_mn_2019.rds")) {
  ptt_mn_bcdc <- readRDS("data-raw/ptt_mn_2019.rds")
} else {
  ptt_mn_bcdc <- bcdata::bcdc_get_data(
    record = ptt2019_record_id,
    resource = '75ed8fec-f7ff-4d74-9257-fe36f697de8f',
    na = c("", "NA", "nr"),
    guess_max = 10000
  )
  saveRDS(ptt_mn_bcdc, "data-raw/ptt_mn_2019.rds")
}

# 04. Wrangle PTT data ----
ptt_pr_2019 <- ptt_pr_bcdc %>% bchousing::WranglePttData()
ptt_dr_2019 <- ptt_dr_bcdc %>% bchousing::WranglePttData(calculate_percentages = FALSE)
ptt_rd_2019 <- ptt_rd_bcdc %>% bchousing::WranglePttData(calculate_percentages = FALSE, region_name_split = TRUE)
ptt_mn_2019 <- ptt_mn_bcdc %>% bchousing::WranglePttData(calculate_percentages = FALSE, region_name_split = TRUE)

# 05. Define functions to align past and current data before joining ----
FixPastData <- function(data, name_column) {
  # If 2019 data was already imported, filter that out
  data <- data %>%
    filter(
      trans_period < '2019-01-01'
    )

  data <- data %>%
    select(-c(GeoUID, PRUID, PRNAME)) %>%
    rename(!!name_column := GeoName) %>%
    mutate(
      n_tot_corp_tran = as.numeric(NA),
      n_tot_indv_tran = as.numeric(NA),
      n_res_strata_rental = as.numeric(NA),
      n_res_unkn = as.numeric(NA),
      n_comm_industry = as.numeric(NA),
      n_comm_util = as.numeric(NA),
      n_comm_unkn = as.numeric(NA),
      n_forest_tran = as.numeric(NA),
      n_foreign_comm = as.numeric(NA),
      n_foreign_oth_unk = as.numeric(NA),
      n_foreign_corp_tran = as.numeric(NA),
      n_foreign_indv_tran = as.numeric(NA),
      perc_n_foreign_res = as.numeric(NA),
      perc_FMV_foreign_res = as.numeric(NA),
      sum_invest_foreign_res = as.numeric(NA),
      perc_invest_foreign_res = as.numeric(NA),
      n_lt1M_foreign_res = as.numeric(NA),
      n_gt1M_foreign_res = as.numeric(NA),
      n_gt3M_foreign_res = as.numeric(NA)
    ) %>%
  mutate_if(is.numeric, list(~na_if(., "NaN")))
  sf::st_geometry(data) <- NULL
  return(data)
}

FixCurrentData <- function(data) {
  data <- data %>%
    rename(
      'no_res_trans' = 'n_res_trans',
      'n_res_1fam_dwelling' = 'n_res_1fam',

    ) %>%
    mutate(
      n_res_other = n_res_strata_rental + n_res_unkn,
      n_comm_other = n_comm_industry + n_comm_util + n_comm_unkn,
      n_foreign_nonres = n_foreign_comm + n_foreign_oth_unk,
      n_res_1fam_suite = as.numeric(NA),
      n_res_1fam_vacant_res = as.numeric(NA),
      n_res_1fam_other = as.numeric(NA),
      n_res_strata_row = as.numeric(NA),
      n_res_strata_other = as.numeric(NA),
      no_foreign_perc = 0,
      sum_FMV_foreign_perc = 0
    ) %>%
    mutate_if(is.numeric, list(~na_if(., "NaN")))
  return(data)
}

#  06. Fix past data ----
ptt_dr_past <- FixPastData(ptt_dr_sf, name_column = 'DevelopmentRegion')
ptt_rd_past <- FixPastData(ptt_rd_sf, name_column = 'RegionalDistrict')
ptt_mn_past <- FixPastData(ptt_mun_sf, name_column = 'Municipality')

# 07. Fix current data ----
ptt_dr_2019 <- FixCurrentData(ptt_dr_2019)
ptt_rd_2019 <- FixCurrentData(ptt_rd_2019) %>%
  mutate(
    RegionalDistrict = as.character(RegionalDistrict),
    # RegionalDistrict = dplyr::if_else(RegionalDistrict == 'Mount Waddington', 'Central Coast-Mount Waddington', RegionalDistrict),
    RegionalDistrict = dplyr::if_else(RegionalDistrict == 'Powell River', 'qathet', RegionalDistrict),
    RegionalDistrict = dplyr::if_else(RegionalDistrict == 'Qathet', 'qathet', RegionalDistrict)
  )
ptt_mn_2019 <- FixCurrentData(ptt_mn_2019)

# 08. Join past and current data ----
ptt_dr_j <- full_join(ptt_dr_past, ptt_dr_2019)
ptt_rd_j <- full_join(ptt_rd_past, ptt_rd_2019)
ptt_mn_j <- full_join(ptt_mn_past, ptt_mn_2019)

# ptt_rd_j <- ptt_rd_j %>%
#   mutate(
#     RegionalDistrict = dplyr::if_else(RegionalDistrict == 'Mount Waddington', 'Central Coast-Mount Waddington', RegionalDistrict),
#     RegionalDistrict = dplyr::if_else(RegionalDistrict == 'Powell River', 'qathet', RegionalDistrict)
#   )

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


# 09. Add geometries ----
ptt_dr_sf <- JoinPttShapes(
    ptt_data = ptt_dr_j,
    shapes = shapes_dr,
    geo_name = "DevelopmentRegion"
  ) %>%
  mutate_if(is.numeric, list(~na_if(., "NaN")))

ptt_rd_sf <- JoinPttShapes(
    ptt_data = ptt_rd_j,
    shapes = shapes_rd,
    geo_name = "RegionalDistrict"
  ) %>%
  mutate_if(is.numeric, list(~na_if(., "NaN")))

ptt_mun_sf <- JoinPttShapes(
    ptt_data = ptt_mn_j,
    shapes = shapes_mun,
    geo_name = "Municipality"
  ) %>%
  mutate_if(is.numeric, list(~na_if(., "NaN")))

# 10. Set up dashboard data ----
# Last month for which PTT data is available
max_trans_period <- max(ptt_pr_2019$trans_period)

ptt_prov_dash <- ptt_pr_2019 %>%
  filter(trans_period == max_trans_period) %>%
  select(tot_mkt_trans, perc_n_foreign_res, sum_FMV, perc_FMV_foreign_res) %>%
  mutate(
    max_trans_period = max_trans_period,
    perc_n_foreign_res = perc_n_foreign_res * 100,
    perc_FMV_foreign_res = perc_FMV_foreign_res * 100
  ) %>%
  rename(
    no_foreign_perc = perc_n_foreign_res,
    sum_FMV_foreign_perc = perc_FMV_foreign_res
  )

# 11. Save .rda files into data directory ----
usethis::use_data(ptt_dr_sf, overwrite = TRUE, compress = "gzip")
usethis::use_data(ptt_rd_sf, overwrite = TRUE, compress = "gzip")
usethis::use_data(ptt_mun_sf, overwrite = TRUE, compress = "gzip")
usethis::use_data(ptt_prov_dash, overwrite = TRUE)
