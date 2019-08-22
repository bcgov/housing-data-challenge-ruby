# 01. Load packages ----
library(tidyverse)
library(here)
library(usethis)
library(bcdata)
library(bchousing)

# 02. Discover data in Open Data Catalogue
bcdata::bcdc_search("property transfer tax 2019")
ptt2019_record_id <- 'b35d45d1-e468-4aec-b8af-e1c1cb24bc07'
bcdata::bcdc_get_data(ptt2019_record_id)

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

# 02. Get data from Open Data Catalogue ----
ptt_pr_bcdc <- bcdata::bcdc_get_data(
  record = ptt2019_record_id,
  resource = '622b9cf1-6278-419d-94a1-986da85b27fe'
)
ptt_dr_bcdc <- bcdata::bcdc_get_data(
  record = ptt2019_record_id,
  resource = '48a2b056-3a9f-4264-9bc7-73ae1835889d'
)
ptt_rd_bcdc <- bcdata::bcdc_get_data(
  record = ptt2019_record_id,
  resource = 'a9ed06f8-ab11-40f7-aa4f-748e45229566'
)
ptt_mn_bcdc <- bcdata::bcdc_get_data(
  record = ptt2019_record_id,
  resource = '75ed8fec-f7ff-4d74-9257-fe36f697de8f'
)

# 03. Wrangle PTT data ----
ptt_pr <- ptt_pr_bcdc %>% bchousing::WranglePttData()
ptt_dr <- ptt_dr_bcdc %>% bchousing::WranglePttData()
ptt_rd <- ptt_rd_bcdc %>% bchousing::WranglePttData()
ptt_mn <- ptt_mn_bcdc %>% bchousing::WranglePttData()


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

# 04. Add geometries to PTT data ----
ptt_dr_sf <- JoinPttShapes(ptt_data = ptt_dr, shapes = shapes_dr, geo_name = "DevelopmentRegion")
ptt_rd_sf <- JoinPttShapes(ptt_data = ptt_rd, shapes = shapes_rd, geo_name = "RegionalDistrict")
ptt_mn_sf <- JoinPttShapes(ptt_data = ptt_mn, shapes = shapes_mun, geo_name = "Municipality")

# Remove unnecessary objects
# rm(list = c(
#     'ptt_dr', 'ptt_dr_raw', 'ptt_rd', 'ptt_rd_raw', 'ptt_mun', 'ptt_mun_raw', 'shapes_dr',
#     'shapes_rd', 'shapes_mun', 'shapes_dr_raw', 'shapes_mun_raw', 'shapes_rd_raw'
#   )
# )

# 05. Join 2019 data to previous data

# 06. Set up dashboard data
# Last month for which PTT data is available
max_trans_period <- max(ptt_prov$trans_period)

ptt_prov_dash <- ptt_prov %>%
  filter(trans_period == max_trans_period) %>%
  select(tot_mkt_trans, no_foreign_perc, sum_FMV, sum_FMV_foreign_perc) %>%
  mutate(max_trans_period = max_trans_period)

# 08. Save .rda files into data directory ----
usethis::use_data(ptt_dr_sf, overwrite = TRUE, compress = "gzip")
usethis::use_data(ptt_rd_sf, overwrite = TRUE, compress = "gzip")
usethis::use_data(ptt_mun_sf, overwrite = TRUE, compress = "gzip")
usethis::use_data(ptt_prov_dash, overwrite = TRUE)

