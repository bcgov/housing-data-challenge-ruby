library(tidyverse)
library(dplyr)
library(magrittr)
library(lubridate)
library(here)
library(rgdal)
library(leaflet)
library(sf)
library(sp)
library(TSP)
library(rmapshaper)

# PROPERTY TRANSFER TAX DATA
ptt2016dr <- read_csv(here::here("data", "propertytax", "2016", "development-region-monthly.csv"))
ptt2016m <- read_csv(here::here("data", "propertytax", "2016", "municipal-monthly.csv"))
ptt2016rd <- read_csv(here::here("data", "propertytax", "2016", "regional-district-monthly.csv"))
ptt2017dr <- read_csv(here::here("data", "propertytax", "2017", "developmentregionmonthly2017.csv"))
ptt2017m <- read_csv(here::here("data", "propertytax", "2017", "municipalitymonthly2017.csv"))
ptt2017rd <- read_csv(here::here("data", "propertytax", "2017", "regionaldistrictmonthly2017.csv"))

## Columns required per project specification
# trans_period, DevelopmentRegion, RegionalDistrict, Municipality, no_mkt_trans,
# no_resid_trans, no_resid_acreage_trans, resid_comm_count, no_resid_farm,
# no_resid_fam, no_res_1fam, no_resid_strata, no_resid_non_strata, no_resid_other,
# no_comm_tot, no_comm_comm, no_comm_strata_nores, no_comm_other, no_recr_tot,
# no_farm_tot, no_unkn_tot, sum_FMV, mn_FMV, md_FMV, sum_PPT_paid, md_PPT, no_foreign,
# sum_FMV_foreign, mn_FMV_foreign, md_FMV_foreign, add_tax_paid

# Mutate 2017 data to consolidate year and month and select required columns
# Development Region
ptt2017dr %<>%
  mutate(trans_period = ymd(paste(Year, Month, 1))) %>%
  select(
    trans_period, DevelopmentRegion, no_mkt_trans = tot_mkt_trans,
    no_resid_trans, no_resid_acreage_trans, resid_comm_count, no_resid_farm,
    no_resid_fam, no_res_1fam, no_resid_strata, no_resid_non_strata, no_resid_other,
    no_comm_tot, no_comm_comm, no_comm_strata_nores, no_comm_other, no_recr_tot,
    no_farm_tot, no_unkn_tot, sum_FMV, mn_FMV, md_FMV, sum_PPT_paid, md_PPT, no_foreign,
    sum_FMV_foreign, mn_FMV_foreign, md_FMV_foreign, add_tax_paid
  )
# Municipality
ptt2017m %<>%
  mutate(trans_period = ymd(paste(Year, Month, 1))) %>%
  select(
    trans_period, DevelopmentRegion, RegionalDistrict, Municipality, no_mkt_trans = tot_mkt_trans,
    no_resid_trans, no_resid_acreage_trans, resid_comm_count, no_resid_farm,
    no_resid_fam, no_res_1fam, no_resid_strata, no_resid_non_strata, no_resid_other,
    no_comm_tot, no_comm_comm, no_comm_strata_nores, no_comm_other, no_recr_tot,
    no_farm_tot, no_unkn_tot, sum_FMV, mn_FMV, md_FMV, sum_PPT_paid, md_PPT, no_foreign,
    sum_FMV_foreign, mn_FMV_foreign, md_FMV_foreign, add_tax_paid
  )
# Regional District
ptt2017rd %<>%
  mutate(trans_period = ymd(paste(Year, Month, 1))) %>%
  select(
    trans_period, DevelopmentRegion, RegionalDistrict, no_mkt_trans = tot_mkt_trans,
    no_resid_trans, no_resid_acreage_trans, resid_comm_count, no_resid_farm,
    no_resid_fam, no_res_1fam, no_resid_strata, no_resid_non_strata, no_resid_other,
    no_comm_tot, no_comm_comm, no_comm_strata_nores, no_comm_other, no_recr_tot,
    no_farm_tot, no_unkn_tot, sum_FMV, mn_FMV, md_FMV, sum_PPT_paid, md_PPT, no_foreign,
    sum_FMV_foreign, mn_FMV_foreign, md_FMV_foreign, add_tax_paid
  )

# Mutate 2016 data to consolidate year and month and select required columns
# Development Region
ptt2016dr %<>%
  mutate(trans_period = paste0(
    Year, "-", ifelse(Month < 10, paste0("0", Month), Month)
  )) %>%
  select(
    trans_period, DevelopmentRegion, no_mkt_trans = tot_mkt_trans,
    no_resid_trans, no_resid_acreage_trans, resid_comm_count, no_resid_farm,
    no_resid_fam, no_res_1fam, no_resid_strata, no_resid_non_strata, no_resid_other,
    no_comm_tot, no_comm_comm, no_comm_strata_nores, no_comm_other, no_recr_tot,
    no_farm_tot, no_unkn_tot, sum_FMV, mn_FMV, md_FMV, sum_PPT_paid, md_PPT, no_foreign,
    sum_FMV_foreign, mn_FMV_foreign, md_FMV_foreign, add_tax_paid
  )
# Municipality
ptt2016m %<>%
  mutate(trans_period = paste0(
    Year, "-", ifelse(Month < 10, paste0("0", Month), Month)
  )) %>%
  select(
    trans_period, DevelopmentRegion, RegionalDistrict, Municipality, no_mkt_trans = tot_mkt_trans,
    no_resid_trans, no_resid_acreage_trans, resid_comm_count, no_resid_farm,
    no_resid_fam, no_res_1fam, no_resid_strata, no_resid_non_strata, no_resid_other,
    no_comm_tot, no_comm_comm, no_comm_strata_nores, no_comm_other, no_recr_tot,
    no_farm_tot, no_unkn_tot, sum_FMV, mn_FMV, md_FMV, sum_PPT_paid, md_PPT, no_foreign,
    sum_FMV_foreign, mn_FMV_foreign, md_FMV_foreign, add_tax_paid
  )
# Regional District
ptt2016rd %<>%
  mutate(trans_period = paste0(
    Year, "-", ifelse(Month < 10, paste0("0", Month), Month)
  )) %>%
  select(
    trans_period, DevelopmentRegion, RegionalDistrict, no_mkt_trans = tot_mkt_trans,
    no_resid_trans, no_resid_acreage_trans, resid_comm_count, no_resid_farm,
    no_resid_fam, no_res_1fam, no_resid_strata, no_resid_non_strata, no_resid_other,
    no_comm_tot, no_comm_comm, no_comm_strata_nores, no_comm_other, no_recr_tot,
    no_farm_tot, no_unkn_tot, sum_FMV, mn_FMV, md_FMV, sum_PPT_paid, md_PPT, no_foreign,
    sum_FMV_foreign, mn_FMV_foreign, md_FMV_foreign, add_tax_paid
  )
# Combine 2016 and 2017 data into corresponding geographical level and save Rds objects
pttDr <- bind_rows(ptt2016dr, ptt2017dr)
pttM <- bind_rows(ptt2016m, ptt2017m)
pttRd <- bind_rows(ptt2016rd, ptt2017rd)
# saveRDS(pttDr, here("data", "rds", "ptt", "pttDr.rds"))
# saveRDS(pttM, here("data", "rds", "ptt", "pttM.rds"))
# saveRDS(pttRd, here("data", "rds", "ptt", "pttRd.rds"))

#' split_by_separator
#' split vector of strings by separator and return first part
#'
#' @param v
#' @param sep
#'
#' @return
split_by_separator <- function(v, sep) {
  splitV <- str_split(v, sep, simplify = TRUE)
  return(splitV[,1])
}

#' saveShapesRds
#' Reads shapes (.shp) file from the source and saves it into destination rds file
#'
#' @param shapeFilePath source file path to read shapes
#' @param layerName layer name
#' @param rdsFilePath destination file path
saveShapesRds <- function(shapeFilePath, layerName, rdsFilePath) {
  shapes <-
    readOGR(
      shapeFilePath,
      layer = layerName,
      verbose = FALSE
    )

  # convert to simple features
  shapes <- st_as_sf(shapes)

  # subset censusDivs to filter out provinces other than BC
  shapes %<>%
    filter("PRNAME" == "British Columbia / Colombie-Britannique") %>%
    ms_simplify(keep = 0.1) %>%
    row_number()

  # Save serialized object as rds
  saveRDS(shapes, rdsFilePath)
}

# Census Divisions
saveShapesRds(
  here::here("data", "shapes", "2016", "lcd_000a16a_e", "lcd_000a16a_e.shp"),
  layerName = "lcd_000a16a_e",
  here::here("data", "rds", "shapes", "shapesCsd.rds")
)

#  Census Economic Regions
saveShapesRds(
  here::here("data", "shapes", "2016", "ler_000a16a_e", "ler_000a16a_e.shp"),
  layerName = "ler_000a16a_e",
  here::here("data", "rds", "shapes", "shapesEr.rds")
)

#  Census Subdivisions
saveShapesRds(
  here::here("data", "shapes", "2016", "lcsd000a16a_e", "lcsd000a16a_e.shp"),
  layerName = "lcsd000a16a_e",
  here::here("data", "rds", "shapes", "bc2011Subdivisions.rds")
)

#  Census Tracts
saveShapesRds(
  here::here("data", "shapes", "2016", "lct_000a16a_e", "lct_000a16a_e.shp"),
  layerName = "lct_000a16a_e",
  here::here("data", "rds", "shapes", "bc2011Tracts.rds")
)

#  Census Dissimination Areas
saveShapesRds(
  here::here("data", "shapes", "2016", "lda_000a16a_e", "lda_000a16a_e.shp"),
  layerName = "lda_000a16a_e",
  here::here("data", "rds", "shapes", "bc2011DisseminationAreas.rds")
)

#  Census Metropolitan Areas
saveShapesRds(
  here::here("data", "shapes", "2016", "lcma000a16a_e", "lcma000a16a_e.shp"),
  layerName = "lcma000a16a_e",
  here::here("data", "rds", "shapes", "bc2011MetropolitanAreas.rds")
)

geoCma <- st_read(
  here::here("data", "shapes", "2016", "lcma000a16a_e", "lcma000a16a_e.shp"),
  stringsAsFactors = FALSE
)
geoCma %<>% filter(PRUID == 59)

geoEr <- st_read(
  here::here("data", "shapes", "2016", "ler_000a16a_e", "ler_000a16a_e.shp"),
  stringsAsFactors = FALSE
)
geoEr %<>%
  filter(PRUID == 59) %>%
  mutate(ERNAME = gsub("--", "-", split_by_separator(ERNAME, " / "))) %>%
  mutate(PRNAME = split_by_separator(PRNAME, " / ")) %>%
  mutate(ERNAME = if_else(
    ERNAME %in% c("North Coast", "Nechako"),
    "Nechako & North Coast",
    ERNAME
    )
  ) %>%
  group_by(PRUID, PRNAME, ERNAME) %>%
  summarise()
str(geoEr)

pttDr %<>%
  mutate(
    DevelopmentRegion = gsub(
      "Mainland/Southwest",
      "Lower Mainland-Southwest",
      gsub(
        "Thompson/Okanagan",
        "Thompson-Okanagan",
        gsub(
          "Vancouver Island/Coast",
          "Vancouver Island and Coast",
          DevelopmentRegion
        )
      )
    )
  ) #%>%
  # na.omit()

period <- "2017-05-01"
pttDrP <- pttDr %>%
  filter(trans_period == period)
pttGeoDr <- inner_join(geoEr, pttDrP, by = c("ERNAME" = "DevelopmentRegion"))# = "ERNAME"))
pttGeoDr[is.na(pttGeoDr)] <- 0
str(pttGeoDr)

library(mapview)
mapview(pttGeoDr, col.regions = sf.colors(8))

# viridis
getPal <- function(pal, dom, bins) {
  colorNumeric(palette = pal,
           domain = dom#,
           # bins = bins
           )
}

palViridis <-
  getPal(
    viridis::viridis(4),
    pttGeoDr$no_mkt_trans,
    c(0, 100, 250, 500, 1000, 2500, 5000, 10000, Inf)
  )

# pttGeoDr %>%
  # st_transform(crs = 4326) %>%
  # as("Spatial") %>%
  leaflet(sf::st_transform(pttGeoDr, crs=4326)) %>%
  addProviderTiles(provider = "CartoDB.Positron") %>%
  addPolygons(
    label = ~ pttGeoDr$ERNAME,
    # color = ~ pal(pttGeoDr$no_mkt_trans),
    stroke = TRUE,
    weight = 1,
    fillOpacity = 0.5,
    smoothFactor = 1,
    color = '#333',
    fillColor = ~ palViridis(pttGeoDr$no_mkt_trans),
    highlight = highlightOptions(
      weight = 5,
      color = "#696969",
      dashArray = "",
      fillOpacity = 0.5,
      bringToFront = TRUE)
  ) %>%
  addLegend(
    "bottomleft",
    pal = palViridis,
    values = ~ pttGeoDr$no_mkt_trans,
    title = "Number of market transactions",
    opacity = 0.5
  )

library(plotly)
  plot_ly(

    pttDr,
    x = ~ no_mkt_trans,
    y = ~ DevelopmentRegion,
    type = "bar",
    orientation = "h"
  ) %>%
    layout(
      #"Number of market transactions",
      # xaxis = axisFormat,
      # yaxis = axisFormat,
      # margin = marginFormatMonthly,
      barmode = 'group'#,
      # legend = legendFormat
    ) %>%
    config(displayModeBar = F)


  plot_ly(
    pttDr %>% group_by(trans_period, DevelopmentRegion)
    # %>% summarise(
    #   "no_mkt_trans"=sum("no_mkt_trans"),
    #   "no_resid_trans"=sum("no_resid_trans"),
    #   "no_resid_acreage_trans"=sum("no_resid_acreage_trans"),
    #   "resid_comm_count"=sum("resid_comm_count"),
    #   "no_resid_farm"=sum("no_resid_farm"),
    #   "no_resid_fam"=sum("no_resid_fam"),
    #   "no_res_1fam"=sum("no_res_1fam"),
    #   "no_resid_strata"=sum("no_resid_strata"),
    #   "no_resid_non_strata"=sum("no_resid_non_strata"),
    #   "no_resid_other"=sum("no_resid_other"),
    #   "no_comm_tot"=sum("no_comm_tot"),
    #   "no_comm_comm"=sum("no_comm_comm"),
    #   "no_comm_strata_nores"=sum("no_comm_strata_nores"),
    #   "no_comm_other"=sum("no_comm_other"),
    #   "no_recr_tot"=sum("no_recr_tot"),
    #   "no_farm_tot"=sum("no_farm_tot"),
    #   "no_unkn_tot"=sum("no_unkn_tot"),
    #   "sum_FMV"=sum("sum_FMV"),
    #   # "mn_FMV"=sum("mn_FMV"),
    #   # "md_FMV"=sum("md_FMV"),
    #   "sum_PPT_paid"=sum("sum_PPT_paid"),
    #   # "md_PPT"=sum("md_PPT"),
    #   "no_foreign"=sum("no_foreign"),
    #   "sum_FMV_foreign"=sum("sum_FMV_foreign"),
    #   # "mn_FMV_foreign"=sum("mn_FMV_foreign"),
    #   # "md_FMV_foreign"=sum("md_FMV_foreign"),
    #   "add_tax_paid"=sum("add_tax_paid")
    # ) %>% ungroup()
    ,
    x = ~ trans_period,
    y = ~ sum_FMV,
    name = "Total FMV",
    type = 'bar'
    # mode = 'marker',
    # line = list(shape = "spline", color = "#00003369")
  # ) %>%
  #   add_lines(
  #     y = ~ sum_FMV_foreign,
  #     name = "Total FMV Foreign",
  #     line = list(shape = "spline", color = "#33000069")
    # ) %>%
    # add_lines(
    #   y = ~ mn_FMV,
    #   name = "Foreign %",
    #   yaxis = "y2",
    #   line = list(
    #     shape = "spline",
    #     color = "#33000069",
    #     dash = 'dot'
    #   )
      )
    # ) %>%
    # layout(
    #   title = "FMV (Fair Market Value)",
    #   # xaxis = axisFormat,
    #   # yaxis = axisFormat,
    #   yaxis2 = list(
    #     # tickfont = tickfontRd,
    #     overlaying = "y",
    #     side = "right",
    #     title = "Foreign %"
    #   )#,
    #   # margin = marginFormat,
    #   # legend = legendFormat
    # )
    #
