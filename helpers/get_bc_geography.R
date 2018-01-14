library(here)
library(dplyr)
library(readr)
library(stringr)
devtools::install_github("mountainmath/cancensus")
library(cancensus)

options(cancensus.api_key = "CensusMapper_f17c13c7fc5e60de7cdd341d5d4db866")
options(cancensus.cache_path = here("cache"))

censusDataset <- "CA16"

shapesCMA <- get_census_geometry(
  dataset = censusDataset, regions = list(PR = "59"), level = 'CMA', geo_format = "sf"
)
shapesCMA <- shapesCMA %>%
  filter(Type == "CMA")

shapesCD <- get_census_geometry(
  dataset = censusDataset, regions = list(PR = "59"), level = 'CD', geo_format = "sf"
)

shapesCSD <- get_census_geometry(
  dataset = censusDataset, regions = list(PR = "59"), level = 'CSD', geo_format = "sf"
)

shapesCT <- get_census_geometry(
  dataset = censusDataset, regions = list(PR = "59"), level = 'CT', geo_format = "sf"
)

shapesCDA <- get_census_geometry(
  dataset = censusDataset, regions = list(PR = "59"), level = 'DA', geo_format = "sf"
)

geoCD$CDuid <- as.character(geoCD$CDuid)
cd <- dplyr::inner_join(geoCD, shapesCD, by = c("CDuid" = "GeoUID"))

geoCSD$CSDuid <- as.character(geoCSD$CSDuid)
csd <- dplyr::inner_join(geoCSD, shapesCSD, by = c("CSDuid" = "GeoUID"))

geoCMA$CMAuid <- as.character(geoCMA$CMAuid)
cma <- dplyr::inner_join(geoCMA, shapesCMA, by = c("CMAuid" = "GeoUID"))


# get Canadian geography atributes from
# http://www12.statcan.gc.ca/census-recensement/2016/geo/ref/gaf/files-fichiers/2016_92-151_XBB_csv.zip
# colnames have been added locally
if(!file.exists(here("data", "2016_92-151_XBB.csv"))){
  fileUrl <- "http://www12.statcan.gc.ca/census-recensement/2016/geo/ref/gaf/files-fichiers/2016_92-151_XBB_csv.zip"
  download.file(fileUrl, destfile = here::here("data", "CAgeography.zip"))
  unzip(zipfile = here::here("data", "CAgeography.zip"), files = "2016_92-151_XBB.csv", exdir = here("data"))
}
geo <- read_csv(here::here("data", "2016_92-151_XBB.csv"))

split_by_separator <- function(v, sep) {
  splitV <- str_split(v, sep, simplify = TRUE)
  return(splitV[,1])
}

# Fix colnames to persist EN colname
colnames(geo) <- split_by_separator(colnames(geo), "/")

# Drop superfluous columns and filter BC geo
dropColumns <- c(
  "DBir2016", "DArplamx", "DArplamy", "DArplat", "DArplong", "PRname", "PRfname",
  "PRfabbr", "FEDuid", "FEDname", "SACtype", "SACcode", "CCSuid", "CCSname",
  "DPLuid", "DPLname", "DPLtype"
)
geo <- geo %>%
  filter(PRuid == "59") %>%
  select(-one_of(dropColumns))

# Discard French parts of ER and CMA names
geo <- geo %>%
  mutate(
    ERname = gsub("--", "-", split_by_separator(geo$ERname, " / ")),
    CMAname = gsub("--", "-", split_by_separator(geo$CMAname, " / ")),
    POPCTRRAname = gsub("--", "-", split_by_separator(geo$POPCTRRAname, " / "))
  )
saveRDS(geo, here::here("data", "geo.rds"))

# Economic regions
geoER <- geo %>%
  select(PRuid, PRename, PReabbr, ERuid, ERname) %>%
  distinct()
saveRDS(geoER, "data/geoER.rds")

# Census divisions
geoCD <- geo %>%
  select(PRuid, PRename, PReabbr, CDuid, CDname, CDtype) %>%
  distinct()
saveRDS(geoCD, "data/geoCD.rds")

# Census sub-divisions
geoCSD <- geo %>%
  select(PRuid, PRename, PReabbr, CSDuid, CSDname, CSDtype) %>%
  distinct()
saveRDS(geoCSD, "data/geoCSD.rds")

# Census metropolitan areas
geoCMA <- geo %>%
  select(PRuid, PRename, PReabbr, CMAPuid, CMAuid, CMAname, CMAtype) %>%
  distinct()
saveRDS(geoCMA, "data/geoCMA.rds")

# Census geo reference - all units
geoRef <- geo %>%
  select(PRuid, PRename, PReabbr, ERuid, ERname, CDuid, CDname, CDtype,
         CMAPuid, CMAuid, CMAname, CMAtype, CSDuid, CSDname, CSDtype) %>%
  distinct()
saveRDS(geoRef, "data/geoRef.rds")
