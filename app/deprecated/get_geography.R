library(here)

# Economic regions
geoER <- readRDS(here::here("data", "geoER.rds"))

# Census divisions
geoCD <- readRDS(here::here("data", "geoCD.rds"))

# Census sub-divisions
geoCSD <- readRDS(here::here("data", "geoCSD.rds"))

# Census metropolitan areas
geoCMA <- readRDS(here::here("data", "geoCMA.rds"))

# Census geo reference - all units
geoRef <- readRDS(here::here("data", "geoRef.rds"))
