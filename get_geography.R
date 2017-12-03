library(tidyverse)
library(data.table)
library(readxl)
library(plotly)
library(stringr)

# get Canadian geography atributes from
# http://www12.statcan.gc.ca/census-recensement/2016/geo/ref/gaf/files-fichiers/2016_92-151_XBB_csv.zip
# colnames have been added locally
geo <- fread("./data/2016_92-151_XBB_csv/2016_92-151_XBB.csv")

# Filter BC geo and drop superfluous columns
dropColumns <- c("DBir2016", "DARPlamx", "DARPlamy", "DARPlat", "DARPlong", "Prname", "Prfname", "Prfabbr", "FEDuid",
                 "FEDname", "SACtype", "SACcode", "CCSuid", "CCSname", "DPLuid", "DPLname", "DPLtype")
geo <- geo %>% 
  filter(Pruid == "59") %>% 
  select(-one_of(dropColumns))

# Discard French parts of ER and CMA names
geo$Ername <- gsub("--", "-", gsub(" / .*", "", geo$Ername, perl = TRUE))
geo$CMAname <- gsub(" / .*", "", geo$CMAname, perl = TRUE)
saveRDS(geo, "data/geo.rds")

# Economic regions
geoER <- geo %>% 
  select(Pruid, Prename, Preabbr, Eruid, Ername) %>%
  distinct()
saveRDS(geoER, "data/geoER.rds")

# Census divisions
geoCD <- geo %>% 
  select(Pruid, Prename, Preabbr, Cduid, Cdname, Cdtype) %>%
  distinct()
saveRDS(geoCD, "data/geoCD.rds")

# Census sub-divisions
geoCSD <- geo %>% 
  select(Pruid, Prename, Preabbr, CSDuid, CSDname, CSDtype) %>%
  distinct()
saveRDS(geoCSD, "data/geoCSD.rds")

# Census metropolitan areas
geoCMA <- geo %>% 
  select(Pruid, Prename, Preabbr, CMAPuid, CMAuid, CMAname, CMAtype) %>%
  distinct()
saveRDS(geoCMA, "data/geoCMA.rds")

# Census geo reference - all units
geoRef <- geo %>%
  select(Pruid, Prename, Preabbr, Eruid, Ername, Cduid, Cdname, Cdtype, 
         CMAPuid, CMAuid, CMAname, CMAtype, CSDuid, CSDname, CSDtype) %>%
  distinct()
saveRDS(geoRef, "data/geoRef.rds")

