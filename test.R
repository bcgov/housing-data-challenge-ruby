library(shiny)
library(rgdal)
library(leaflet)
library(maps)
library(dplyr)
library(htmlwidgets)
library(DT)
library(rgeos)
library(tidyr)

# read objects
# property tax
propertyTax <- readRDS("./data/propertyTax.rds")
ptRegDisMth <- readRDS("./data/pt-regional-district-monthly.rds")
ptRegDisWk <- readRDS("./data/pt-regional-district-weekly.rds")
ptMunMth <- readRDS("./data/pt-municipal-monthly.rds")
ptDevRegMth <- readRDS("./data/pt-development-region-monthly.rds")
ptDevRegWk <- readRDS("./data/pt-development-region-weekly.rds")

# shapefiles
bcCensusDivs <- readRDS("./data/bc2011CensusDivisions.rds")
bcCensusEconRegs <- readRDS("./data/bc2011EconomicRegions.rds")
bcCensusTracts <- readRDS("./data/bc2011Tracts.rds")
bcCensusConsSubdivs <- readRDS("./data/bc2011ConsolidatedSubdivisions.rds")
bcCensusSubdivs <- readRDS("./data/bc2011Subdivisions.rds")
bcCensusDissAreas <- readRDS("./data/bc2011DisseminationAreas.rds")
bcCensusMetroArea <- readRDS("./data/bc2011MetropolitanAreas.rds")
bcCensusPopCentres <- readRDS("./data/bc2011PopulationCentres.rds")

data <- bcCensusPopCentres@data

# Selection of transaction periods
transPeriods <-
    sort(as.vector(propertyTax[!duplicated(propertyTax[, c("trans_period")]),
                               c("trans_period")]),
         decreasing = TRUE)

propertyTaxPeriod <-
    subset(propertyTax, trans_period == max(transPeriods))

# # Convert join columns to uppercase to avoid mismatches due to case sensitivity
# bcCensusDivs@data$CDNAME <- toupper(bcCensusDivs@data$CDNAME)
# 
# bcCensusDivsMap <-
#     merge(
#         bcCensusDivs,
#         propertyTaxPeriod,
#         by.x = "CDNAME",
#         by.y = "Regional.District",
#         sort = FALSE,
#         by = ALL
#     )

pt_resolution <- "mun"
pt_trans_period <- "2016-12-01"

switch(pt_resolution, 
   "regdis" = {
        propertyTaxPeriod <-
           subset(ptRegDisMth, trans_period == pt_trans_period)
        # Convert join columns to uppercase to avoid mismatches due to case sensitivity
        bcCensusDivs@data$CDNAME <- toupper(bcCensusDivs@data$CDNAME)
        geoUnit <- as.character(bcCensusDivs$CDNAME)
        
        shapesDF <-
           merge(
               bcCensusDivs,
               propertyTaxPeriod,
               by.x = "CDNAME",
               by.y = "Regional.District",
               sort = FALSE,
               by = ALL
           )
    },
   "devreg" = {
       propertyTaxPeriod <-
           subset(ptDevRegMth, trans_period == pt_trans_period)
       # Convert join columns to uppercase to avoid mismatches due to case sensitivity
       
       erData <- bcCensusEconRegs@data
       
       erData <- separate(data = erData, col = ERNAME, into = c("ERNAME_E", "ERNAME_F"), sep = "\\/", fill = "right")
       erData$ERNAME_E <- trimws(toupper(gsub("--", "/", erData$ERNAME_E)))

       setdiff(
           toupper(erData$ERNAME_E), propertyTaxPeriod$DevelopmentRegion
       )
       # [1] "VANCOUVER ISLAND AND COAST" "KOOTENAY"        "NECHAKO"
       # [4] "LOWER MAINLAND/SOUTHWEST "   "NORTH COAST "               
       
       setdiff(
           propertyTaxPeriod$DevelopmentRegion, toupper(erData$ERNAME_E)
       )
       # [1] "MAINLAND/SOUTHWEST"     "NORTHEAST"              "VANCOUVER ISLAND/COAST"
       
       propertyTaxPeriod$DevelopmentRegion <- 
           gsub("MAINLAND/SOUTHWEST", "LOWER MAINLAND/SOUTHWEST", propertyTaxPeriod$DevelopmentRegion)
       propertyTaxPeriod$DevelopmentRegion <- 
           gsub("VANCOUVER ISLAND/COAST", "VANCOUVER ISLAND AND COAST", propertyTaxPeriod$DevelopmentRegion)
       
       bcCensusEconRegs@data$ERNAME <- erData$ERNAME_E
       
       geoUnit <- as.character(bcCensusEconRegs$ERNAME)
       shapesDF <-
           merge(
               bcCensusEconRegs,
               propertyTaxPeriod,
               by.x = "ERNAME",
               by.y = "DevelopmentRegion",
               sort = FALSE,
               by = ALL
           )
   },
   "mun" = {
       propertyTaxPeriod <-
           subset(ptMunMth, trans_period == pt_trans_period)
       # Convert join columns to uppercase to avoid mismatches due to case sensitivity
       bcCensusMetroArea@data$CMANAME <- toupper(bcCensusMetroArea@data$CMANAME)
       
       propertyTaxPeriod$ <- 
       
       rm(data)
       data <- bcCensusSubdivs@data
       
       propertyTaxPeriod$Municipality <- 
           gsub("ABBOTSFORD", "ABBOTSFORD - MISSION", propertyTaxPeriod$Municipality)
       
       
       geoUnit <- as.character(bcCensusMetroArea$CMANAME)
       shapesDF <-
           merge(
               bcCensusMetroArea,
               propertyTaxPeriod,
               by.x = "CMANAME",
               by.y = "Municipality",
               sort = FALSE,
               by = ALL
           )
   }
)

# pal <- colorBin("YlGnBu", bins = 5, bcCensusDivsMap$no_mkt_trans)
pal <- colorQuantile("YlOrRd", n = 7, as.integer(shapesDF$no_mkt_trans))
data <- shapesDF@data
#initial map output
map <- leaflet() %>%
    setView(lng = -125,
            lat = 53,
            zoom = 5) %>%
    addTiles(group = "OpenStreetMap") %>%
    addProviderTiles("CartoDB.Positron") %>%
    addPolygons(
        data = shapesDF,
        stroke = TRUE,
        weight = 1,
        fillOpacity = 0.6,
        smoothFactor = 1,
        color = '#333',
        layerId = shapesDF@data$OBJECTID,
        fillColor = ~pal(no_mkt_trans),
        popup = paste0(
            "<b>",
            geoUnit,
            "</b><br>Transactions period: ",
            as.character(shapesDF$trans_period),
            "</b><br>Number of transactions: ",
            as.character(shapesDF$no_mkt_trans),
            "</b><br>Number of foreign transactions: ",
            as.character(shapesDF$no_foreign),
            "</b><br>Total value: ",
            as.character(shapesDF$sum_FMV),
            "</b><br>Total value by foreign purchasers: ",
            as.character(shapesDF$sum_FMV_foreign)
        ),
        group = "divisions"
    ) %>%
    # addLegend(
    #     "bottomleft",
    #     pal = pal,
    #     values = ~shapesDF$no_mkt_trans,
    #     title = "Legend"
    # ) %>%
    addLayersControl(
        overlayGroups = c("Census Divisions"),
        options = layersControlOptions(collapsed = FALSE)
    )

map


