library(shiny)
library(rgdal)
library(leaflet)
library(maps)
library(dplyr)
library(htmlwidgets)

library(DT)
library(rgeos)
library(tidyr)
library(crosstalk)

# read objects
# property tax
propertyTax <- readRDS("./data/propertyTax.rds")
ptRegDisMth <- readRDS("./data/pt-regional-district-monthly.rds")
ptMunMth <- readRDS("./data/pt-municipal-monthly.rds")
ptDevRegMth <- readRDS("./data/pt-development-region-monthly.rds")

# shapefiles
bcCensusDivs <- readRDS("./data/bc2011CensusDivisions.rds")
bcCensusEconRegs <- readRDS("./data/bc2011EconomicRegions.rds")
bcCensusTracts <- readRDS("./data/bc2011Tracts.rds")
bcCensusConsSubdivs <- readRDS("./data/bc2011ConsolidatedSubdivisions.rds")
bcCensusDissAreas <- readRDS("./data/bc2011DisseminationAreas.rds")
bcCensusMetroAreas <- readRDS(("./data/bc2011MetropolitanAreas.rds"))

# Selection of transaction periods
transPeriods <-
    sort(as.vector(propertyTax[!duplicated(propertyTax[, c("trans_period")]),
                               c("trans_period")]),
         decreasing = TRUE)

allMetrics <- c("Total Market Transactions (count)" = "no_mkt_trans",
                "Residential Total (count)" = "no_resid_trans",
                "Residential - Acreage (count)" = "no_resid_acreage_trans",
                "Residential - Commerce (count)" = "resid_comm_count",
                "Residential - Farm (count)" = "no_resid_farm",
                "Residential - Multi-family (count)" = "no_resid_fam",
                "Residential - Single-family Residential (count)" = "no_res_1fam",
                "Residential - Strata Residential (count)" = "no_resid_strata",
                "Residential - Strata Non- Residential or Rental (count)" = "no_resid_non_strata",
                "Residential - Other (count)" = "no_resid_other",
                "Commercial Total (count)" = "no_comm_tot",
                "Commercial - Commerce (count)" = "no_comm_comm",
                "Commercial - Strata Non-Residential (count)" = "no_comm_strata_nores",
                "Commercial - Other (count)" = "no_comm_other",
                "Recreational Total (count)" = "no_recr_tot",
                "Farm Total (count)" = "no_farm_tot",
                "Other/Unknown Total (count)" = "no_unkn_tot",
                "FMV sum ($ sum)" = "sum_FMV",
                "FMV Average ($ mean)" = "mn_FMV",
                "FMV Median ($ median)" = "md_FMV",
                "PTT Paid ($ sum)" = "sum_PPT_paid",
                "PTT Paid Median ($ median)" = "md_PPT",
                "Foreign Involvement Transactions (count)" = "no_foreign",
                "Foreign Involvement transactions - Residential (count)" = "no_foreign_res",
                "Foreign Involvement transactions - Commercial (count)" = "no_foreign_comm",
                "Foreign Involvement transactions - Other (count)" = "no_foreign_comm",
                "FMV sum of Foreign Involvement Transactions ($ sum)" = "sum_FMV_foreign",
                "FMV Average of Foreign Involvement Transactions ($ mean)" = "mn_FMV_foreign",
                "FMV Average of Foreign Involvement Transactions ($ mean)" = "md_FMV_foreign",
                "Under $1 million (count, foreign involvement transactions)" = "no_lt1M_foreign",
                "$1 million - $3 million (count, foreign involvement transactions)" = "no_gt1M_foreign",
                "Over $3 million (count, foreign involvement transactions)" = "no_gt3M_foreign",
                "Additional Tax Paid ($ sum)" = "add_tax_paid")

# Selection of metrics
selectionMetrics <- c("Transactions #" = "no_mkt_trans",
                      "FMV Sum" = "sum_FMV",
                      "PTT Paid" = "sum_PPT_paid",
                      "Foreign Transactions #" = "no_foreign",
                      "FMV Sum of Foreign Transactions" = "sum_FMV_foreign",
                      "Additional Tax Paid" = "add_tax_paid")

# /**
#  * Wrangle PropertyTax object
#  * 
#  * @param propertyTax     dataframe   Property tax data frame
#  * @param pt_trans_period string      Period to filter on
#  * @param pt_group_by     string      Column to group by
#  */
wranglePropertyTax <- function(propertyTax, pt_trans_period, pt_group_by) {
    propertyTax %>% 
        filter(trans_period == pt_trans_period) %>%
        group_by_(pt_group_by) %>%
        select(no_mkt_trans, sum_FMV, sum_PPT_paid, no_foreign, sum_FMV_foreign, add_tax_paid) %>%
        summarise(no_mkt_trans = sum(no_mkt_trans, na.rm=TRUE),
                  sum_FMV = sum(sum_FMV, na.rm=TRUE),
                  sum_PPT_paid = sum(sum_PPT_paid, na.rm=TRUE),
                  no_foreign = sum(no_foreign, na.rm=TRUE),
                  sum_FMV_foreign = sum(sum_FMV_foreign, na.rm=TRUE),
                  add_tax_paid = sum(add_tax_paid, na.rm=TRUE)) %>%
        arrange(desc(no_mkt_trans))
    
    return(propertyTax)
}

# Selection of property tax variables
pt_names <- names(select_if(propertyTax, is.numeric))
pt_names

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

pt_view <- "regdis"
pt_trans_period <- "2016-12-01"



rm(propertyTaxPeriod)
propertyTaxPeriod <- ptRegDisMth
ppt <- propertyTaxPeriod %>% 
    filter(trans_period == 2016-12-01) %>%
    group_by(RegionalDistrict) %>%
    select(no_mkt_trans, sum_FMV, sum_PPT_paid, no_foreign, sum_FMV_foreign, add_tax_paid) %>%
    summarise(no_mkt_trans = sum(no_mkt_trans, na.rm=TRUE),
              sum_FMV = sum(sum_FMV, na.rm=TRUE),
              sum_PPT_paid = sum(sum_PPT_paid, na.rm=TRUE),
              no_foreign = sum(no_foreign, na.rm=TRUE),
              sum_FMV_foreign = sum(sum_FMV_foreign, na.rm=TRUE),
              add_tax_paid = sum(add_tax_paid, na.rm=TRUE)) %>%
    arrange(desc(no_mkt_trans))


switch(pt_view, 
       "regdis" = {
           propertyTaxPeriod <-
               wranglePropertyTax(ptRegDisMth, pt_trans_period, "RegionalDistrict")
           # subset(ptRegDisMth, trans_period == pt_trans_period)
           
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
               wranglePropertyTax(ptDevRegMth, pt_trans_period, "DevelopmentRegion")
           # subset(ptDevRegMth, trans_period == pt_trans_period)
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
               wranglePropertyTax(ptMunMth, pt_trans_period, "Municipality")
           # subset(ptMunMth, trans_period == pt_trans_period)
           # Convert join columns to uppercase to avoid mismatches due to case sensitivity
           bcCensusMetroAreas@data$CMANAME <- toupper(bcCensusMetroAreas@data$CMANAME)
           
           propertyTaxPeriod$Municipality <- 
               gsub("ABBOTSFORD", "ABBOTSFORD - MISSION", propertyTaxPeriod$Municipality)
           
           geoUnit <- as.character(bcCensusMetroAreas$CMANAME)
           shapesDF <-
               merge(
                   bcCensusMetroAreas,
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


