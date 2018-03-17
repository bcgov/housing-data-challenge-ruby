library(data.table)
library(stringr)

# /**
#  * PROPERTY TAX
#  */

# /**
#  * Load geo concordance file
#  */
geoConcordance <-
    read.csv("./data/geography-concordance.csv", header = TRUE)
geoConcordance <- as.data.frame(sapply(geoConcordance, toupper))

placeNamesTranslations <-
    read.csv("./data/place_names_translations.csv", header = TRUE)

# /**
#  * Load property tax files, convert JOIN columns to uppercase,
#  * merge with geo-concordance and save object as rds
#  *
#  * @param ptFilePath  string  Path to property tax file
#  * @param uCaseCols   vector  Column names to convert to uppercase
#  * @param ptJoinCol   string  Property Tax column to use as join to Geo Concordance
#  * @param gcJoinCol   string  Geo Concordance column to use as join to Property Tax
#  * @param sfJoinCol   string  Geo Concordance column to use as join to Shapefile
#  * @param rdsFilePath string  Path to rds file to be save object to
#  * @param doMerge     boolean Whether to merge property tax and Geo Concordance files
#  */
savePropertyTaxRds <- function(
        ptFilePath,
        ptUcaseCols,
        ptJoinCol,
        gcJoinCol,
        sfJoinCol,
        rdsFilePath,
        doMerge) {

    propertyTax <- read.csv(ptFilePath, header = TRUE)

    # Convert to uppercase
    # propertyTax <- data.frame(lapply(propertyTax, function(v) {
    #   if (is.factor(v)) return(toupper(v))
    #   else return(v)
    # }))
    propertyTax[[ptJoinCol]] <- str_to_upper(propertyTax[[ptJoinCol]])

    # Merge Property Tax and Geo Concordance
    if (doMerge == TRUE) {
        # Get unique geo-concordance unit names
        unqGeoConcordance <- geoConcordance[, c(gcJoinCol, sfJoinCol)]
        unqGeoConcordance <-
            unqGeoConcordance[!duplicated(unqGeoConcordance[, c(gcJoinCol)]), ]
        # Convert to uppercase
        unqGeoConcordance <- as.data.frame((sapply(unqGeoConcordance, toupper)))

        # Merge
        propertyTax <-
            merge(propertyTax,
                  unqGeoConcordance,
                  by.x = ptJoinCol,
                  by.y = gcJoinCol)
    }

    saveRDS(propertyTax, rdsFilePath)
}

# /**
#  * Regional District Monthly
#  */
savePropertyTaxRds(
    "./data/propertytax/regional-district-monthly.csv",
    c("RegionalDistrict"),
    "RegionalDistrict",
    "RD.PTT.grp",
    "Regional.District",
    "./data/pt-regional-district-monthly.rds",
    TRUE
)

# /**
#  * Regional District Weekly
#  */
savePropertyTaxRds(
    "./data/propertytax/regional-district-weekly.csv",
    c("RegionalDistrict"),
    "RegionalDistrict",
    "RD.PTT.grp",
    "Regional.District",
    "./data/pt-regional-district-weekly.rds",
    TRUE
)

# /**
#  * Municipal Monthly
#  */
savePropertyTaxRds(
    "./data/propertytax/municipal-monthly.csv",
    c("Municipality"),
    "Municipality",
    "Name",
    "Name",
    "./data/pt-municipal-monthly.rds",
    FALSE
)

# /**
#  * Development Region Monthly
#  */
savePropertyTaxRds(
    "./data/propertytax/development-region-monthly.csv",
    c("DevelopmentRegion"),
    "DevelopmentRegion",
    "DevelopmentRegion",
    "DevelopmentRegion",
    "./data/pt-development-region-monthly.rds",
    TRUE
)

# /**
#  * Development Region Weekly
#  */
savePropertyTaxRds(
    "./data/propertytax/development-region-weekly.csv",
    c("DevelopmentRegion"),
    "DevelopmentRegion",
    "DevelopmentRegion",
    "DevelopmentRegion",
    "./data/pt-development-region-weekly.rds",
    TRUE
)

# /**
#  * Provincial Monthly
#  */
savePropertyTaxRds(
    "./data/propertytax/provincial-monthly.csv",
    c(""),
    "",
    "",
    "",
    "./data/pt-provincial-monthly.rds",
    FALSE
)


# /**
#  * Census 2016 - Census Divisions
#  */
csv <- read.csv("./data/census2016/2016 Census - Census divisions.CSV", header = TRUE)
csv <- csv[csv$Geographic.code..Province...territory %in% "59", -c(3:8,10,12,13,17,21)]
setnames(csv, old = names(csv), new = c("CDUID"
    ,"CDNAME"
    ,"Population.2016"
    ,"Population.2011"
    ,"Population.Change"
    ,"Total.Private.Dwellings.2016"
    ,"Total.Private.Dwellings.2011"
    ,"Total.Private.Dwellings.Change"
    ,"Private.Dwellings.Occupied.by.Usual.Residents.2016"
    ,"Private.Dwellings.Occupied.by.Usual.Residents.2011"
    ,"Private.Dwellings.Occupied.by.Usual.Residents.Change"
    ,"Land.Area.in.Square.Kilometres.2016"
    ,"Population.Density.per.Square.Kilometre.2016"
    ,"National.Population.Rank.2016"
    ,"Provincial.Territorial.Population.Rank..2016")
)
csv$CDNAME <- toupper(csv$CDNAME)
saveRDS(csv, "./data/census2016-divisions.rds")


# /**
#  * Census 2016 - Economic Regions
#  */
csv <- read.csv("./data/census2016/2016 Census - Economic regions.CSV", header = TRUE)
csv <- csv[csv$Geographic.code..Province...territory %in% "59", -c(3:6,8,10,16)]
names(csv)
setnames(csv, old = names(csv), new = c(
    "ERUID"
    ,"ERNAME"
    ,"Population.2016"
    ,"Population.2011"
    ,"Population.Change"
    ,"Total.Private.Dwellings.2016"
    ,"Private.Dwellings.Occupied.by.Usual.Residents.2016"
    ,"Land.Area.in.Square.Kilometres.2016"
    ,"Population.Density.per.Square.Kilometre.2016")
)
csv$ERNAME <- toupper(csv$ERNAME)
saveRDS(csv, "./data/census2016-economic-regions.rds")


# /**
#  * Census 2016 - Metro Areas
#  */
csv <- read.csv("./data/census2016/2016 Census - Census metropolitan areas and census agglomerations.CSV", header = TRUE)
csv <- csv[csv$Geographic.code..Province...territory %in% "59", -c(3:9,11,13,14,18,22)]
names(csv)
setnames(csv, old = names(csv), new = c(
    "CMAUID"
    ,"CMANAME"
    ,"Population.2016"
    ,"Population.2011"
    ,"Population.Change"
    ,"Total.Private.Dwellings.2016"
    ,"Total.Private.Dwellings.2011"
    ,"Total.Private.Dwellings.Change"
    ,"Private.Dwellings.Occupied.by.Usual.Residents.2016"
    ,"Private.Dwellings.Occupied.by.Usual.Residents.2011"
    ,"Private.Dwellings.Occupied.by.Usual.Residents.Change"
    ,"Land.Area.in.Square.Kilometres.2016"
    ,"Population.Density.per.Square.Kilometre.2016"
    ,"National.Population.Rank.2016"
    ,"Provincial.Territorial.Population.Rank..2016")
)
csv$CMANAME <- toupper(csv$CMANAME)
saveRDS(csv, "./data/census2016-metro-areas.rds")


# /**
#  * Census 2016 - Tracts
#  */
csv <- read.csv("./data/census2016/2016 Census - Census tracts.CSV", header = TRUE)
csv <- csv[csv$Geographic.code..Province...territory %in% "59", -c(2:4,6,7,9,11,17)]
names(csv)
setnames(csv, old = names(csv), new = c(
    "TUID"
    ,"CMANAME"
    ,"Population.2016"
    ,"Population.2011"
    ,"Population.Change"
    ,"Total.Private.Dwellings.2016"
    ,"Private.Dwellings.Occupied.by.Usual.Residents.2016"
    ,"Land.Area.in.Square.Kilometres.2016"
    ,"Population.Density.per.Square.Kilometre.2016")
)
csv$CMANAME <- toupper(csv$CMANAME)
saveRDS(csv, "./data/census2016-tracts.rds")

# /**
#  * Census 2016 - Province
#  */
csv <- read.csv("./data/census2016/2016 Census - Canada, provinces and territories.CSV", header = TRUE)
csv <- csv[csv$Geographic.code %in% "59", -c(3:5,7,9,10,14,18)]
names(csv)
setnames(csv, old = names(csv), new = c(
    "PRUID"
    ,"PRNAME"
    ,"Population.2016"
    ,"Population.2011"
    ,"Population.Change"
    ,"Total.Private.Dwellings.2016"
    ,"Total.Private.Dwellings.2011"
    ,"Total.Private.Dwellings.Change"
    ,"Private.Dwellings.Occupied.by.Usual.Residents.2016"
    ,"Private.Dwellings.Occupied.by.Usual.Residents.2011"
    ,"Private.Dwellings.Occupied.by.Usual.Residents.Change"
    ,"Land.Area.in.Square.Kilometres.2016"
    ,"Population.Density.per.Square.Kilometre.2016")
)
# csv$PRNAME <- toupper(csv$PRNAME)
saveRDS(csv, "./data/census2016-province.rds")




# /**
#  * Load boundaries files, filter out non-BC data and save object as rds
#  *
#  * @param shapeFilePath  string  Path to boundaries shapefile
#  * @param layerName      string  Name of the layer in shapefile
#  * @param rdsFilePath    string  Path to rds file to be save object to
#  */
saveBcShapesRds <- function(shapeFilePath, layerName, rdsFilePath) {
    bcShapes <-
        readOGR(
            shapeFilePath,
            layer = layerName,
            verbose = FALSE
        )

    # subset censusDivs to filter out provinces other than BC
    bcShapes <-
        bcShapes[bcShapes$PRNAME == "British Columbia / Colombie-Britannique",]

    # # Simplify shapefile to speed up map rendering
    # bcShapesSimplified <-
    #     gSimplify(bcShapes, tol = 0.01, topologyPreserve = TRUE)
    #
    # # Bring the data back
    # bcShapes <-
    #     SpatialPolygonsDataFrame(bcShapesSimplified, bcShapes@data)

    # Add OBJECTID column to use as unique ID
    # and to keep the correct row ordering
    bcShapes@data$OBJECTID <- 1:nrow(bcShapes@data)

    # Save serialized object as rds
    saveRDS(bcShapes, rdsFilePath)
}


# **** CENSUS DIVISIONS ****
saveBcShapesRds(
    "./data/boundaries/2011-census-divisions/gcd_000a11a_e.shp",
    layerName = "gcd_000a11a_e",
    "./data/bc2011CensusDivisions.rds"
)


# /**
#  * CENSUS ECONOMIC REGIONS
#  */
saveBcShapesRds(
    "./data/boundaries/2011-census-economic-regions/ger_000a11a_e.shp",
    layerName = "ger_000a11a_e",
    "./data/bc2011EconomicRegions.rds"
)


# /**
#  * CENSUS CONSOLIDATED SUBDIVISIONS
#  */
saveBcShapesRds(
    "./data/boundaries/2011-census-consolidated-subdivisions/gccs000a11a_e.shp",
    layerName = "gccs000a11a_e",
    "./data/bc2011ConsolidatedSubdivisions.rds"
)


# /**
#  * CENSUS SUBDIVISIONS
#  */
saveBcShapesRds(
    "./data/boundaries/2011-census-subdivisions/gcsd000a11a_e.shp",
    layerName = "gcsd000a11a_e",
    "./data/bc2011Subdivisions.rds"
)


# /**
#  * CENSUS TRACTS
#  */
saveBcShapesRds(
    "./data/boundaries/2011-census-tracts/gct_000a11a_e.shp",
    layerName = "gct_000a11a_e",
    "./data/bc2011Tracts.rds"
)


# /**
#  * CENSUS DISSEMINATION AREAS
#  */
saveBcShapesRds(
    "./data/boundaries/2011-census-dissemination-areas/gda_000a11a_e.shp",
    layerName = "gda_000a11a_e",
    "./data/bc2011DisseminationAreas.rds"
)


# /**
#  * CENSUS METROPOLITAN AREAS
#  */
saveBcShapesRds(
    "./data/boundaries/2011-census-metropolitan-areas/gcma000a11a_e.shp",
    layerName = "gcma000a11a_e",
    "./data/bc2011MetropolitanAreas.rds"
)


# /**
#  * CENSUS POPULATION CENTRES
#  */
saveBcShapesRds(
    "./data/boundaries/2011-census-population-centres/gpc_000a11a_e.shp",
    layerName = "gpc_000a11a_e",
    "./data/bc2011PopulationCentres.rds"
)


saveBcShapesRds(
    "./data/boundaries/Boundaries - Census Subdivisions 2011/CSD_2011.shp",
    layerName = "CSD_2011",
    "./data/bcBoundsCSD2011"
)
