

library(shiny)
library(rgdal)
library(leaflet)
library(maps)
library(dplyr)
library(htmlwidgets)
library(rgeos)

# Load census divisions shapes
bcCensusDivs <-
    readOGR(
        "./data/boundaries/census-divisions/gcd_000b11a_e.shp",
        layer = "gcd_000b11a_e",
        verbose = FALSE
    )

# subset censusDivs to filter out provinces other than BC
bcCensusDivs <-
    bcCensusDivs[bcCensusDivs$PRNAME == "British Columbia / Colombie-Britannique",]

# Simplify shapefile to speed up map rendering
bcCensusDivsSimplified <-
    gSimplify(bcCensusDivs, tol = 0.01, topologyPreserve = TRUE)

# Bring the data back
bcCensusDivs <-
    SpatialPolygonsDataFrame(bcCensusDivsSimplified, bcCensusDivs@data)

# Remove simplified object
rm(bcCensusDivsSimplified)
gc()

# Load geographical concordance file
geoConcordance <-
    read.csv("./data/geography-concordance.csv", header = TRUE)

# Get Regional Districts' Names
regionalDistricts <- geoConcordance[, c(4, 5)]
regionalDistricts <-
    regionalDistricts[!duplicated(regionalDistricts[, c("Regional.District")]), ]

# Load property tax file
propertyTax <-
    read.csv("./data/propertytax/regional-district-monthly.csv", header = TRUE)

# Convert join columns to uppercase to avoid mismatches due to case sensitivity
bcCensusDivs@data$CDNAME <- toupper(bcCensusDivs@data$CDNAME)
propertyTax <- propertyTax %>% mutate_each(funs(toupper), RegionalDistrict)
regionalDistricts <-
    regionalDistricts %>% mutate_each(funs(toupper), c(Regional.District, RD.name.2))

# Merge Property Tax and Geo Concordance
propertyTax <-
    merge(propertyTax,
          regionalDistricts,
          by.x = "RegionalDistrict",
          by.y = "RD.name.2")

# Selection of transaction periods
transPeriods <-
    as.vector(propertyTax[!duplicated(propertyTax[, c("trans_period")]), c("trans_period")])

# Merge census divisions shapes, geo concordance and property tax data
bcCensusDivs@data$rec <- 1:nrow(bcCensusDivs@data)
# tmp <- left_join(bcCensusDivs@data, propertyTax, by = c("CDNAME" = "Regional.District"), copy = TRUE) %>% arrange(rec)


#################
propertyTaxDec <- subset(propertyTax, trans_period == max(transPeriods))
bcCensusDivsMap <-
    merge(
        bcCensusDivs,
        propertyTaxDec,
        by.x = "CDNAME",
        by.y = "Regional.District",
        sort = FALSE,
        by = ALL
    )

# Define color pallette
# pal <- colorBin("YlGnBu", bins = 10, bcCensusDivsMap$no_mkt_trans)
pal <- colorQuantile("YlGnBu", n = 9, bcCensusDivsMap$no_mkt_trans)

# Generate leaflet map object
bcMap <- leaflet(bcCensusDivsMap) %>%
    setView(lng = -125, lat = 53, zoom = 5) %>%
    addTiles(group = "OpenStreetMap") %>%
    addProviderTiles("CartoDB.Positron") %>%
    addPolygons(
        stroke = TRUE,
        weight = 1,
        fillOpacity = 0.6,
        smoothFactor = 0.5,
        color = '#333',
        fillColor = ~ pal(no_mkt_trans),
        popup = paste0(
            "<b>",
            as.character(bcCensusDivsMap$CDNAME),
            "</b><br>Transactions period: ",
            as.character(bcCensusDivsMap$trans_period),
            "</b><br>Number of transactions: ",
            as.character(bcCensusDivsMap$no_mkt_trans),
            "</b><br>Number of foreign transactions: ",
            as.character(bcCensusDivsMap$no_foreign),
            "</b><br>Total value: ",
            as.character(bcCensusDivsMap$sum_FMV),
            "</b><br>Total value by foreign purchasers: ",
            as.character(bcCensusDivsMap$sum_FMV_foreign)
        ),
        group = "Census Divisions"
    ) %>%
    addLegend(
        "bottomleft",
        pal = pal,
        values = ~ bcCensusDivsMap$no_mkt_trans,
        title = "Legend"
    ) %>%
    addLayersControl(
        overlayGroups = c("Census Divisions"),
        options = layersControlOptions(collapsed = FALSE)
    )

#################

# Shiny server UI
ui <- bootstrapPage(
    titlePanel("BC Housing Data"),
    tags$p(
        "Current map is based on census division boundaries and property transfer tax data"
    ),
    selectInput("trans_period", "Transaction Period", transPeriods),
    leafletOutput("themap")
)

# Shiny server logic
server <- function(input, output, session) {

    output$themap <- renderLeaflet({
        bcMap
    })
    
}

# Run the application
shinyApp(ui = ui, server = server)
