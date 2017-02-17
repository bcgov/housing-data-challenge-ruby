
library(shiny)
library(rgdal)
library(leaflet)
library(maps)
library(dplyr)
library(htmlwidgets)

# Load census divisions shapes
censusDivs <- readOGR("./data/boundaries/census-divisions/gcd_000b11a_e.shp", layer = "gcd_000b11a_e", verbose = FALSE)

# subset censusDivs to filter out provinces other than BC
bcCensusDivs <- subset(censusDivs, censusDivs$PRNAME %in% c(
  "British Columbia / Colombie-Britannique"
))

# Clean-up unnecessary objects to free up memory
rm(censusDivs)
gc()

# Load geographical concordance file
geoConcordance <- read.csv("./data/geography-concordance.csv", header = TRUE)

# Get Regional Districts
regionalDistricts <- geoConcordance[, c(4,5)]
regionalDistricts <- regionalDistricts[!duplicated(regionalDistricts[, c("Regional.District")]),]

# Load property tax file
propertyTax <- read.csv("./data/propertytax/regional-district-monthly.csv", header = TRUE)

# Convert join columns to uppercase to avoid mismatches due to case sensitivity
bcCensusDivs$CDNAME <- toupper(bcCensusDivs$CDNAME)
propertyTax <- propertyTax %>% mutate_each(funs(toupper), RegionalDistrict)
regionalDistricts <- regionalDistricts %>% mutate_each(funs(toupper), c(Regional.District, RD.name.2))

# Merge Property Tax and Geo Concordance
propertyTax <- merge(propertyTax, regionalDistricts, by.x = "RegionalDistrict", by.y = "RD.name.2")

propertyTaxDec <- subset(propertyTax, trans_period == "2016-12-01")

# Merge census divisions shapes, geo concordance and property tax data
bcCensusDivsBck <- bcCensusDivs
bcCensusDivsBck@data$rec <- 1:nrow(bcCensusDivsBck@data)
# tmp <- left_join(bcCensusDivsBck@data, propertyTax, by = c("CDNAME" = "Regional.District"), copy = TRUE) %>% arrange(rec)


bcMapDF <- merge(bcCensusDivsBck, propertyTaxDec, by.x = "CDNAME", by.y = "Regional.District", 
                 sort = FALSE, by = ALL)

# Fill in some sample data
# bcCensusDivs$datacol <- sample(100, size = nrow(bcCensusDivs), replace = TRUE)

# Define color pallette
pal <- colorBin("YlGnBu", bins = 10, bcMapDF$no_mkt_trans)
pal <- colorQuantile("YlGnBu", n = 9, bcMapDF$no_mkt_trans)

# Generate leaflet map object
bcMap <- leaflet(bcMapDF) %>%
  setView(lng = -125, lat = 53, zoom = 5) %>% 
  addTiles(group = "OpenStreetMap") %>% 
  addProviderTiles("CartoDB.Positron") %>%
  addPolygons(
    stroke = TRUE, 
    weight = 1, 
    fillOpacity = 0.6, 
    smoothFactor = 0.5,
    color = '#333', 
    fillColor = ~pal(no_mkt_trans),
    popup = paste0(
      "<b>",
      as.character(bcMapDF$CDNAME), 
      "</b><br>Transactions period: ", 
      as.character(bcMapDF$trans_period),
      "</b><br>Number of transactions: ", 
      as.character(bcMapDF$no_mkt_trans),
      "</b><br>Number of foreign transactions: ", 
      as.character(bcMapDF$no_foreign),
      "</b><br>Total value: ", 
      as.character(bcMapDF$sum_FMV),
      "</b><br>Total value by foreign purchasers: ", 
      as.character(bcMapDF$sum_FMV_foreign)
    ), 
    group = "Census Divisions"
  ) %>%
  addLegend("bottomright", pal = pal, values = ~bcMapDF$no_mkt_trans, title = "Legend") %>%
  addLayersControl(
    overlayGroups = c("Census Divisions"),
    options = layersControlOptions(collapsed = FALSE)
  )

# Clean-up unnecessary objects to free up memory
rm(bcCensusDivs)
gc()

# Render map
# bcMap
# saveWidget(bcMap, file="census-divisions_no_mkt_trans.html", selfcontained=FALSE)


# Shiny server UI
ui <- fluidPage(
   
  titlePanel("BC Housing Data"),
  
  ui <- bootstrapPage(
    tags$p("Current map is based on census division boundaries and property transfer tax data"),
    leafletOutput("themap")
  )
  
)

# Shiny server logic
server <- function(input, output, session) {
   
  output$themap <- renderLeaflet({
    bcMap
  })
  
}

# Run the application 
shinyApp(ui = ui, server = server)

