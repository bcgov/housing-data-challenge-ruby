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
library(plotly)

# Read objects
# Property transfer tax
ptRegDisMth <- readRDS("./data/pt-regional-district-monthly.rds")
ptMunMth <- readRDS("./data/pt-municipal-monthly.rds")
ptDevRegMth <- readRDS("./data/pt-development-region-monthly.rds")
ptProvMth <- readRDS("./data/pt-provincial-monthly.rds")

# boundaries shapefiles
bcCensusDivs <- readRDS("./data/bc2011CensusDivisions.rds")
bcCensusEconRegs <- readRDS("./data/bc2011EconomicRegions.rds")
bcCensusTracts <- readRDS("./data/bc2011Tracts.rds")
bcCensusMetroAreas <- readRDS(("./data/bc2011MetropolitanAreas.rds"))

# census 2016
c16Divs <- readRDS("./data/census2016-divisions.rds")
c16EconRegs <- readRDS("./data/census2016-economic-regions.rds")
c16Tracts <- readRDS("./data/census2016-tracts.rds")
c16MetroAreas <- readRDS("./data/census2016-metro-areas.rds")
c16Prov <- readRDS("./data/census2016-province.rds")

#  Cleanup - @TODO Move this to getdata.R
c16EconRegs$ERNAME <- 
    gsub("LOWER MAINLAND--SOUTHWEST", "MAINLAND/SOUTHWEST", c16EconRegs$ERNAME)
c16EconRegs$ERNAME <- 
    gsub("THOMPSON--OKANAGAN", "THOMPSON/OKANAGAN", c16EconRegs$ERNAME)
c16EconRegs$ERNAME <- 
    gsub("VANCOUVER ISLAND AND COAST", "VANCOUVER ISLAND/COAST", c16EconRegs$ERNAME)
c16EconRegs$Total.Private.Dwellings.2011 <- 0

ptDevRegMth$DevelopmentRegion <- 
    gsub("MAINLAND/SOUTHWEST", "LOWER MAINLAND/SOUTHWEST", ptDevRegMth$DevelopmentRegion)
ptDevRegMth$DevelopmentRegion <- 
    gsub("VANCOUVER ISLAND/COAST", "VANCOUVER ISLAND AND COAST", ptDevRegMth$DevelopmentRegion)
ptDevRegMth$Total.Private.Dwellings.Change <- 0

ptMunMth$Municipality <- 
    gsub("ABBOTSFORD", "ABBOTSFORD - MISSION", ptMunMth$Municipality)


# addRatioColumn <- function(df, ratioCol, dividend, divisor, decimals = 3) {
#     df <- df %>% 
#         mutate_(ratioCol = round(dividend / divisor, decimals)) %>% 
#         filter(!is.na(dividend)) %>% 
#         filter(!is.na(divisor))
#     return(df)
# }

# Add Percentage of Foreign Transactions column
ptRegDisMth <- ptRegDisMth %>% 
    mutate(no_foreign_perc = round(no_foreign / no_mkt_trans, 4) * 100) %>% 
    mutate(sum_FMV_foreign_perc = round(sum_FMV_foreign / sum_FMV, 4) * 100)

ptDevRegMth <- ptDevRegMth %>% 
    mutate(no_foreign_perc = round(no_foreign / no_mkt_trans, 4) * 100) %>% 
    mutate(sum_FMV_foreign_perc = round(sum_FMV_foreign / sum_FMV, 4) * 100)

ptMunMth <- ptMunMth %>% 
    mutate(no_foreign_perc = round(no_foreign / no_mkt_trans, 4) * 100) %>% 
    mutate(sum_FMV_foreign_perc = round(sum_FMV_foreign / sum_FMV, 4) * 100)

ptProvMth <- ptProvMth %>% 
    mutate(no_foreign_perc = round(no_foreign / no_mkt_trans, 4) * 100) %>% 
    mutate(sum_FMV_foreign_perc = round(sum_FMV_foreign / sum_FMV, 4) * 100)

allMetrics <- c("Total Market Transactions #" = "no_mkt_trans",
                "Res. Total #" = "no_resid_trans",
                "Res. - Acreage #" = "no_resid_acreage_trans",
                "Res. - Commerce #" = "resid_comm_count",
                "Res. - Farm #" = "no_resid_farm",
                "Res. - Multi-family #" = "no_resid_fam",
                "Res. - Single-family Res. #" = "no_res_1fam",
                "Res. - Strata Res. #" = "no_resid_strata",
                "Res. - Strata Non- Res. or Rental #" = "no_resid_non_strata",
                "Res. - Other #" = "no_resid_other",
                "Comm. Total #" = "no_comm_tot",
                "Comm. - Comm. #" = "no_comm_comm",
                "Comm. - Strata Non-Res. #" = "no_comm_strata_nores",
                "Comm. - Other #" = "no_comm_other",
                "Recr. Total #" = "no_recr_tot",
                "Farm Total #" = "no_farm_tot",
                "Other/Unknown Total #" = "no_unkn_tot",
                "FMV Sum" = "sum_FMV",
                "FMV Average" = "mn_FMV",
                "FMV Median" = "md_FMV",
                "PTT Paid" = "sum_PPT_paid",
                "PTT Paid Median" = "md_PPT",
                "Foreign Transactions #" = "no_foreign",
                # "Foreign Involvement transactions - Res. #" = "no_foreign_res",
                # "Foreign Involvement transactions - Comm. #" = "no_foreign_comm",
                # "Foreign Involvement transactions - Other #" = "no_foreign_comm",
                "FMV sum of Foreign Transactions" = "sum_FMV_foreign",
                "FMV Mean of Foreign Transactions" = "mn_FMV_foreign",
                "FMV Median of Foreign Transactions" = "md_FMV_foreign",
                # "Under $1 million (count, foreign involvement transactions)" = "no_lt1M_foreign",
                # "$1 million - $3 million (count, foreign involvement transactions)" = "no_gt1M_foreign",
                # "Over $3 million (count, foreign involvement transactions)" = "no_gt3M_foreign",
                "Additional Tax Paid" = "add_tax_paid",
                "Foreign Transactions %" = "no_foreign_perc",
                "FMV Sum of Foreign %" = "sum_FMV_foreign_perc"
)

# Selection of metrics
selectionMetrics <- c("Transactions #" = "no_mkt_trans",
             "FMV Sum" = "sum_FMV",
             "PTT Paid" = "sum_PPT_paid",
             "Foreign Transactions #" = "no_foreign",
             "FMV Sum of Foreign Transactions" = "sum_FMV_foreign",
             "Additional Tax Paid" = "add_tax_paid")

selectionMetricsDF <- data.frame(
    Metric =
        c("no_mkt_trans", "sum_FMV", "sum_PPT_paid", "no_foreign", 
            "sum_FMV_foreign", "add_tax_paid"),
    MetricName = 
        c("Transactions #", "FMV Sum", "PTT Paid", "Foreign Transactions #",
            "FMV Sum of Foreign Transactions", "Additional Tax Paid")
)

maxTransPeriod <- max(levels(ptProvMth$trans_period))
propertyTax <- ptRegDisMth
chartHeight <- 400
mapHeight <- 300

pt_view <- 'devreg'
pt_trans_period <- '2016-12-01'
pt_metric <- 'no_mkt_trans'

# Add a homepage Jumbotron
jumbotron <- function(header, popPerc = 0, popInc = TRUE, dwellPerc = 0, dwellInc = TRUE,
                      trans_period, no_mkt_trans = 0, no_foreign_perc = 0, 
                      sum_FMV = 0, sum_FMV_foreign_perc = 0) {
    
    popChange <- "increased"
    if (popInc == FALSE) {
        popChange <- "decreased"
    }
    
    dwellChange <- "increased"
    if (dwellInc == FALSE) {
        dwellChange <- "decreased"
    }
    
    HTML(paste0("<div class=\"jumbotron\">
                <h1> ", header, "</h1>
                <ul>
                    <li class=\"left\">Between 2011 and 2016 census, BC&nbsp;population 
                        has ", popChange ," by <strong>", popPerc , "%</strong>.</li>
                    <li class=\"right\">At the same time, the number of private 
                        dwellings has ", dwellChange ," by <strong>", dwellPerc , 
                        "%</strong>.</li>
                    <li class=\"left\">For the month starting ", trans_period, ", 
                        there were <strong>", format(no_mkt_trans, big.mark=","), 
                        "</strong> housing market transactions, <strong>", 
                        paste("$", format(no_foreign_perc, big.mark=","), sep=""), 
                        "%</strong> of which involved foreign citizens.</li>
                    <li class=\"right\">The volume of these transactions was <strong>", 
                        paste("$", format(sum_FMV, big.mark=","), sep="") ,
                        "</strong> (<strong>", sum_FMV_foreign_perc , "%</strong> foreign).</li>
                </ul>
                </div>") )
}

shinyApp(ui = navbarPage(theme = "css/bcgov.css",
    title = "Housing Market",
    tabPanel("Home",
        fluidPage(
            jumbotron(
                header = "BC Housing Market Data Visualization project", 
                popPerc = c16Prov$Population.Change,
                popInc = TRUE,
                dwellPerc = c16Prov$Total.Private.Dwellings.Change,
                dwellInc = TRUE,
                trans_period = maxTransPeriod,
                no_mkt_trans = ptProvMth[ptProvMth$trans_period == maxTransPeriod,"no_mkt_trans"],
                no_foreign_perc = ptProvMth[ptProvMth$trans_period == maxTransPeriod,"no_foreign_perc"] ,
                sum_FMV = ptProvMth[ptProvMth$trans_period == maxTransPeriod,"sum_FMV"],
                sum_FMV_foreign_perc = ptProvMth[ptProvMth$trans_period == maxTransPeriod,"sum_FMV_foreign_perc"]
            )
        )
    ),
    tabPanel('Overview',
        fluidPage(
            titlePanel("Property Sales Monthly Overview"),
            column(4, plotlyOutput("pt_mothly_fmv", height = chartHeight)),
            column(4, plotlyOutput("pt_mothly_mnd_fmv", height = chartHeight)),
            column(4, plotlyOutput("pt_mothly_ptt", height = chartHeight)),
            column(4, plotlyOutput("pt_mothly", height = chartHeight)),
            column(4, plotlyOutput("pt_mothly_res", height = chartHeight)),
            column(4, plotlyOutput("pt_mothly_comm", height = chartHeight))
        )
    ),
    tabPanel('Monthly Data',
        fluidPage(
            titlePanel("BC Housing Data Visualization"),
            tags$p(
                "Current map is based on census division boundaries and property transfer tax data"
            ),
            sidebarLayout(
                sidebarPanel(width = 2,
                    # selectInput("pt_trans_period", "Transaction Period", transPeriods),
                    selectInput("pt_trans_period", "Transaction Period", levels(propertyTax$trans_period), multiple = FALSE),
                    selectInput("pt_view", "View",
                        c("Regional District" = "regdis",
                            "Development Region" = "devreg",
                            "Municipality" = "mun")
                    ),
                    selectInput("pt_metric", "Metric", selectionMetrics)
                ),
                mainPanel(width = 10,
                    column(7, leafletOutput("map", height = mapHeight)),
                    column(5, plotlyOutput("interactive", height = mapHeight))
                )
            ),
            tabsetPanel(
                tabPanel("Foreign Involvement", 
                    column(4, plotlyOutput("no_foreign_period", height = chartHeight)),
                    column(4, plotlyOutput("foreign_period_mn", height = chartHeight)),
                    column(4, plotlyOutput("foreign_period_md", height = chartHeight))
                ),
                tabPanel("Tabular Data", dataTableOutput("dt")),
                tabPanel("Population and Dwellings", 
                    column(4, plotlyOutput("c16pop", height = chartHeight)),
                    column(4, plotlyOutput("c16dwell", height = chartHeight)),
                    column(4, plotlyOutput("c16change", height = chartHeight))
                )
            )
        )
    ),
    tabPanel('Your Dataviz',
        fluidPage(
            titlePanel("Make Your Own Data Visualization"),
                tags$p("This is where you can make your own data visualization based on
                    existing data, or your own data uploaded as csv file."
                ),
                fluidRow(
                    column(width = 3,
                        sidebarPanel(width = 12,
                            selectInput('dataset', "Pre-loaded datasets", c(
                                "Choose one" = "",
                                "PTT - Regional District" = "regdis",
                                "PTT - Development Region" = "devreg",
                                "PTT - Municipality" = "mun",
                                "Census 2016 - Population and Dwellings" = "c16"
                            ), selected = "", selectize = TRUE),
                            tags$hr(),
                            fileInput('uploadFile', 'or upload CSV File',
                                accept=c('text/csv', 
                                    'text/comma-separated-values,text/plain', 
                                    '.csv')
                            ),
                            checkboxInput('header', 'First line is a header', TRUE),
                            radioButtons('separator', 'Columns separated by',
                                c(Comma=',',
                                    Semicolon=';',
                                    Tab='\t'),
                                    ','
                            ),
                            radioButtons('quote', 'Columns enclosed by',
                                c(None='',
                                    'Double Quote'='"',
                                    'Single Quote'="'"),
                                    '"')
                        )
                    ),
                    column(width = 9,
                        mainPanel(width = 9,
                            tags$p("Dataviz"),
                            plotlyOutput("doVizPlot")
                        ),
                        sidebarPanel(width = 3,
                            selectInput('xAxisCol', 'X Axis Variable', ""),
                            selectInput('yAxisCol', 'Y Axis Variable', "", selected = ""),
                            selectInput('variable', 'Color Variable', "", selected = "")
                        )
                    )
                ),
                fluidRow(
                    uiOutput("doVizDt")
                )
            )
    )
),


server <- function(input, output, session) {
    
    propertyTaxPeriod <- ptRegDisMth %>% 
        filter(trans_period %in% maxTransPeriod)
    
    propertyTaxPeriod$geoUnit <- propertyTaxPeriod$Regional.District
    
    # For use in overview charts
    propertyTax <- ptRegDisMth
    propertyTax$geoUnit <- ptRegDisMth$Regional.District
    
    # Convert join columns to uppercase to avoid mismatches due to case sensitivity
    bcCensusDivs@data$CDNAME <- toupper(bcCensusDivs@data$CDNAME)
    geoUnit <- as.character(bcCensusDivs$CDNAME)
    byY <- "Regional.District"
    shapesDF <-
        merge(
            bcCensusDivs,
            propertyTaxPeriod,
            by.x = "CDNAME",
            by.y = "Regional.District",
            sort = FALSE,
            by = ALL
        )
    
    pal <- colorQuantile("YlGnBu", n = 9, as.integer(shapesDF$no_mkt_trans))
    data <- shapesDF@data
    
    # Initial map output
    # Have to fully draw the map, it doesn't go through observe when it's not 
    # in the first tab
    output$map <- renderLeaflet({
        leaflet(shapesDF) %>%
            setView(lng = -125,
                    lat = 53,
                    zoom = 5) %>%
            addTiles(group = "OpenStreetMap") %>%
            addProviderTiles("CartoDB.Positron") %>%
            
            addPolygons(
                data = shapesDF,
                stroke = TRUE,
                weight = 1,
                fillOpacity = 0.5,
                smoothFactor = 1,
                color = '#333',
                layerId = shapesDF@data$OBJECTID,
                fillColor = ~pal(shapesDF$no_mkt_trans),
                popup = paste0(
                    "<strong>", geoUnit, "</strong>",
                    "<table class=\"leaflet-popup-table\">
                    <tr><td>Period</td><td>",
                    as.Date(shapesDF$trans_period),
                    "</td></tr><tr><td>Number of transactions</td><td>",
                    format(shapesDF$no_mkt_trans, big.mark=","),
                    "</td></tr><tr><td>Number of foreign transactions</td><td>",
                    format(shapesDF$no_foreign, big.mark=","),
                    "</td></tr><tr><td>Number % by foreign purchasers</td><td>",
                    format(shapesDF$no_foreign_perc, big.mark=","),
                    "</td></tr><tr><td>Total value</td><td>",
                    paste("$", format(shapesDF$sum_FMV, big.mark=","), sep=""),
                    "</td></tr><tr><td>Total value by foreign purchasers</td><td>",
                    paste("$", format(shapesDF$sum_FMV_foreign, big.mark=","), sep=""),
                    "</td></tr><tr><td>Value % by foreign purchasers</td><td>",
                    format(shapesDF$sum_FMV_foreign_perc, big.mark=","),
                    "</td></tr><tr><td>PTT Paid</td><td>",
                    paste("$", format(shapesDF$sum_PPT_paid, big.mark=","), sep=""),
                    "</td></tr><tr><td>Additional Tax Paid</td><td>",
                    paste("$", format(shapesDF$add_tax_paid, big.mark=","), sep=""),
                    "</td></tr></table>"
                ),
                group = "divisions"
            ) %>%
            addLegend(
                "bottomleft",
                pal = pal,
                values = shapesDF$no_mkt_trans,
                title = "Transactions #",
                labFormat = labelFormat(prefix = "$"),
                opacity = 0.8
            ) %>%
            addLayersControl(
                overlayGroups = c("Census Divisions"),
                options = layersControlOptions(collapsed = FALSE)
            ) %>%
            clearGroup(group = "selected")
        
    })
    
    # observeEvent(input$map_shape_click, {
    #     #create object for clicked polygon
    #     click <- input$map_shape_click
    #     
    #     #define leaflet proxy for second regional level map
    #     proxy <- leafletProxy("map")
    #     
    #     #subset regions shapefile by the clicked on polygons
    #     selectedReg <-
    #         bcCensusDivsMap[bcCensusDivsMap@data$OBJECTID == click$id, ]
    #     
    #     output$text <- renderText({
    #         paste0(" Selected CDNAME: ", selectedReg@data$CDNAME)
    #     })
    #     
    #     output$text2 <- renderText({
    #         paste0(" Selected regional district: ",
    #                selectedReg@data$RegionalDistrict)
    #     })
    #     
    #     #map clicked on polygons
    #     proxy %>% addPolygons(
    #         data = selectedReg,
    #         fillColor = "red",
    #         fillOpacity = 0.7,
    #         weight = 1,
    #         color = "black",
    #         stroke = T,
    #         group = "selected",
    #         # layerId = "selected")
    #         layerId = selectedReg@data$OBJECTID
    #     )
    #     
    #     
    #     #remove polygon group that are clicked twice
    #     if (click$group == "selected") {
    #         proxy %>%
    #             clearGroup(group = "selected")
    #     }
    #     
    # })
    
    # Chart formatting
    fontFamily <- "Myriad-Pro, Calibri, Arial, 'sans serif'"
    tickfontBl <- list(
        family = fontFamily,
        size = 12,
        color = "black"
    )
    
    tickfontRd = list(
        family = fontFamily,
        size = 12,
        color = "#C40C0C"
    )
    
    axisFormat <- list(
        title = "",
        showticklabels = TRUE,
        # tickangle = 45,
        tickfont = tickfontBl
    )
    
    legendFormat <- list(
        font = list(
            family = fontFamily,
            size = 11,
            color = "#696969"),
        bgcolor = "#f3f3f3",
        bordercolor = "#e6e6e6",
        borderwidth = 1)
    
    marginFormat <- list(
        l = 50,
        r = 50,
        b = 100,
        t = 100,
        pad = 4
    )
    
    marginFormatMonthly <- list(
        l = 150,
        r = 50,
        b = 50,
        t = 50,
        pad = 4
    )
    
    # This observer is responsible for maintaining the circles and legend,
    # according to the variables the user has chosen to map to color and size.
    observe({
        pt_view <- input$pt_view
        pt_trans_period <- input$pt_trans_period
        pt_metric <- input$pt_metric
        
        
        switch(pt_view, 
            "regdis" = {
                propertyTaxPeriod <- ptRegDisMth %>% 
                    filter(trans_period %in% pt_trans_period)

                propertyTaxPeriod$geoUnit <- propertyTaxPeriod$Regional.District
                
                # For use in overview charts
                propertyTax <- ptRegDisMth
                propertyTax$geoUnit <- ptRegDisMth$Regional.District
                
                c16 <- c16Divs
                c16$geoUnitVal <- c16Divs$CDNAME
                
                propertyTax <-
                    merge(
                        propertyTax,
                        c16,
                        by.x = "Regional.District",
                        by.y = "CDNAME",
                        sort = FALSE,
                        by = ALL
                    )
                
                # Convert join columns to uppercase to avoid mismatches due to case sensitivity
                bcCensusDivs@data$CDNAME <- toupper(bcCensusDivs@data$CDNAME)
                geoUnit <- as.character(bcCensusDivs$CDNAME)
                byY <- "Regional.District"
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
                propertyTaxPeriod <- ptDevRegMth %>% 
                    filter(trans_period %in% pt_trans_period) %>%
                    arrange_(.dots = c(paste0("desc(", pt_metric, ")")))

                propertyTaxPeriod$geoUnit <- propertyTaxPeriod$DevelopmentRegion

                c16 <- c16EconRegs
                c16$geoUnitVal <- c16EconRegs$ERNAME
                c16$Total.Private.Dwellings.Change <- 0
                
                # For use in overview charts
                propertyTax <- ptDevRegMth
                propertyTax$geoUnit <- ptDevRegMth$DevelopmentRegion

                propertyTax <-
                    merge(
                        propertyTax,
                        c16,
                        by.x = "DevelopmentRegion",
                        by.y = "ERNAME",
                        sort = FALSE,
                        by = ALL
                    )
                
                # Convert join columns to uppercase to avoid mismatches due to case sensitivity
                erData <- bcCensusEconRegs@data
                erData <- separate(data = erData, col = ERNAME, into = c("ERNAME_E", "ERNAME_F"), sep = "\\/", fill = "right")
                erData$ERNAME_E <- trimws(toupper(gsub("--", "/", erData$ERNAME_E)))
                bcCensusEconRegs@data$ERNAME <- erData$ERNAME_E
                
                geoUnit <- as.character(bcCensusEconRegs$ERNAME)
                byY <- "DevelopmentRegion"
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
                propertyTaxPeriod <- ptMunMth %>% 
                    filter(trans_period %in% pt_trans_period) %>%
                    arrange_(.dots = c(paste0("desc(", pt_metric, ")")))
                
                propertyTaxPeriod$geoUnit <- propertyTaxPeriod$Municipality
                
                c16 <- c16MetroAreas
                c16$geoUnitVal <- c16MetroAreas$CMANAME
                
                # For use in overview charts
                propertyTax <- ptMunMth
                propertyTax$geoUnit <- ptMunMth$Municipality

                propertyTax <-
                    merge(
                        propertyTax,
                        c16,
                        by.x = "Municipality",
                        by.y = "CMANAME",
                        sort = FALSE,
                        by = ALL
                    )
                
                # Convert join columns to uppercase to avoid mismatches due to case sensitivity
                bcCensusMetroAreas@data$CMANAME <- toupper(bcCensusMetroAreas@data$CMANAME)
                   
                geoUnit <- as.character(bcCensusMetroAreas$CMANAME)
                byY <- "Municipality"
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
        
        # reorder data for chart sorting 
        propertyTaxPeriod$geoUnit <- factor(propertyTaxPeriod$geoUnit, 
            levels = unique(propertyTaxPeriod$geoUnit)
            [order(propertyTaxPeriod[[pt_metric]], decreasing = FALSE)])
        c16$geoUnitVal <- factor(c16$geoUnitVal, 
            levels = unique(c16$geoUnitVal)
            [order(c16$Population.2016, decreasing = FALSE)])
        
        pal <- colorQuantile("YlGnBu", n = 9, as.integer(shapesDF[[pt_metric]]))
        data <- shapesDF@data

        leafletProxy("map") %>%
            clearControls() %>%
            clearShapes() %>%
            addPolygons(
                data = shapesDF,
                stroke = TRUE,
                weight = 1,
                fillOpacity = 0.5,
                smoothFactor = 1,
                color = '#333',
                layerId = shapesDF@data$OBJECTID,
                fillColor = ~pal(shapesDF[[pt_metric]]),
                popup = paste0(
                    "<strong>", geoUnit, "</strong>",
                    "<table class=\"leaflet-popup-table\">
                    <tr><td>Period</td><td>",
                    as.Date(shapesDF$trans_period),
                    "</td></tr><tr><td>Number of transactions</td><td>",
                    format(shapesDF$no_mkt_trans, big.mark=","),
                    "</td></tr><tr><td>Number of foreign transactions</td><td>",
                    format(shapesDF$no_foreign, big.mark=","),
                    "</td></tr><tr><td>Number % by foreign purchasers</td><td>",
                    format(shapesDF$no_foreign_perc, big.mark=","),
                    "</td></tr><tr><td>Total value</td><td>",
                    paste("$", format(shapesDF$sum_FMV, big.mark=","), sep=""),
                    "</td></tr><tr><td>Total value by foreign purchasers</td><td>",
                    paste("$", format(shapesDF$sum_FMV_foreign, big.mark=","), sep=""),
                    "</td></tr><tr><td>Value % by foreign purchasers</td><td>",
                    format(shapesDF$sum_FMV_foreign_perc, big.mark=","),
                    "</td></tr><tr><td>PTT Paid</td><td>",
                    paste("$", format(shapesDF$sum_PPT_paid, big.mark=","), sep=""),
                    "</td></tr><tr><td>Additional Tax Paid</td><td>",
                    paste("$", format(shapesDF$add_tax_paid, big.mark=","), sep=""),
                    "</td></tr></table>"
                ),
                group = "divisions"
            ) %>%
            addLegend(
                "bottomleft",
                pal = pal,
                values = shapesDF[[pt_metric]],
                title = pt_metric, #"Transactions #",
                labFormat = labelFormat(prefix = "$"),
                opacity = 0.8
            ) %>%
            addLayersControl(
                overlayGroups = c("Census Divisions"),
                options = layersControlOptions(collapsed = FALSE)
            ) %>%
            clearGroup(group = "selected")
        
        output$dt = DT::renderDataTable(
            datatable(propertyTaxPeriod, #%>% 
                          # select_(.dots = selectionMetricsDF$Metric),
                      options = list(
                          lengthChange = TRUE,
                          initComplete = JS("
                                            function(settings, json) {
                                            $(this.api().table().header()).css({
                                            'background-color': 'rgba(0, 51, 102, 0.80)',
                                            'border-bottom': '5px solid #fcba19',
                                            'color': '#fff'
                                            });
                                            }")),
            colnames = allMetrics,
            selection = list(target = 'row+column')) %>% 
                formatCurrency(
                    c("FMV Sum",
                      "FMV Average",
                      "FMV Median",
                      "PTT Paid",
                      "PTT Paid Median",
                      "FMV sum of Foreign Transactions",
                      "FMV Mean of Foreign Transactions",
                      "FMV Median of Foreign Transactions",
                      "Additional Tax Paid"), 
                    currency = "$",
                    digits = 0
                ) %>%
                formatCurrency(
                    c("Total Market Transactions #",
                      "Res. Total #",
                      "Res. - Acreage #",
                      "Res. - Commerce #",
                      "Res. - Farm #",
                      "Res. - Multi-family #",
                      "Res. - Single-family Res. #",
                      "Res. - Strata Res. #",
                      "Res. - Strata Non- Res. or Rental #",
                      "Res. - Other #",
                      "Comm. Total #",
                      "Comm. - Comm. #",
                      "Comm. - Strata Non-Res. #",
                      "Comm. - Other #",
                      "Recr. Total #",
                      "Farm Total #",
                      "Other/Unknown Total #",
                      "Foreign Transactions #"), 
                    currency = "",
                    digits = 0
                ) 
        )

        # Interactive based on user input
        output$interactive <- renderPlotly({
            plot_ly(propertyTaxPeriod, 
                x = ~propertyTaxPeriod[[pt_metric]],
                y = ~geoUnit, 
                type = "bar", orientation = "h") %>%
            layout(title = pt_metric, #"Number of market transactions",
                xaxis = axisFormat,
                yaxis = axisFormat,
                margin = marginFormatMonthly,
                barmode = 'group',
                legend = legendFormat
            ) %>% 
            config(displayModeBar = F)
        })
        
        # Census 2016 population
        output$c16pop <- renderPlotly({
            plot_ly(c16,
                    y = ~geoUnitVal, 
                    x = ~Population.2016,
                    name = "Population 2016",
                    marker = list(color = '#C40C0C'),
                    type = "bar"
            ) %>%
                add_trace(x = ~Population.2011, name = "Population 2011", 
                          marker = list(color = 'rgb(62, 180, 240)')) %>% 
                layout(title = "Census Population",
                       xaxis = axisFormat,
                       yaxis = axisFormat,
                       margin = marginFormatMonthly,
                       barmode = 'group',
                       legend = legendFormat
                ) %>% 
                config(displayModeBar = F)
        })
        
        # Census 2016 dwellings
        output$c16dwell <- renderPlotly({
            plot_ly(c16,
                    y = ~geoUnitVal, 
                    x = ~Total.Private.Dwellings.2016,
                    name = "Dwellings 2016",
                    marker = list(color = '#C40C0C'),
                    type = "bar"
            ) %>%
                add_trace(x = ~Total.Private.Dwellings.2011, name = "Dwellings 2011", 
                          marker = list(color = 'rgb(62, 180, 240)')) %>% 
                layout(title = "Census Private Dwellings",
                       xaxis = axisFormat,
                       yaxis = axisFormat,
                       margin = marginFormatMonthly,
                       barmode = 'group',
                       legend = legendFormat
                ) %>% 
                config(displayModeBar = F)
        })
        
        # Census 2016 change
        output$c16change <- renderPlotly({
            plot_ly(c16,
                    y = ~geoUnitVal, 
                    x = ~Population.Change,
                    name = "Population",
                    marker = list(color = '#C40C0C'),
                    type = "bar"
            ) %>%
                add_trace(x = ~Total.Private.Dwellings.Change, name = "Dwellings", 
                          marker = list(color = 'rgb(62, 180, 240)')) %>% 
                layout(title = "Change from 2011 Census (%)",
                       xaxis = axisFormat,
                       yaxis = axisFormat,
                       margin = marginFormatMonthly,
                       # barmode = 'group',
                       legend = legendFormat
                ) %>% 
                config(displayModeBar = F)
        })
        
        # add_lines(y = ~Total.Private.Dwellings.2016, name = "Total.Private.Dwellings.2016", 
                          
        # Foreign - Number of Foreign Transactions
        output$no_foreign_period <- renderPlotly({
            plot_ly(propertyTaxPeriod %>% 
                        arrange(desc(no_foreign)), 
                x = ~geoUnit, 
                y = ~no_foreign,
                type = "bar",
                marker = list(color = 'rgb(62, 180, 240)')) %>%
            layout(title = "Number of Foreign Transactions",
                   xaxis = axisFormat,
                   yaxis = axisFormat,
                   margin = marginFormat,
                   legend = legendFormat
            ) %>% 
            config(displayModeBar = F)
        })
        
        # Foreign - FMV Mean
        output$foreign_period_mn <- renderPlotly({
            plot_ly(propertyTaxPeriod %>% 
                        filter(!is.na(mn_FMV_foreign)) %>% 
                arrange(desc(mn_FMV)), 
                x = ~geoUnit, 
                y = ~mn_FMV,
                name = "Canadian",
                marker = list(color = '#C40C0C'),
                type = "bar"
            ) %>%
            add_trace(y = ~mn_FMV_foreign, name = "Foreign", marker = list(color = 'rgb(62, 180, 240)')) %>% 
            layout(title = "FMV Mean",
                   xaxis = axisFormat,
                   yaxis = axisFormat,
                   margin = marginFormat,
                   barmode = 'group',
                   legend = legendFormat
            ) %>% 
            config(displayModeBar = F)
        })
        
        # Foreign - FMV Median
        output$foreign_period_md <- renderPlotly({
            plot_ly(propertyTaxPeriod %>% 
                filter(!is.na(md_FMV_foreign)) %>% 
                    arrange(desc(md_FMV)), 
                x = ~geoUnit, 
                y = ~md_FMV,
                name = "Canadian",
                marker = list(color = '#C40C0C'),
                type = "bar"
            ) %>%
            add_trace(y = ~md_FMV_foreign, name = "Foreign", marker = list(color = 'rgb(62, 180, 240)')) %>% 
            layout(title = "FMV Median",
                   xaxis = axisFormat,
                   yaxis = axisFormat,
                   margin = marginFormat,
                   barmode = 'group',
                   legend = legendFormat
            ) %>% 
            config(displayModeBar = F)
        })
        
    })
    
    # Monthly Overview - FMV (Fair Market Value)
    output$pt_mothly_fmv <- renderPlotly({
        plot_ly(
            ptProvMth,
            x = ~trans_period, y = ~sum_FMV, name = "Total FMV", 
            type = 'scatter', mode = 'lines', 
            line = list(shape = "spline", color = "#C40C0C")
        ) %>%
            add_lines(y = ~sum_FMV_foreign, name = "Total FMV Foreign", 
                      line = list(shape = "spline", color = 'rgb(42, 120, 180)')) %>% 
            add_lines(y = ~no_foreign_perc, name = "Foreign %", yaxis = "y2",
                      line = list(shape = "spline", color = '#396', dash = 'dot')) %>%
            layout(
                title = "FMV (Fair Market Value)",
                xaxis = axisFormat,
                yaxis = axisFormat,
                yaxis2 = list(
                    tickfont = tickfontRd,
                    overlaying = "y",
                    side = "right",
                    title = "Foreign %"
                ),
                margin = marginFormat,
                legend = legendFormat
            ) %>% 
            config(displayModeBar = F)
    })
    
    # Monthly Overview - Average FMV
    output$pt_mothly_mnd_fmv <- renderPlotly({
        plot_ly(
            ptProvMth,
            x = ~trans_period, y = ~mn_FMV, name = "Mean FMV", 
            type = 'scatter', mode = 'lines', 
            line = list(shape = "spline", color = '#C40C0C')
        ) %>%
            add_lines(y = ~mn_FMV_foreign, name = "Mean FMV Foreign", 
                      line = list(shape = "spline", color = 'rgb(62, 180, 240)')
            ) %>%
            add_lines(y = ~md_FMV, name = "Median FMV", 
                      line = list(shape = "spline", color = '#C40C0C', dash = 'dot')
            ) %>%
            add_lines(y = ~md_FMV_foreign, name = "Median FMV Foreign", 
                      line = list(shape = "spline", color = 'rgb(62, 180, 240)', dash = 'dot')
            ) %>%
            # add_lines(y = ~md_PPT, name = "Median PTT", line = list(shape = "spline")) %>% 
            layout(
                title = "Average FMV",
                xaxis = axisFormat,
                yaxis = axisFormat,
                margin = marginFormat,
                legend = legendFormat
            ) %>% 
            config(displayModeBar = F)
    })
    
    # Monthly Overview - Property Transfer Tax
    output$pt_mothly_ptt <- renderPlotly({
        plot_ly(
            ptProvMth,
            x = ~trans_period, 
            y = ~sum_PPT_paid, 
            name = "PTT", 
            type = 'scatter', mode = 'lines', line = list(shape = "spline")) %>%
            add_lines(y = ~add_tax_paid, name = "Additional PTT", line = list(shape = "spline")) %>% 
            layout(
                title = "Property Transfer Tax", 
                xaxis = axisFormat,
                yaxis = axisFormat,
                margin = marginFormat,
                legend = legendFormat
            ) %>% 
            config(displayModeBar = F)
    })
    
    # Monthly Overview - Number of market transactions
    output$pt_mothly <- renderPlotly({
        plot_ly(ptProvMth, 
                x = ~trans_period, 
                y = ~no_resid_trans, 
                name = "Residential", 
                type = "bar",
                # marker = list(color = brewer.pal(5, "Paired")),
                hoverinfo = "y+name"
        ) %>%
            add_trace(y = ~no_comm_tot, name = "Commercial") %>%
            add_trace(y = ~no_recr_tot, name = "Recreational") %>% 
            add_trace(y = ~no_farm_tot, name = "Farms") %>% 
            add_trace(y = ~no_unkn_tot, name = "Unknown") %>% 
            layout(title = "Number of market transactions",
                   xaxis = axisFormat,
                   yaxis = axisFormat,
                   margin = marginFormat,
                   barmode = 'stack',
                   legend = legendFormat) %>% 
            config(displayModeBar = F)
    })
    
    # Monthly Overview - Number of market transactions - Residential
    output$pt_mothly_res <- renderPlotly({
        plot_ly(ptProvMth, 
                x = ~trans_period, 
                y = ~no_res_1fam, 
                name = "Single Family", 
                type = "bar",
                # marker = list(color = brewer.pal(6, "Set1")),
                hoverinfo = "y+name"
        ) %>%
            add_trace(y = ~no_resid_fam, name = "Multi Family") %>%
            add_trace(y = ~no_resid_strata, name = "Strata") %>% 
            add_trace(y = ~no_resid_non_strata, name = "Non-strata / Rental") %>% 
            add_trace(y = ~no_resid_acreage_trans, name = "Acreage") %>% 
            add_trace(y = ~resid_comm_count, name = "Commercial") %>% 
            add_trace(y = ~no_resid_farm, name = "Farm") %>% 
            add_trace(y = ~no_resid_other, name = "Other") %>% 
            layout(title = "Number of market transactions - Residential",
                   xaxis = axisFormat,
                   yaxis = axisFormat,
                   margin = marginFormat,
                   barmode = 'stack',
                   legend = legendFormat) %>% 
            config(displayModeBar = F)
    })
    
    # Monthly Overview - Number of market transactions - Commercial
    output$pt_mothly_comm <- renderPlotly({
        plot_ly(ptProvMth, 
                x = ~trans_period, 
                y = ~no_comm_comm, 
                name = "Commerce", 
                type = "bar",
                # marker = list(color = brewer.pal(6, "Set1")),
                hoverinfo = "y+name"
        ) %>%
            add_trace(y = ~no_comm_strata_nores, name = "Strata non-residential") %>%
            add_trace(y = ~no_comm_other, name = "Other") %>% 
            layout(title = "Number of market transactions - Commercial",
                   xaxis = axisFormat,
                   yaxis = axisFormat,
                   margin = marginFormat,
                   barmode = 'stack',
                   legend = legendFormat) %>% 
            config(displayModeBar = F)
    })
    
    
    # Your own dataviz
    
    #This function is repsonsible for loading in the selected file
    uploadedData <- reactive({
        uploadedFile <- input$uploadFile
        if (is.null(uploadedFile)) {
            return(NULL)
        }
        
        data <- read.csv(uploadedFile$datapath, header=input$header, sep=input$separator, 
                 quote=input$quote)
        
        updateSelectInput(session, inputId = 'xAxisCol', label = 'X Variable',
                          choices = names(data), selected = names(data)[1])
        updateSelectInput(session, inputId = 'yAxisCol', label = 'Y Variable',
                          choices = names(data), selected = names(data)[2])
        updateSelectInput(session, inputId = 'variable', label = 'Variable',
                          choices = names(data), selected = names(data)[3])
        data
        
    })
    
    #This previews the CSV data file
    output$doVizDt <- renderUI({
        output$vizDt <- renderDataTable(uploadedData(),
            options = list(
                lengthChange = TRUE,
                initComplete = JS("
                    function(settings, json) {
                        $(this.api().table().header()).css({
                            'background-color': 'rgba(0, 51, 102, 0.80)',
                            'border-bottom': '5px solid #fcba19',
                            'color': '#fff'
                        });
                    }"
                )
            )
        )
        dataTableOutput("vizDt")
    })
    
    # Do your data visualization
    output$doVizPlot <- renderPlotly({
        # if (input$doViz == 0) {
        #     return(NULL)
        # }
        df = uploadedData()
        if (is.null(df)) {
            return(NULL)
        }

        plot_ly(df,
                x = ~df[,input$xAxisCol],
                y = ~df[,input$yAxisCol],
                z = ~df[,input$variable], type="heatmap"
            ) %>% 
            # add_trace(~df[,input$variable]) %>% 
            layout(title = input$xAxisCol,
                   xaxis = axisFormat,
                   yaxis = axisFormat,
                   margin = marginFormatMonthly,
                   # barmode = 'group',
                   legend = legendFormat
            ) %>%
            config(displayModeBar = F)
        
    })
    
})
