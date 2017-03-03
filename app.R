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

# read objects
# propoerty tax
# propertyTax <- readRDS("./data/propertyTax.rds")
ptRegDisMth <- readRDS("./data/pt-regional-district-monthly.rds")
# ptRegDisWk <- readRDS("./data/pt-regional-district-weekly.rds")
ptMunMth <- readRDS("./data/pt-municipal-monthly.rds")
ptDevRegMth <- readRDS("./data/pt-development-region-monthly.rds")
# ptDevRegWk <- readRDS("./data/pt-development-region-weekly.rds")

# shapefiles
bcCensusDivs <- readRDS("./data/bc2011CensusDivisions.rds")
bcCensusEconRegs <- readRDS("./data/bc2011EconomicRegions.rds")
bcCensusTracts <- readRDS("./data/bc2011Tracts.rds")
bcCensusConsSubdivs <- readRDS("./data/bc2011ConsolidatedSubdivisions.rds")
bcCensusDissAreas <- readRDS("./data/bc2011DisseminationAreas.rds")
bcCensusMetroAreas <- readRDS(("./data/bc2011MetropolitanAreas.rds"))

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
                "FMV sum ($ sum)" = "sum_FMV",
                "FMV Average ($ mean)" = "mn_FMV",
                "FMV Median ($ median)" = "md_FMV",
                "PTT Paid ($ sum)" = "sum_PPT_paid",
                "PTT Paid Median ($ median)" = "md_PPT",
                "Foreign Involvement Transactions #" = "no_foreign",
                # "Foreign Involvement transactions - Res. #" = "no_foreign_res",
                # "Foreign Involvement transactions - Comm. #" = "no_foreign_comm",
                # "Foreign Involvement transactions - Other #" = "no_foreign_comm",
                "FMV sum of Foreign Involvement Transactions ($ sum)" = "sum_FMV_foreign",
                "FMV Average of Foreign Involvement Transactions ($ mean)" = "mn_FMV_foreign",
                "FMV Average of Foreign Involvement Transactions ($ mean)" = "md_FMV_foreign",
                # "Under $1 million (count, foreign involvement transactions)" = "no_lt1M_foreign",
                # "$1 million - $3 million (count, foreign involvement transactions)" = "no_gt1M_foreign",
                # "Over $3 million (count, foreign involvement transactions)" = "no_gt3M_foreign",
                "Additional Tax Paid ($ sum)" = "add_tax_paid"
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


propertyTax <- ptRegDisMth

shinyApp(ui = navbarPage(theme = "bcgov.css",
    title = 'BC Housing DataViz',
    tabPanel('Property Sales',
        fluidPage(
            titlePanel("BC Housing Data Visualization"),
            tags$p(
                "Current map is based on census division boundaries and property transfer tax data"
            ),
            sidebarLayout(
                sidebarPanel(width = 3,
                    # selectInput("pt_trans_period", "Transaction Period", transPeriods),
                    selectInput("pt_trans_period", "Transaction Period", levels(propertyTax$trans_period), multiple = FALSE),
                    selectInput("pt_view", "View",
                        c("Regional District" = "regdis",
                            "Development Region" = "devreg",
                            "Municipality" = "mun")
                    ),
                    selectInput("pt_metric", "Metric", selectionMetrics),
                    verbatimTextOutput("text"),
                    verbatimTextOutput("text2")
                ),
                mainPanel(width = 9,
                    leafletOutput("map", height = 300)
                )
            ),
            tabsetPanel(
                tabPanel("Number of Transactions", 
                    column(6, plotlyOutput("interactive", height = 600))
                ),
                tabPanel("Foreign Involvement", 
                    tags$h3("Foreign Involvement")
                ),
                tabPanel("Tabular Data", dataTableOutput("dt"))
            )
        )
    ),
    tabPanel('Monthly overview',
        fluidPage(
            titlePanel("Property Sales Monthly Overview"),
            column(6, plotlyOutput("pt_mothly", height = 600)),
            column(6, plotlyOutput("pt_mothly_res", height = 600))
        )
    )
),


server <- function(input, output, session) {
    
    #initial map output
    output$map <- renderLeaflet({
        leaflet() %>%
            setView(lng = -125,
                    lat = 53,
                    zoom = 5) %>%
            addTiles(group = "OpenStreetMap") %>%
            addProviderTiles("CartoDB.Positron") #%>%
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
                   
                # Convert join columns to uppercase to avoid mismatches due to case sensitivity
                bcCensusMetroAreas@data$CMANAME <- toupper(bcCensusMetroAreas@data$CMANAME)
                   
                propertyTaxPeriod$Municipality <- 
                    gsub("ABBOTSFORD", "ABBOTSFORD - MISSION", propertyTaxPeriod$Municipality)
                   
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
                    c("FMV sum ($ sum)", "FMV Average ($ mean)", "FMV Median ($ median)",
                      "PTT Paid ($ sum)", "PTT Paid Median ($ median)",
                      "FMV sum of Foreign Involvement Transactions ($ sum)",
                      "FMV Average of Foreign Involvement Transactions ($ mean)",
                      "FMV Average of Foreign Involvement Transactions ($ mean)",
                      "Additional Tax Paid ($ sum)"), 
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
                      "Foreign Involvement Transactions #"), 
                    currency = "",
                    digits = 0
                ) 
        )
        
        axisFormat <- list(
            title = "",
            showticklabels = TRUE,
            # tickangle = 45,
            tickfont = list(
                family = "Myriad-Pro, Calibri, Arial, 'sans serif'",
                size = 12,
                color = "black"
            )#,
            # exponentformat = "E"
        )
        
        output$interactive <- renderPlotly({
            plot_ly(propertyTaxPeriod, 
                    x = ~propertyTaxPeriod[[pt_metric]],
                    y = ~geoUnit, 
                    type = "bar", orientation = "h") %>%
                layout(title = pt_metric, #"Number of market transactions",
                    xaxis = axisFormat,
                    yaxis = axisFormat,
                    margin = list(b = 100, l = 150),
                    barmode = 'group'
                )
        })
        
        output$pt_mothly <- renderPlotly({
            plot_ly(propertyTax, 
                    x = ~trans_period, 
                    y = ~no_resid_trans, 
                    name = "Residential", 
                    type = "bar",
                    hoverinfo = "y+name"
                ) %>%
                add_trace(y = ~no_comm_tot, name = "Commercial") %>%
                add_trace(y = ~no_recr_tot, name = "Recreational") %>% 
                add_trace(y = ~no_farm_tot, name = "Farms") %>% 
                add_trace(y = ~no_unkn_tot, name = "Unknown") %>% 
                # add_trace(y = ~no_mkt_trans, name = "Total", type = "scatter", mode = "line") %>% 
                layout(title = "Number of market transactions",
                    xaxis = axisFormat,
                    yaxis = axisFormat,
                    margin = list(b = 50, l = 50),
                    barmode = 'stack')
        })
        
        output$pt_mothly_res <- renderPlotly({
            plot_ly(propertyTax, 
                    x = ~trans_period, 
                    y = ~no_res_1fam, 
                    name = "Single Family", 
                    type = "bar",
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
                       margin = list(b = 50, l = 50),
                       barmode = 'stack')
        })
        
    })
})
