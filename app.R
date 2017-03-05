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
library(RColorBrewer)

# Read objects
# Property transfer tax
ptRegDisMth <- readRDS("./data/pt-regional-district-monthly.rds")
ptMunMth <- readRDS("./data/pt-municipal-monthly.rds")
ptDevRegMth <- readRDS("./data/pt-development-region-monthly.rds")

# boundaries shapefiles
bcCensusDivs <- readRDS("./data/bc2011CensusDivisions.rds")
bcCensusEconRegs <- readRDS("./data/bc2011EconomicRegions.rds")
bcCensusTracts <- readRDS("./data/bc2011Tracts.rds")
bcCensusConsSubdivs <- readRDS("./data/bc2011ConsolidatedSubdivisions.rds")
bcCensusDissAreas <- readRDS("./data/bc2011DisseminationAreas.rds")
bcCensusMetroAreas <- readRDS(("./data/bc2011MetropolitanAreas.rds"))

# addRatioColumn <- function(df, ratioCol, dividend, divisor, decimals = 3) {
#     df <- df %>% 
#         mutate_(ratioCol = round(dividend / divisor, decimals)) %>% 
#         filter(!is.na(dividend)) %>% 
#         filter(!is.na(divisor))
#     return(df)
# }

# Add Percentage of Foreign Transactions column
ptRegDisMth <- ptRegDisMth %>% 
    mutate(no_foreign_perc = round(no_foreign / no_mkt_trans, 4) * 100) #%>% 
    # filter(!is.na(no_foreign))

ptDevRegMth <- ptDevRegMth %>% 
    mutate(no_foreign_perc = round(no_foreign / no_mkt_trans, 4) * 100) #%>% 
    # filter(!is.na(no_foreign))

ptMunMth <- ptMunMth %>% 
    mutate(no_foreign_perc = round(no_foreign / no_mkt_trans, 4) * 100) #%>% 
    # filter(!is.na(no_foreign))

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
                "Foreign Transactions Percentage" = "no_foreign_perc"
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
chartHeight <- 400
mapHeight <- 300

# pt_view <- 'regdis'
# pt_trans_period <- '2016-12-01'
# pt_metric <- 'no_mkt_trans'

shinyApp(ui = navbarPage(theme = "bcgov.css",
    title = "Housing Market",
    tabPanel('Monthly Data',
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
                    leafletOutput("map", height = mapHeight)
                )
            ),
            tabsetPanel(
                tabPanel("Number of Transactions", 
                    column(4, plotlyOutput("interactive", height = chartHeight))
                ),
                tabPanel("Foreign Involvement", 
                    column(4, plotlyOutput("no_foreign_period", height = chartHeight)),
                    column(4, plotlyOutput("foreign_period_mn", height = chartHeight)),
                    column(4, plotlyOutput("foreign_period_md", height = chartHeight))
                ),
                tabPanel("Tabular Data", dataTableOutput("dt"))
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
    tabPanel('Your Dataviz',
        fluidPage(
            titlePanel("Make Your Own Data Visualization"),
            tags$p(
                "This is where you can make your own data visualization based on
                existing data, or your own data uploaded as csv file."
            ),
            sidebarLayout(
                sidebarPanel(width = 3,
                             tags$p("Selections")
                    # # selectInput("pt_trans_period", "Transaction Period", transPeriods),
                    # selectInput("pt_trans_period", "Transaction Period", levels(propertyTax$trans_period), multiple = FALSE),
                    # selectInput("pt_view", "View",
                    #     c("Regional District" = "regdis",
                    #         "Development Region" = "devreg",
                    #         "Municipality" = "mun")
                    # ),
                    # selectInput("pt_metric", "Metric", selectionMetrics),
                    # verbatimTextOutput("text"),
                    # verbatimTextOutput("text2")
                ),
                mainPanel(width = 9,
                          tags$p("Dataviz")
                          # leafletOutput("map", height = mapHeight)
                )
            )
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
            },
            "devreg" = {
                propertyTaxPeriod <- ptDevRegMth %>% 
                    filter(trans_period %in% pt_trans_period) %>%
                    arrange_(.dots = c(paste0("desc(", pt_metric, ")")))

                propertyTaxPeriod$geoUnit <- propertyTaxPeriod$DevelopmentRegion

                # For use in overview charts
                propertyTax <- ptDevRegMth
                propertyTax$geoUnit <- ptDevRegMth$DevelopmentRegion
                
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
                   
                # For use in overview charts
                propertyTax <- ptMunMth
                propertyTax$geoUnit <- ptMunMth$Municipality
                
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
        
        # Group and summarize overview data by geoUnit
        propertyTaxOverview <- propertyTax %>% 
            select(trans_period, no_mkt_trans, no_resid_trans, 
               no_resid_acreage_trans,  resid_comm_count, no_resid_farm, 
               no_resid_fam, no_res_1fam, no_resid_strata, no_resid_non_strata, 
               no_resid_other, no_comm_tot, no_comm_comm, no_comm_strata_nores, 
               no_comm_other, no_recr_tot, no_farm_tot, no_unkn_tot, sum_FMV, 
               mn_FMV, md_FMV, sum_PPT_paid, md_PPT, no_foreign, sum_FMV_foreign, 
               mn_FMV_foreign, md_FMV_foreign, add_tax_paid, no_foreign_perc) %>% 
            group_by(trans_period) %>% 
            summarize(
                no_mkt_trans = sum(no_mkt_trans, na.rm=TRUE), 
                no_resid_trans = sum(no_resid_trans, na.rm=TRUE),
                no_resid_acreage_trans = sum(no_resid_acreage_trans, na.rm=TRUE),
                resid_comm_count = sum(resid_comm_count, na.rm=TRUE), 
                no_resid_farm = sum(no_resid_farm, na.rm=TRUE), 
                no_resid_fam = sum(no_resid_fam, na.rm=TRUE), 
                no_res_1fam = sum(no_res_1fam, na.rm=TRUE), 
                no_resid_strata = sum(no_resid_strata, na.rm=TRUE), 
                no_resid_non_strata = sum(no_resid_non_strata, na.rm=TRUE),
                no_resid_other = sum(no_resid_other, na.rm=TRUE), 
                no_comm_tot = sum(no_comm_tot, na.rm=TRUE), 
                no_comm_comm = sum(no_comm_comm, na.rm=TRUE), 
                no_comm_strata_nores = sum(no_comm_strata_nores, na.rm=TRUE),
                no_comm_other = sum(no_comm_other, na.rm=TRUE), 
                no_recr_tot = sum(no_recr_tot, na.rm=TRUE), 
                no_farm_tot = sum(no_farm_tot, na.rm=TRUE),
                no_unkn_tot = sum(no_unkn_tot, na.rm=TRUE), 
                sum_FMV = sum(sum_FMV, na.rm=TRUE),
                mn_FMV = mean(mn_FMV, na.rm=TRUE), 
                md_FMV = median(md_FMV, na.rm=TRUE), 
                sum_PPT_paid = sum(sum_PPT_paid, na.rm=TRUE), 
                md_PPT = median(md_PPT, na.rm=TRUE), 
                no_foreign = sum(no_foreign, na.rm=TRUE), 
                sum_FMV_foreign = sum(sum_FMV_foreign, na.rm=TRUE), 
                mn_FMV_foreign = mean(mn_FMV_foreign, na.rm=TRUE), 
                md_FMV_foreign = median(md_FMV_foreign, na.rm=TRUE), 
                add_tax_paid = sum(add_tax_paid, na.rm=TRUE),
                no_foreign_perc = mean(no_foreign_perc, na.rm = TRUE)
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

        output$interactive <- renderPlotly({
            plot_ly(propertyTaxPeriod, 
                x = ~propertyTaxPeriod[[pt_metric]],
                y = ~geoUnit, 
                type = "bar", orientation = "h") %>%
            layout(title = pt_metric, #"Number of market transactions",
                xaxis = axisFormat,
                yaxis = axisFormat,
                margin = list(b = 100, l = 150),
                barmode = 'group',
                legend = legendFormat
            ) %>% 
            config(displayModeBar = F)
        })

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
            layout(title = "Number of market transactions",
                xaxis = axisFormat,
                yaxis = axisFormat,
                margin = marginFormat,
                barmode = 'stack',
                legend = legendFormat) %>% 
            config(displayModeBar = F)
        })
        
        output$pt_mothly_fmv <- renderPlotly({
            plot_ly(
                propertyTaxOverview,
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
        
        output$pt_mothly_mnd_fmv <- renderPlotly({
            plot_ly(
                propertyTaxOverview,
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
        
        output$pt_mothly_ptt <- renderPlotly({
            plot_ly(
                propertyTaxOverview,
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
                       margin = marginFormat,
                       barmode = 'stack',
                       legend = legendFormat) %>% 
                config(displayModeBar = F)
        })
        
        output$pt_mothly_comm <- renderPlotly({
            plot_ly(propertyTax, 
                    x = ~trans_period, 
                    y = ~no_comm_comm, 
                    name = "Commerce", 
                    type = "bar",
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
        
    })
})
