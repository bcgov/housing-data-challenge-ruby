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

propertyTaxPeriod <-
    subset(propertyTax, trans_period == max(transPeriods))

# Convert join columns to uppercase to avoid mismatches due to case sensitivity
bcCensusDivs@data$CDNAME <- toupper(bcCensusDivs@data$CDNAME)

bcCensusDivsMap <-
    merge(
        bcCensusDivs,
        propertyTaxPeriod,
        by.x = "CDNAME",
        by.y = "Regional.District",
        sort = FALSE,
        by = ALL
    )

shinyApp(ui = navbarPage(theme = "bcgov.css",
    title = 'BC Housin',
    tabPanel('Property Tax',
        fluidPage(
            titlePanel("BC Housing Data"),
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
                    selectInput("pt_metric", "Metric", selectionMetrics, multiple = TRUE),
                    verbatimTextOutput("text"),
                    verbatimTextOutput("text2")
                ),
                mainPanel(width = 9,
                    # column(7,
                        leafletOutput("map", height = 450)
                    # ),
                    # column(5,
                    #     plotlyOutput("trend", height = 450)
                    # )
                )
            ),
            fluidRow(
                plotlyOutput("trend")
            ),
            fluidRow(
                dataTableOutput("dt")
            )
        )
    ),
    tabPanel('Length menu',        DT::dataTableOutput('ex2')),
    tabPanel('No pagination',      DT::dataTableOutput('ex3')),
    tabPanel('No filtering',       DT::dataTableOutput('ex4')),
    tabPanel('Function callback',  DT::dataTableOutput('ex5'))
),


server <- function(input, output, session) {
    
    # transPeriod <- reactive({
    #     input$trans_period
    # })
    # 
    # view <- reactive({
    #     
    # })
    
    # display 10 rows initially
    output$ex1 <- DT::renderDataTable(
        DT::datatable(iris, options = list(pageLength = 25))
    )
    
    # -1 means no pagination; the 2nd element contains menu labels
    output$ex2 <- DT::renderDataTable(
        DT::datatable(
            iris, options = list(
                lengthMenu = list(c(5, 15, -1), c('5', '15', 'All')),
                pageLength = 15
            )
        )
    )
    
    # you can also use paging = FALSE to disable pagination
    output$ex3 <- DT::renderDataTable(
        DT::datatable(iris, options = list(paging = FALSE))
    )
    
    # turn off filtering (no searching boxes)
    output$ex4 <- DT::renderDataTable(
        DT::datatable(iris, options = list(searching = FALSE))
    )
    
    # write literal JS code in JS()
    output$ex5 <- DT::renderDataTable(DT::datatable(
        iris,
        options = list(rowCallback = DT::JS(
            'function(row, data) {
            // Bold cells for those >= 5 in the first column
            if (parseFloat(data[1]) >= 5.0)
            $("td:eq(1)", row).css("font-weight", "bold");
            }'
        ))
    ))
    
    # Define color pallette
    # pal <- colorBin("YlGnBu", bins = 5, bcCensusDivsMap$no_mkt_trans)
    pal <- colorQuantile("YlGnBu", n = 7, bcCensusDivsMap$no_mkt_trans)
    
    #initial map output
    output$map <- renderLeaflet({
        leaflet() %>%
            setView(lng = -125,
                    lat = 53,
                    zoom = 5) %>%
            addTiles(group = "OpenStreetMap") %>%
            addProviderTiles("CartoDB.Positron") %>%
            addPolygons(
                data = bcCensusDivsMap,
                stroke = TRUE,
                weight = 1,
                fillOpacity = 0.6,
                smoothFactor = 1,
                color = '#333',
                layerId = bcCensusDivsMap@data$OBJECTID,
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
                group = "divisions"
            ) %>%
            addLegend(
                "bottomleft",
                pal = pal,
                values = bcCensusDivsMap$no_mkt_trans,
                title = "Transactions #",
                labFormat = labelFormat(prefix = "$"),
                opacity = 0.8
            ) %>%
            addLayersControl(
                overlayGroups = c("Census Divisions"),
                options = layersControlOptions(collapsed = FALSE)
            )
    })
    
    # c("Total Market Transactions (count)" = "no_mkt_trans",
    #   "FMV sum ($ sum)" = "sum_FMV",
    #   "PTT Paid ($ sum)" = "sum_PPT_paid",
    #   "Foreign Involvement Transactions (count)" = "no_foreign",
    #   "FMV sum of Foreign Involvement Transactions ($ sum)" = "sum_FMV_foreign",
    #   "Additional Tax Paid ($ sum)" = "add_tax_paid")

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
        # pt_metric <- input$pt_metric
        
        # propertyTax <- propertyTax %>% 
        #     filter(trans_period == pt_trans_period) %>%
        #     group_by(Regional.District) %>%
        #     select(no_mkt_trans, sum_FMV, sum_PPT_paid, no_foreign, sum_FMV_foreign, add_tax_paid) %>%
        #     summarise(no_mkt_trans = sum(no_mkt_trans, na.rm=TRUE),
        #               sum_FMV = sum(sum_FMV, na.rm=TRUE),
        #               sum_PPT_paid = sum(sum_PPT_paid, na.rm=TRUE),
        #               no_foreign = sum(no_foreign, na.rm=TRUE),
        #               sum_FMV_foreign = sum(sum_FMV_foreign, na.rm=TRUE),
        #               add_tax_paid = sum(add_tax_paid, na.rm=TRUE)) %>%
        #     arrange(desc(no_mkt_trans))
        
        switch(pt_view, 
               "regdis" = {
                   # propertyTaxPeriod <-
                   #     subset(ptRegDisMth, trans_period == pt_trans_period)
                   propertyTaxPeriod <- ptRegDisMth %>% 
                       filter(trans_period == pt_trans_period) %>%
                       group_by(Regional.District) %>%
                       select(Regional.District, no_mkt_trans, sum_FMV, sum_PPT_paid, 
                              no_foreign, sum_FMV_foreign, add_tax_paid) %>%
                       summarise(no_mkt_trans = sum(no_mkt_trans, na.rm=TRUE),
                                 sum_FMV = sum(sum_FMV, na.rm=TRUE),
                                 sum_PPT_paid = sum(sum_PPT_paid, na.rm=TRUE),
                                 no_foreign = sum(no_foreign, na.rm=TRUE),
                                 sum_FMV_foreign = sum(sum_FMV_foreign, na.rm=TRUE),
                                 add_tax_paid = sum(add_tax_paid, na.rm=TRUE)) %>%
                       arrange(desc(no_mkt_trans))
                   
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
                   # propertyTaxPeriod <-
                   #     subset(ptDevRegMth, trans_period == pt_trans_period)
                   propertyTaxPeriod <- ptDevRegMth %>% 
                       filter(trans_period == pt_trans_period) %>%
                       group_by(DevelopmentRegion) %>%
                       select(DevelopmentRegion, no_mkt_trans, sum_FMV, sum_PPT_paid, 
                              no_foreign, sum_FMV_foreign, add_tax_paid) %>%
                       summarise(no_mkt_trans = sum(no_mkt_trans, na.rm=TRUE),
                                 sum_FMV = sum(sum_FMV, na.rm=TRUE),
                                 sum_PPT_paid = sum(sum_PPT_paid, na.rm=TRUE),
                                 no_foreign = sum(no_foreign, na.rm=TRUE),
                                 sum_FMV_foreign = sum(sum_FMV_foreign, na.rm=TRUE),
                                 add_tax_paid = sum(add_tax_paid, na.rm=TRUE)) %>%
                       arrange(desc(no_mkt_trans))
                   
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
                   # propertyTaxPeriod <-
                   #     subset(ptMunMth, trans_period == pt_trans_period)
                   propertyTaxPeriod <- ptMunMth %>% 
                       filter(trans_period == pt_trans_period) %>%
                       group_by(Municipality) %>%
                       select(Municipality, no_mkt_trans, sum_FMV, sum_PPT_paid, no_foreign, sum_FMV_foreign, add_tax_paid) %>%
                       summarise(no_mkt_trans = sum(no_mkt_trans, na.rm=TRUE),
                                 sum_FMV = sum(sum_FMV, na.rm=TRUE),
                                 sum_PPT_paid = sum(sum_PPT_paid, na.rm=TRUE),
                                 no_foreign = sum(no_foreign, na.rm=TRUE),
                                 sum_FMV_foreign = sum(sum_FMV_foreign, na.rm=TRUE),
                                 add_tax_paid = sum(add_tax_paid, na.rm=TRUE)) %>%
                       arrange(desc(no_mkt_trans))
                   
                   
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
        
        # pal <- colorBin("YlGnBu", bins = 10, bcCensusDivsMap$no_mkt_trans)
        pal <- colorQuantile("YlGnBu", n = 9, as.integer(shapesDF$no_mkt_trans))
        data <- shapesDF@data
        #initial map output
        
        leafletProxy("map") %>%
            clearShapes() %>%
# 
#         map <- leaflet() %>%
#             setView(lng = -125,
#                     lat = 53,
#                     zoom = 5) %>%
#             addTiles(group = "OpenStreetMap") %>%
#             addProviderTiles("CartoDB.Positron") %>%
            addPolygons(
                data = shapesDF,
                stroke = TRUE,
                weight = 1,
                fillOpacity = 0.85,
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
            #     values = shapesDF$no_mkt_trans,
            #     title = "Legend"
            # ) %>%
            addLayersControl(
                overlayGroups = c("Census Divisions"),
                options = layersControlOptions(collapsed = FALSE)
            ) %>%
            clearGroup(group = "selected")
        
        output$dt = DT::renderDataTable(
            datatable(propertyTaxPeriod,
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
            colnames = selectionMetrics,
            selection = list(target = 'row+column')) %>% 
                formatCurrency(
                    c('FMV Sum', 'PTT Paid', 'FMV Sum of Foreign Transactions', 'Additional Tax Paid'), 
                    currency = "$",
                    digits = 0
                ) %>%
                formatCurrency(
                    c('Transactions #', 'Foreign Transactions #'), 
                    currency = "",
                    digits = 0
                ) 
        )
        
        output$trend <- renderPlotly({
            plot_ly(propertyTaxPeriod, x = ~as.name(byY), y = ~no_mkt_trans, type = "bar") %>%
                add_trace(y = ~no_foreign) %>%
                # layout(yaxis = list(title = 'Count'), barmode = 'group')
                layout(xaxis = list(title = "", tickangle = -45),
                    yaxis = list(title = ""),
                    margin = list(b = 100),
                    barmode = 'group')
        })
        
        
        
    })
    # output$trend <- renderPlotly({
    #     plot_ly(propertyTax, x = ~trans_period, y = ~no_mkt_trans, type = "bar") %>%
    #         add_trace(y = ~no_foreign) %>%
    #         layout(yaxis = list(title = 'Count'), barmode = 'group')
    # })
    
    # output$trend <- renderPlot({
    #     plot(propertyTax)
    # })
    
})