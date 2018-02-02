library(shiny)
library(rgdal)
library(sf)
library(leaflet)
library(maps)
library(readr)
library(stringr)
library(magrittr)
library(dplyr)
library(htmlwidgets)
library(DT)
library(rgeos)
library(tidyr)
library(crosstalk)
library(plotly)
library(ggplot2)
library(cancensus)
library(sankeyD3)
library(sunburstR)
library(treemap)
library(data.tree)
library(d3treeR)
library(RColorBrewer)
library(shinycssloaders)

censusStir <- census2016CmaStir

censusStir %<>%
  mutate(
    percent_less_than_30 =
      round(stir_less_than_30 / (stir_less_than_30 + stir_more_than_30) * 100, digits = 2),
    percent_more_than_30 =
      round(stir_more_than_30 / (stir_less_than_30 + stir_more_than_30) * 100, digits = 2)
  ) %<>%
  arrange(desc(percent_more_than_30))

palStir <- colorNumeric(
  palette = "YlGnBu",
  domain = censusStir$percent_more_than_30)


class(censusStir)

# STIR Map
  palStir <- colorNumeric(
    palette = "YlGnBu",
    domain = censusStir$percent_more_than_30)

  st_as_sf(censusStir) %>%
    # censusStir() %>%
    leaflet() %>%
    addProviderTiles(provider = "CartoDB.Positron") %>%
    addPolygons(
      label = ~ `Region`,
      color = ~ pal(percent_more_than_30),
      stroke = TRUE,
      weight = 1,
      fillOpacity = 0.5,
      smoothFactor = 1,
      # color = '#333',
      # fillColor = ~ palViridis(censusStir()$percent_more_than_30),
      # fillColor = ~ pal(censusStir()$percent_more_than_30),
      popup = paste0(
        "<strong>",
        paste(censusStir$`Region`),
        "</strong>",
        "<table class=\"leaflet-popup-table\">
        <tr><td>Census Year</td><td>2016</td></tr>",
        "<tr><td>Population</td><td>",
        format(censusStir$Population, big.mark = ","),
        "</td></tr><tr><td>Dwellings</td><td>",
        format(censusStir$Dwellings, big.mark = ","),
        "</td></tr><tr><td>Households</td><td>",
        format(censusStir$Households, big.mark = ","),
        "</td></tr><tr><td>STIR < 30%</td><td>",
        format(censusStir$percent_less_than_30, big.mark = ","),
        "</td></tr><tr><td><strong>STIR > 30%</strong></td><td><strong>",
        format(censusStir$percent_more_than_30, big.mark = ","),
        "</strong></td></tr></table>"
      ),
      highlight = highlightOptions(
        weight = 5,
        color = "#696969",
        dashArray = "",
        fillOpacity = 0.5,
        bringToFront = TRUE)
    # ) %>%
    # addLegend(
    #   "bottomleft",
    #   pal = palStir,
    #   values = ~ percent_more_than_30,
    #   title = "Percentage of families with STIR > 30%",
    #   opacity = 0.5
    )

# STIR Lollipop
censusStir$Region <- as.character(censusStir$`Region Name`)
censusStirgg2 <- ggplot(censusStir, aes(x = Region, y = percent_more_than_30)) +
  geom_segment(aes(x = Region, xend = Region, y = 0, yend = percent_more_than_30), size = 1.5, color = colCommercial) +
  geom_point(color = colCommercial, size = 4, alpha = 0.8, shape = 21, stroke = 4) +
  theme_light() +
  coord_flip() +
  theme(
    panel.grid.major.y = element_blank(),
    panel.border = element_blank(),
    axis.ticks.y = element_blank()
  )

censusStirgg2
output$stirLollipop <- renderPlotly(
  ggplotly(censusStirgg2) %>%
    layout(
      xaxis = list(title = "")
    )
)

# STIR Stacked Bar
topLabels <- c('More than 30%', 'Less than 30%')
labelPositions <- c(
  censusStir[1,"percent_more_than_30"]$percent_more_than_30 / 2,
  censusStir[1,"percent_more_than_30"]$percent_more_than_30 +
    censusStir[1,"percent_less_than_30"]$percent_less_than_30 / 2
)

plot_ly(censusStir, x = ~percent_more_than_30, name = "More than 30%", y = ~Region, type = 'bar', orientation = 'h',
        marker = list(color = colCommercial,
                      line = list(color = 'rgb(248, 248, 249)', width = 1), hoverinfo="x+y+name")) %>%
  add_trace(x = ~percent_less_than_30, name = "Less than 30%", marker = list(color = colSingleFam)) %>%
  layout(xaxis = list(title = "",
                      showgrid = FALSE,
                      showline = FALSE,
                      showticklabels = FALSE,
                      zeroline = FALSE,
                      domain = c(0.15, 1)),
         yaxis = list(title = "",
                      showgrid = FALSE,
                      showline = FALSE,
                      showticklabels = FALSE,
                      zeroline = FALSE),
         barmode = 'stack',
         #paper_bgcolor = 'rgb(248, 248, 255)', plot_bgcolor = 'rgb(248, 248, 255)',
         margin = list(l = 70, r = 10, t = 70, b = 30),
         showlegend = FALSE)

  plot_ly(censusStir, x = ~percent_more_than_30, name = "More than 30%", y = ~Region, type = 'bar', orientation = 'h',
          marker = list(color = colCommercial,
                        line = list(color = 'rgb(248, 248, 249)', width = 1), hoverinfo="x+y+name")) %>%
    add_trace(x = ~percent_less_than_30, name = "Less than 30%", marker = list(color = colMultiFam)) %>%
    layout(xaxis = list(title = "",
                        showgrid = FALSE,
                        showline = FALSE,
                        showticklabels = FALSE,
                        zeroline = FALSE,
                        domain = c(0.15, 1)),
           yaxis = list(title = "",
                        showgrid = FALSE,
                        showline = FALSE,
                        showticklabels = FALSE,
                        zeroline = FALSE),
           barmode = 'stack',
           margin = list(l = 70, r = 10, t = 70, b = 30),
           showlegend = FALSE) %>%
    # labeling the y-axis
    add_annotations(xref = 'paper', yref = 'y', x = 0.14, y = ~`Region`,
                    xanchor = 'right',
                    text = ~`Region`,
                    font = list(family = 'Arial', size = 12,
                                color = 'rgb(67, 67, 67)'),
                    showarrow = FALSE, align = 'right') %>%
    # labeling the percentages of each bar (x_axis)
    add_annotations(xref = 'x', yref = 'y',
                    x = ~(percent_more_than_30 / 2), y = ~`Region`,
                    text = ~paste(percent_more_than_30, '%'),
                    font = list(family = 'Arial', size = 12,
                                color = 'rgb(67, 67, 67)'),
                    showarrow = FALSE) %>%
    add_annotations(xref = 'x', yref = 'y',
                    x = ~(percent_more_than_30 + percent_less_than_30 / 2), y = ~Region,
                    text = ~paste(percent_less_than_30, '%'),
                    font = list(family = 'Arial', size = 12,
                                color = 'rgb(67, 67, 67)'),
                    showarrow = FALSE) %>%
    # labeling the first Likert scale (on the top)
    add_annotations(xref = 'x', yref = 'paper',
                    x = labelPositions,
                    y = 1.05,
                    text = topLabels,
                    font = list(family = 'Arial', size = 12,
                                color = 'rgb(67, 67, 67)'),
                    showarrow = FALSE) %>%
    config(displayModeBar = F)
