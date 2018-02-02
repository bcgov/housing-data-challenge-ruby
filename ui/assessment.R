tabPanel('BC Assessment',
         fluidPage(
           titlePanel("BC Assessment Data Visualization"),
           tags$p(
             "Current map is based on census division boundaries and BC Assessment data"
           ),
           tabsetPanel(tabPanel("Tab 1"),
                       tabPanel("Tab 2"),
                       tabPanel("Tab 3"))
         ))
