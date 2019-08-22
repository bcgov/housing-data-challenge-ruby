tabPanel(
  "Home",
  fluidPage(
    jumbotron(
      header = "BC Housing Market",
      popPerc = c16Prov$Population.Change,
      popInc = TRUE,
      dwellPerc = c16Prov$Total.Private.Dwellings.Change,
      dwellInc = TRUE,
      trans_period = maxTransPeriod,
      no_mkt_trans = no_mkt_trans,
      no_foreign_perc = no_foreign_perc,
      sum_FMV = sum_FMV,
      sum_FMV_foreign_perc = sum_FMV_foreign_perc
    ),
    fluidRow(
      id = "splash-intro",
      tags$div(
        id = "splash-intro-text",
        HTML(
          paste0(
            "This platform is the result of the ",
            tags$strong("Data Visualization Challenge"),
            " launched by the ",
            tags$a(href="http://www.bcic.com", "BC Innovation Council"),
            " in partnership with ",
            tags$a(href = "https://www2.gov.bc.ca/gov/content/data/about-data-management/bc-stats", "BC Stats"),
            " in December 2016. The challenge aimed to develop a tool to visualize
            BC Stats housing data in a more meaningful and impactful way. It provides BC Stats with an innovative tool
            for better use of their data and, and at the same time, offers interested BC residents, government agencies,
            and non-profits the opportunity to interact with, understand and make decisions based on community growth
            and housing data. The platform is currently in its beta-version, users are invited to provide ",
            tags$a("feedback"),
            " on their use of this iteration including ideas as to how it could be advanced
            to further fulfill its role."
          )
        )
      ),
      tags$hr(),
      tags$div(
        id = "splash-intro-credit",
        HTML(
          paste("Photo by", tags$a(
            href = "https://unsplash.com/photos/596V9_X_naI?utm_source=unsplash&utm_medium=referral&utm_content=creditCopyText",
            "Spencer Watson"), "on", tags$a(
              href = "https://unsplash.com/search/photos/vancouver?utm_source=unsplash&utm_medium=referral&utm_content=creditCopyText", "Unsplash.")
          )
        )
      )
    )
  )
)
