ui <- navbarPage(
  theme = "css/bcgov.css",
  title = "BC Housing Data visualization",
  source(file.path("ui", "home.R"),  local = TRUE)$value,
  source(file.path("ui", "census.R"),  local = TRUE)$value,
  source(file.path("ui", "ptt.R"),  local = TRUE)$value,
  source(file.path("ui", "about.R"),  local = TRUE)$value,
  # source(file.path("ui", "census_search.R"),  local = TRUE)$value
  tabPanel("Feedback", id = "feedback")
)
