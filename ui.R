ui <- navbarPage(
  theme = "css/bcgov.css",
  title = "British Columbia Housing Market Data Visualization",
  source(file.path("ui", "home.R"),  local = TRUE)$value,
  source(file.path("ui", "census.R"),  local = TRUE)$value,
  source(file.path("ui", "ptt.R"),  local = TRUE)$value,
  source(file.path("ui", "census_search.R"),  local = TRUE)$value
)
