pdf(NULL)

server <- function(input, output, session) {
  # Include the logic (server)
  source(file.path("server", "ptt.R"),  local = TRUE)$value
  source(file.path("server", "census.R"),  local = TRUE)$value
  source(file.path("server", "census_search.R"),  local = TRUE)$value

  # Enable bookmarking
  # observe({
  #   reactiveValuesToList(input)
  #   session$doBookmark()
  # })
  # onBookmarked(function(url) {
  #   updateQueryString(url)
  # })
}
