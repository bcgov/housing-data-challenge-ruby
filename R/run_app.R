#' Run the Shiny Application
#'
#' @export
#' @importFrom shiny shinyApp
#' @importFrom shiny runApp
run_app <- function(..., host = '0.0.0.0', port = '80') {
  shiny::runApp(system.file("app", package = "bchousing"), host = host, port = port, quiet = TRUE)
  # app = shiny::shinyApp(
  #   ui = app_ui(),
  #   server = app_server#,
  #   # options = c(
  #   #   'host' = '0.0.0.0',
  #   #   'port' = 8080
  #   # )
  # )
}
