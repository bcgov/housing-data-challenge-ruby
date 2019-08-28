#' Run the Shiny Application
#'
#' @export
#' @importFrom shiny shinyApp
#' @importFrom shiny runApp
#' @importFrom golem with_golem_options
run_app <- function(name = 'BC Housing', host = '0.0.0.0', port = 8080) {
  # shiny::runApp(system.file("app", package = "bchousing"), host = host, port = port, quiet = TRUE)
  golem::with_golem_options(
    app = shinyApp(
      ui = app_ui,
      server = app_server,
      options = list(port = port)
    ),
    golem_opts = list(name = name)
  )
}
