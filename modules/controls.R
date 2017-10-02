ctrlPeriodInput <- function(id, opts, label = "Period") {
  ns <- NS(id)

  tagList(selectInput(id,
                      label,
                      opts,
                      multiple = FALSE))
}

ctrlPeriod <- function(input, output, session, data, view) {

}
