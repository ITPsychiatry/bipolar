#' Runs shiny dashboard
#'
#' @param .port integer; which port to allocate? 7777 is the default
#' @import shiny
#' @import DT
#' @importFrom shinyWidgets pickerInput
#' @import flextable
#'
#' @export
run_shiny <- function(.port = 7777) {
  shiny::runApp(
    system.file(
      package = "bipolar",
      file.path("shiny", "app.R")
    ),
    port = .port,
    host = "0.0.0.0"
  )
}
