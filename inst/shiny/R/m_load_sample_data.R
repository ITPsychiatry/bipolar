button_load_sample_data_ui <- function(id, .label = "Load Sample Data") {
  ns <- NS(id)

  tagList(
    actionButton(ns("btn_load"),
                 label = .label,
                 class = "btn-sample-data-load")
  )
}


button_load_sample_data <- function(id, read_function = readRDS, ...) {
  moduleServer(
    id,
    function(input, output, session) {
      loaded_data <- eventReactive(input$btn_load, {
        tryCatch(read_function(...),
                 error = function(err) {
                   showNotification(
                     type = "error",
                     sprintf("Error while loading package sample data. %s", err$message)
                   )
                   return(NULL)
                 })
      })
      return(loaded_data)
    }
  )
}

