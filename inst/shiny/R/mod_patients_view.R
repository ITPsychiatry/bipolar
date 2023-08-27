patients_view_ui <- function(id) {
  ns <- NS(id)

  tagList(
    pickerInput(ns("sel_patient"),
                label = "Please select a patient:",
                choices = c(),
                multiple = FALSE,
                options = list(`live-search` = TRUE, size = 25)),
    patient_data_ui(ns("patients_details"))
  )
}


patients_view <- function(id, visits_data = reactive(), calls_data = reactive()) {
  moduleServer(
    id,
    function(input, output, session) {
      ns <- session$ns

      filter_dataset <- function(dataset, filter_value) {
        d <- Filter(function(x) x$patient_id == filter_value, dataset)
        unlist(d, recursive = FALSE)
      }

      observeEvent(visits_data(), {
        updatePickerInput(
          session,
          inputId = "sel_patient",
          choices = sapply(visits_data(), `[[`, "patient_id") |> sort()
        )
      })

      selected_patient_visits <- eventReactive(c(input$sel_patient, visits_data()), {
        filter_dataset(visits_data(), input$sel_patient)
      })
      selected_patient_calls <- eventReactive(c(input$sel_patient, calls_data()), {
        filter_dataset(calls_data(), input$sel_patient)
      })

      # server part
      patient_data("patients_details",
                   module_data = selected_patient_visits,
                   calls_data = selected_patient_calls)
    }
  )
}

#### Submodules: ----
patient_data_ui <- function(id) {
  ns <- NS(id)
  uiOutput(ns("ui_details"))
}


patient_data <- function(id, module_data = reactive(), calls_data = reactive()) {
  moduleServer(
    id,
    function(input, output, session) {
      ns <- session$ns

      output$ui_details <- renderUI({
        req(module_data())

        div(
          tags$details(
            class = "patient-section",
            tags$summary(h4("Visits History:")),
            div(class = "table-container", DT::dataTableOutput(ns("dt_visits_history")))
          ),
          div(
            class = "patient-section",
            h4("Comments:"),
            helpText("Lorem ipsum dolor sit amet")
          ),
          tags$details(
            class = "patient-section",
            tags$summary(h4("Mobile Calls")),
            div(class = "table-container", DT::dataTableOutput(ns("dt_calls_history")))
          ),
          div(
            class = "patient-section",
            h4("Mobile Calls Summary"),
            helpText("Lorem ipsum dolor sit amet")
          )
        )
      })

      output$dt_visits_history <- DT::renderDataTable({
        validate(
          need(module_data(), "No visits to display")
        )

        module_data()$visits
      })

      output$dt_calls_history <- DT::renderDataTable({
        validate(
          need(calls_data(), "No calls to display")
        )

        calls_data()$calls
      })
    }
  )
}
