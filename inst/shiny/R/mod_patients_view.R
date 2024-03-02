patients_view_ui <- function(id) {
  ns <- NS(id)

  tagList(
    div(
      class = "upload-csv-container",
      upload_csv_file_ui(ns("visits"), .label = "Load psychiatric data:")
    ),
    pickerInput(ns("sel_patient"),
                label = "Please select a patient:",
                choices = c(),
                multiple = FALSE,
                options = list(`live-search` = TRUE, size = 25)),
    patient_data_ui(ns("patients_details"))
  )
}

patients_view <- function(id ) {
  moduleServer(
    id,
    function(input, output, session) {
      ns <- session$ns
      uploaded_data <- reactiveVal()

      visits_data <- upload_csv_file("visits")


      filter_dataset <- function(dataset, filter_value) {
        d <- Filter(function(x) x$patient_id == filter_value, dataset)
        unlist(d, recursive = FALSE)
      }

      observeEvent(visits_data(), {
        print(visits_data())
        uploaded_data(visits_data())
      })



      observeEvent(uploaded_data(), {
        updatePickerInput(
          session,
          inputId = "sel_patient",
          choices =  unique(uploaded_data()$patient_id)
        )
      })

      selected_patient_visits <- eventReactive(c(input$sel_patient, uploaded_data()), {
        req(uploaded_data(), input$sel_patient)
        uploaded_data() %>% filter(patient_id == input$sel_patient)
      })

      # server part
      patient_data("patients_details",
                   module_data = selected_patient_visits)
    }
  )
}

#### Submodules: ----
patient_data_ui <- function(id) {
  ns <- NS(id)
  render_details(
    "Visits History:",
    tabsetPanel(
      id = ns("visit_history"),
      tabPanel(
        "Table",
        div(class = "table-container", DT::dataTableOutput(ns("dt_visits_history")))
      ),
      tabPanel(
        "Graph",
        div(class = "plot-container", plotly::plotlyOutput(ns("plot_visits_history")))
      )
    )
  )
}


patient_data <- function(id, module_data = reactive() ) {
  moduleServer(
    id,
    function(input, output, session) {
      output$dt_visits_history <- DT::renderDataTable({
        validate(
          need(module_data(), "No visits to display")
        )
        selected_cols <- c('patient_id' , 'visit_date', 'hamd_suma', 'yms_suma', 'hamd_ymrs')
        module_data() %>% select(any_of(selected_cols))

      })

      output$plot_visits_history <- plotly::renderPlotly({
        validate(
          need(module_data(), "No visits data for plotting")
        )
        p <- ggplot2::ggplot(module_data(), aes(x = visit_date, y = hamd_ymrs, label = hamd_ymrs)) +
          geom_point() +
          geom_text(aes(label = paste(hamd_ymrs, paste0("HAMD: ",hamd_suma), paste0("YMS: ",yms_suma), paste0("DATE: ",as.Date(visit_date)),
                                      sep = "\n")), nudge_y = 0.2 ) +
          ggtitle("Visits History Over Time")

        plotly::ggplotly(p, tooltip = "text")
      })


    }
  )
}
