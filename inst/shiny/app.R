library(bipolar)
library(shiny)
library(DT)
library(shinyWidgets)
library(ggplot2)
library(shinyFiles)
library(flextable)
library(shinyjs)
library(shinycssloaders)


# increase the size limit for file upload (https://groups.google.com/g/shiny-discuss/c/rU3vwGMZexQ/m/zeKhiYXrtEQJ)
options(shiny.maxRequestSize=200*1024^2)

#### Main app: -----
ui <- navbarPage(
  title = "BIPOLAR",

  tabPanel("Configuration",
           config_view_ui("io_config")),
  tabPanel("Explore Patients",
           patients_view_ui("v_patients")),
  tabPanel("Explore Model",
           output_view_ui("v_output")),
  header = includeCSS("www/styles.css")
)


server <- function(input, output, session) {
  app_config <- config_view("io_config")
  # reactive datasets
  patients_visits <- reactiveVal()
  patients_calls <- reactiveVal()
  model_output <- reactiveVal()

  patients_view("v_patients")
  output_view("v_output", app_config)

  app_data <- data_management("data_csv_upload")

  observeEvent(app_data()$visits, {
    app_data_list <- reactiveValuesToList(app_data())

    visits_by_patient_id <- app_data_list$visits
    .patients_visits <- visits_by_patient_id$patient_id %>%
      unique() %>%
      lapply(function(pid) {
        d <- filter(visits_by_patient_id, patient_id == pid)
        list(
          patient_id = pid,
          visits = d
        )
      })
    patients_visits(.patients_visits)
  })

  observeEvent(app_data()$mobile_calls, {
    app_data_list <- reactiveValuesToList(app_data())

    calls_by_patient_id <- app_data_list$mobile_calls
    .patients_calls <- calls_by_patient_id$patient_id %>%
      unique() %>%
      lapply(function(pid) {
        d <- filter(calls_by_patient_id, patient_id == pid)
        list(
          patient_id = pid,
          calls = d
        )
      })
    patients_calls(.patients_calls)
  })

  observeEvent(app_data()$output, {
    app_data_list <- reactiveValuesToList(app_data())

    output_by_patient_id <- app_data_list$output$dt_output

    .model_output <- list(patients_id = app_data_list$output$patients_id,
                          acoustic_params = app_data_list$output$acoustic_params,
                          model_name = app_data_list$output$model_name,
                          hyperparams = app_data_list$output$hyperparams,
                          dt_output = app_data_list$output$dt_output,
                          confusion_matrix = app_data_list$output$confusion_matrix

         )

    model_output(.model_output)
  })

}

shinyApp(ui, server)
