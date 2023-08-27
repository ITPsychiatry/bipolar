library(bipolarPreprocessing)
library(shiny)
library(DT)
library(shinyWidgets)

# increase the size limit for file upload (https://groups.google.com/g/shiny-discuss/c/rU3vwGMZexQ/m/zeKhiYXrtEQJ)
options(shiny.maxRequestSize=200*1024^2)

# mocked data:
DATA <- list(
  patients = list(
    list(
      patient_id = "0001",
      visits = dplyr::tibble(
        visit_date = seq.Date(as.Date("2023-01-13"), as.Date("2023-05-01"), length.out = 5),
        type = c("initial", "control", "control", "control", "intervention"),
        hamilton_sum = sample(5:15, 5),
        young_sum = sample(5:15, 5),
        state = c("mixed", "depression", "euthymia", "mixed", "mixed")
      )
    ),
    list(
      patient_id = "0002",
      visits = dplyr::tibble(
        visit_date = seq.Date(as.Date("2023-02-13"), as.Date("2023-04-01"), length.out = 3),
        type = c("initial", "control", "control"),
        hamilton_sum = sample(5:15, 3),
        young_sum = sample(5:15, 3),
        state = c("mixed", "depression", "euthymia")
      )
    )
  )
)


#### Main app: -----
ui <- navbarPage(
  title = "Bipolar Data Browser",
  tabPanel("Data Source",
           data_management_ui("data_csv_upload")),
  tabPanel("Patients",
           patients_view_ui("v_patients")),
  tabPanel("Model Output"),
  header = includeCSS("www/styles.css")
)


server <- function(input, output, session) {
  # reactive datasets
  patients_visits <- reactiveVal()
  patients_calls <- reactiveVal()

  patients_view("v_patients",
                visits_data = patients_visits,
                calls_data = patients_calls)
  app_data <- data_management("data_csv_upload")

  observeEvent(app_data()$visits, {
    app_data_list <- reactiveValuesToList(app_data())
    # prepare patients_visits
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
    # prepare patients_visits
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
}

shinyApp(ui, server)
