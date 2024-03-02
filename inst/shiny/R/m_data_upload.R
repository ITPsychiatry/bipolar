data_management_ui <- function(id) {
  ns <- NS(id)

  tagList(
    upload_csv_file_ui(ns("visits"), .label = "Visits Data:"),
    upload_csv_file_ui(ns("calls"), .label = "Mobile Calls Data:"),
    upload_rds_file_ui(ns("output"), .label = "Output Data:")
  )
}


data_management <- function(id) {
  moduleServer(
    id,
    function(input, output, session) {
      uploaded_data <- reactiveValues()

      visits_data <- upload_csv_file("visits")
      mobile_calls_data <- upload_csv_file("calls")
      output_data <- upload_rds_file("output")

      observeEvent(visits_data(), {
        uploaded_data$visits <- visits_data()
      })
      observeEvent(mobile_calls_data(), {
        uploaded_data$mobile_calls <- mobile_calls_data()
      })
      observeEvent(output_data(), {
        uploaded_data$output <- output_data()
      })


      return(reactive(
        uploaded_data
      ))
    }
  )
}



upload_csv_file_ui <- function(id, .label) {
  ns <- NS(id)

  fluidRow(
    column(
      4,
      div(
        id = "psychiatric-data-load-container",
        fileInput(ns("fl_csv"), .label),
        div(
          id = "button-psychiatric-data-load",
          button_load_sample_data_ui(ns("sample_psychiatric_data"))
        )
      ),
      div(
        actionLink(ns("toggle_upload_options"), label = "Show/Hide Upload Options"),
        conditionalPanel(
          ns = ns,
          condition = "input.toggle_upload_options % 2 == 1",
          checkboxInput(
            ns("chb_header"),
            label = "Header Row",
            value = TRUE
          ),
          selectInput(
            ns("sel_separator"),
            label = "Separator",
            choices = c(";", "|", ",", "space", "tab"),
            selected = ","
          )
        )
      )
    ),
    column(
      8,
      DT::DTOutput(ns("data_preview"))
    )

  )
}


upload_csv_file <- function(id) {
  moduleServer(
    id,
    function(input, output, session) {
      uploaded_data <- reactiveVal()

      sample_data <- button_load_sample_data(
        "sample_psychiatric_data",
        file = system.file(package = "bipolar",
                           file.path("sample_data", "psychiatric_data_synthetic.RDS"))
      )

      observeEvent(sample_data(), {
        d <- sample_data() %>% mutate(visit_date = as.POSIXct(visit_date, format = "%d.%m.%Y %H:%M"))
        d <- d %>% arrange(patient_id)
        uploaded_data(d)
      })

      csv_data <- eventReactive(input$fl_csv, {
        file <- input$fl_csv
        print(file$datapath)
        ext <- tools::file_ext(file$datapath)

        validate(
          need(ext == "csv", "Please upload a csv file.")
        )

        tryCatch(
          data.table::fread(file$datapath,
                                 sep = input$sel_separator,
                                 header = input$chb_header) %>%
            arrange(patient_id)
        )
      })

      observeEvent(csv_data(), {
        uploaded_data(csv_data())
      })

      output$data_preview <- DT::renderDT({
        validate(
          need(uploaded_data(), "")
        )

        preview <- head(uploaded_data())
        DT::datatable(preview,
                      options = list(scrollX = TRUE, dom = "t"),
                      class = "preview-datatable",
                      rownames = FALSE,
                      selection = "single")
      })

      return(uploaded_data)
    }
  )
}

##


upload_rds_file_ui <- function(id, .label) {
  ns <- NS(id)

  fluidRow(
    class = "upload-rds-container",
    column(
      4,
      fileInput(ns("fl_rds"), .label, accept = c('.rds')),
    ),
    column(
      8,
      DT::DTOutput(ns("data_preview"))
    )
  )
}


upload_rds_file <- function(id) {
  moduleServer(
    id,
    function(input, output, session) {
      rds_data <- eventReactive(input$fl_rds, {
        file <- input$fl_rds
        if (is.null(file)) return(NULL)

        ext <- tools::file_ext(file$datapath)
        validate(
          need(ext == "rds", "Please upload an RDS file.")
        )

        tryCatch(
          readRDS(file$datapath )
        )
      })

      output$data_preview <- DT::renderDT({
        validate(
          need(rds_data(), "No data available")
        )

        preview <- data.table::data.table(names=names(rds_data()))
        DT::datatable(preview)

      })

      return(rds_data)
    }
  )
}
