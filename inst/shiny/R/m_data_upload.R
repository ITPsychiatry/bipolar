data_management_ui <- function(id) {
  ns <- NS(id)

  tagList(
    upload_csv_file_ui(ns("visits"), .label = "Visits Data:"),
    upload_csv_file_ui(ns("calls"), .label = "Mobile Calls Data:")
  )
}


data_management <- function(id) {
  moduleServer(
    id,
    function(input, output, session) {
      uploaded_data <- reactiveValues()

      visits_data <- upload_csv_file("visits")
      mobile_calls_data <- upload_csv_file("calls")

      observeEvent(visits_data(), {
        uploaded_data$visits <- visits_data()
      })
      observeEvent(mobile_calls_data(), {
        uploaded_data$mobile_calls <- mobile_calls_data()
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
    class = "upload-csv-container",
    column(
      4,
      fileInput(ns("fl_csv"), .label),
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
      csv_data <- eventReactive(input$fl_csv, {
        file <- input$fl_csv
        print(file$datapath)
        ext <- tools::file_ext(file$datapath)

        validate(
          need(ext == "csv", "Please upload a csv file.")
        )

        tryCatch(
          data.table::fread(file$datapath, sep = input$sel_separator, header = input$chb_header)
        )
      })

      output$data_preview <- DT::renderDT({
        validate(
          need(csv_data(), "No data available")
        )

        preview <- head(csv_data())
        DT::datatable(preview,
                      options = list(scrollX = TRUE, dom = "t"),
                      class = "preview-datatable",
                      rownames = FALSE,
                      selection = "single")
      })

      return(csv_data)
    }
  )
}

