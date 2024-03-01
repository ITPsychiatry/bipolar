# The main module (not used now, but the layout may be useful as an alternative separate view)
model_outputs_ui <- function(id) {
  ns <- NS(id)
  tagList(
    div(
      experiment_selector_ui(ns("outputs_selector"))
    ),
    render_details(
      "Model Performance Summary",
      div(class = "table-container",
          render_performance_flextable_ui(ns("flx_cv_table")))
    ),
    render_details(
      "Confusion Matrix and Statistics",
      div(class = "table-container",
          render_confusion_matrix_ui("confusion_matrix"))
    )
  )
}


model_outputs <- function(id, app_config = reactive()) {
  moduleServer(
    id,
    function(input, output, session) {
      experiment_path <- experiment_selector("outputs_selector", app_config)

      selected_experiment <- eventReactive(experiment_path(), {
        req(experiment_path())
        exp_name <- basename(experiment_path())
        tryCatch(load_results(exp_name),
                 error = function(err) NULL)
      })

      render_performance_flextable("flx_cv_table", selected_experiment)
      render_confusion_matrix("confusion_matrix", selected_experiment)
    }
  )
}

# Submodules: ----
experiment_selector_ui <- function(id) {
  ns <- NS(id)

  tagList(selectInput(
    ns("sel_experiment"),
    "Please choose an experiment:",
    choices = list()
  ))
}


experiment_selector <- function(id, app_config = reactive()) {
  moduleServer(
    id,
    function(input, output, session) {
      observeEvent(app_config(), {
        choices <- tryCatch(
          list.files(app_config()$outputs, full.names = TRUE),
          error = function(err) {
            return(list())
          }
        )
        named_choices <- c(choices) |> setNames(basename(choices))
        updateSelectInput(session = session,
                          inputId = "sel_experiment",
                          choices = named_choices)
      })

      return(reactive(input$sel_experiment))
    }
  )
}


render_performance_flextable_ui <- function(id) {
  ns <- NS(id)
  uiOutput(ns("model_performance"))
}

render_performance_flextable <- function(id, experiment = reactive()) {
  moduleServer(
    id,
    function(input, output, session) {
      output$model_performance <- renderUI({
        validate(need(
          experiment()$performance_metrics,
          "Improper structure of the experiment."
        ))
        experiment()$performance_metrics |>
          as.data.frame() |>
          flextable() |>
          bold(part = "header") |>
          colformat_double(digits = 3) |>
          autofit() |>
          htmltools_value()
      })
    }
  )
}

render_confusion_matrix_ui <- function(id) {
  ns <- NS(id)
  uiOutput(ns("flx_conf_matrix"))
}

render_confusion_matrix <- function(id, experiment = reactive()) {
  moduleServer(
    id,
    function(input, output, session) {
      output$flx_conf_matrix <- renderUI({
        validate(need(
          experiment()$confusion_matrix,
          "Improper structure of the experiment."
        ))
        cm_table <- experiment()$confusion_matrix$table |> as.data.frame()
        colnames(cm_table) <- c("Prediction", "Reference", "Freq")
        cm_table <- cm_table %>%
          tidyr::pivot_wider(names_from = "Reference", values_from = "Freq")
        cm_table %>%
          flextable() %>%
          add_header_row(values = c(colnames(cm_table)[1], rep("Reference", ncol(cm_table) - 1))) %>%
          merge_h(part = "header") |>
          merge_v(part = "header") |>
          autofit() |>
          align(part = "all", align = "center") |>
          htmltools_value()
      })
    }
  )
}
