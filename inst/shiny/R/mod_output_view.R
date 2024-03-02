output_view_ui <- function(id) {
  ns <- NS(id)

  tagList(
    div(
      id = "experiment-load-container",
      experiment_selector_ui(ns("outputs_selector")),
      div(
        id = "button-experiment-load",
        button_load_sample_data_ui(ns("sample_experiment"))
      )
    ),

    pickerInput(
      ns("sel_model"),
      label = "Please select a model:",
      choices = c(),
      multiple = FALSE,
      options = list(`live-search` = TRUE, size = 25)
    ),
    pickerInput(
      ns("sel_patient"),
      label = "Please select a patient:",
      choices = c(),
      multiple = FALSE,
      options = list(`live-search` = TRUE, size = 25)
    ),
    pickerInput(
      ns("sel_param"),
      label = "Please select a parameter:",
      choices = c(),
      multiple = FALSE,
      options = list(`live-search` = TRUE, size = 25)
    ),
    output_data_ui(ns("output_details")),

    render_details(
      "Model Performance Summary",
      div(class = "table-container",
          render_performance_flextable_ui(ns("flx_cv_table")))
    ),
    render_details(
      "Confusion Matrix and Statistics",
      div(class = "table-container",
          render_confusion_matrix_ui(ns("confusion_matrix")))
    )
  )
}


output_view <- function(id, app_config = reactive()) {

  moduleServer(
    id,
    function(input, output, session) {
      ns <- session$ns

      model_data <- reactiveVal()

      sample_data <- button_load_sample_data(
        "sample_experiment",
        file = system.file(package = "bipolar",
                           file.path("sample_data", "experiment_01.rds"))
      )

      observeEvent(sample_data(), {
        model_data(sample_data())
      })

      experiment_path <- experiment_selector("outputs_selector", app_config)

      uploaded_data <- eventReactive(experiment_path(), {
        req(experiment_path())
        exp_name <- basename(experiment_path())
        tryCatch(load_results(exp_name),
                 error = function(err) NULL)
      })

      observeEvent(uploaded_data(), {
        model_data(uploaded_data())
      })

      observeEvent(model_data(), {
        updatePickerInput(
          session,
          inputId = "sel_patient",
          choices = model_data()$patients_id
        )
      })

      observeEvent(model_data(), {
        updatePickerInput(
          session,
          inputId = "sel_param",
          choices =  model_data()$acoustic_params

        )
      })

      observeEvent(model_data(), {
        updatePickerInput(
          session,
          inputId = "sel_model",
          choices = model_data()$model_name
        )
      })

      selected_patient_param <- reactive({
        req(model_data()$dt_output, input$sel_patient)
        model_data()$dt_output %>%
          filter(patient_id == input$sel_patient) %>%
          select(-starts_with("X"), ends_with(input$sel_param) )

      })

      selected_param <- eventReactive(c(input$sel_param, model_data()), {
        input$sel_param
      })

      output_data("output_details",
                  patient_param_data = selected_patient_param,
                  param_name = selected_param)

      render_performance_flextable("flx_cv_table", model_data)

      render_confusion_matrix("confusion_matrix", model_data)
    }
  )
}

#### Submodules: ----
output_data_ui <- function(id) {
  ns <- NS(id)
  tagList(
    render_details(
      "Output:",
      div(
        div(class = "plot-container",
            plotly::plotlyOutput(ns("plot_output")) %>% withSpinner()),
        div(class = "plot-container",
            plotly::plotlyOutput(ns("plot_output_features")) %>% withSpinner())
      )
    )
  )
}


output_data <- function(id,
                        patient_param_data = reactive(),
                        param_name = reactive()) {
  moduleServer(
    id,
    function(input, output, session) {
      ns <- session$ns

      output$plot_output <- plotly::renderPlotly({
        validate(
          need(patient_param_data(), "No output data for plotting")
        )
        df = patient_param_data()
        p <- ggplot2::ggplot(df[!is.na(df$true), ], aes(x =  create_date  )) +
          geom_point(aes(y = 'true', color = as.factor(true),
                         text = paste("True Value:", true
                         )))  +
          geom_point(aes(y = 'predicted', color = as.factor(predict),
                         text = paste("Predicted Value:",  predict,
                                      "<br>Depression:", round(depression,3),
                                      "<br>Mania:", round(mania,3),
                                      "<br>Euthymia:", round(euthymia,3),
                                      "<br>Mixed:", round(mixed,3)))) +
          ggtitle("True vs Predicted") +
          xlab("Date") +
          scale_color_brewer(palette = "Set1") +
          scale_y_discrete(labels = c("1" = "True", "2" = "Predict")) +
          labs(color = "BD state") +
          theme(axis.text.x = element_text(angle = 90))

        plotly::ggplotly(p, tooltip = "text")
      })

      output$plot_output_features <- plotly::renderPlotly({
        validate(
          need(patient_param_data(), "No output data for plotting")
        )
        df <- patient_param_data()
        y_col <- param_name()
        df <- df[!is.na(df$true), ]

        p <- ggplot2::ggplot(df, aes(x =  create_date  )) +
          geom_point(aes(y = df[,grep(y_col,names(df))] ,
                         text = paste("Create Date:", create_date ,
                                      "<br>Value:", df[,grep(y_col,names(df))]
                         )))  +
          xlab("Date") +
          ggtitle("Sensor data over time") +
          scale_color_brewer(palette = "Set1") +
          labs(color = "BD state", y = y_col) +
          theme(axis.text.x = element_text(angle = 90))

        plotly::ggplotly(p, tooltip = "text")
      })
    }
  )
}
