config_view_ui <- function(id) {
  ns <- NS(id)

  tagList(
    useShinyjs(),
    fluidRow(
      column(
        12,
        h2("Configuration Panel"),
        hr(),
        tags$p(
          tags$span("Please set Your local environment here. Specify inputs and outputs directories on Your disk."),
          tags$span(tags$details(
            tags$summary("More"),
            helpText("Inputs is the directory where Your source data is stored."),
            helpText("Outputs is the directory where all Your experiment results are stored.")
          )
        )),
        radioButtons(ns("rd_method"),
                     label = "",
                     choices = c("Select folders" = "by_selection",
                                 "Read configuration from YAML file" = "by_yaml"),
                     inline = TRUE,
                     width = "100%")
      ),
      column(
        6,
        div(
          id = ns("folder-selectors-container"),
          dir_button_ui(ns("btn_pick_inputs_dir"), "Choose INPUTS directory"),
          dir_button_ui(ns("btn_pick_outputs_dir"), "Choose OUTPUTS directory"),
          disabled(shinySaveButton(
            ns("btn_save"),
            "Save config...",
            title = "Saving configuration...",
            filetype = ".yml",
            filename = "config",
            buttonType = "primary",
            icon = icon(lib = "glyphicon", name = "save")
          ))
        ),
        shinyjs::hidden(div(
          id = ns("config-from-yaml-container"),
          load_config_ui(ns("load"))
        ))
      ),
      column(
        6,
        verbatimTextOutput(ns("txt_selected_input_dir"), placeholder = TRUE),
        verbatimTextOutput(ns("txt_selected_output_dir"), placeholder = TRUE)
      )
    )
  )
}

config_view <- function(id) {
  moduleServer(
    id,
    function(input, output, session) {
      VOLUMES <- c(Home = fs::path_home(), "R Installation" = R.home(), getVolumes()())
      INPUTS_DIR <- dir_button("btn_pick_inputs_dir", VOLUMES)
      OUTPUTS_DIR <- dir_button("btn_pick_outputs_dir", VOLUMES)

      loaded_config <- load_config("load")

      config <- reactiveVal()
      rv_inputs_dir <- reactiveVal()
      rv_outputs_dir <- reactiveVal()

      observe({
        saving_enabled <- isTruthy(INPUTS_DIR()) && isTruthy(OUTPUTS_DIR())
        shinyjs::toggleState("btn_save", condition = saving_enabled)
      })

      observeEvent(input$rd_method, {
        shinyjs::toggleElement(
          id = session$ns("folder-selectors-container"),
          condition = input$rd_method == "by_selection",
          asis = TRUE
        )
        shinyjs::toggleElement(
          id = session$ns("config-from-yaml-container"),
          condition = input$rd_method == "by_yaml",
          asis = TRUE
        )
      })

      observeEvent(INPUTS_DIR(), {
        rv_inputs_dir(INPUTS_DIR())
      })

      observeEvent(OUTPUTS_DIR(), {
        rv_outputs_dir(OUTPUTS_DIR())
      })

      observeEvent(c(rv_inputs_dir(), rv_outputs_dir()), {
        cnf <- new_bipolar_config(
          list(inputs = NA_character_, outputs = NA_character_)
        )
        io <- lapply(c(`in` = rv_inputs_dir(), `out` = rv_outputs_dir()), function(i) {
          ifelse(length(i) == 0, NULL, i)
        })
        if (length(io$`in`) != 0) {
          cnf <- set_inputs_location(cnf, io$`in`)
          Sys.setenv("INTPUTS_DIR" = io$`in`)
        }
        if (length(io$`out`) != 0) {
          cnf <- set_outputs_location(cnf, io$`out`)
          Sys.setenv("OUTPUTS_DIR" = io$`out`)
        }
        config(cnf)
      })

      shinyFileSave(
        input,
        "btn_save",
        roots = VOLUMES,
        session = session,
        restrictions = .libPaths()
      )

      observeEvent(loaded_config(), {
        config(loaded_config())
        .inputs <- loaded_config()$inputs
        .outputs <- loaded_config()$outputs
        rv_inputs_dir(.inputs)
        rv_outputs_dir(.outputs)
      })

      observeEvent(input$btn_save, {
        cnf <- create_bipolar_config(
          inputs = INPUTS_DIR(),
          outputs = OUTPUTS_DIR()
        )
        if (is.integer(input$btn_save)) {
          cat("No file-save path has been set (shinyFileSave)\n")
        } else {
          save_to <- parseSavePath(VOLUMES, input$btn_save)
          bipolar::save_bipolar_config(cnf, path = save_to$datapath)
          config(cnf)
        }
      })

      observeEvent(config(), {
        print(config())
      })

      output$txt_selected_input_dir <- renderText({
        rv_inputs_dir()
      })
      output$txt_selected_output_dir <- renderText({
        rv_outputs_dir()
      })

      return(config)
    }
  )
}


dir_button_ui <- function(id, .label) {
  ns <- NS(id)

  tagList(
    div(
      class = "dir-btn-container",
      # tags$label(.label),
      shinyDirButton(ns("btn_dir_selector"),
                     label = .label,
                     title = "Choose a directory",
                     icon = icon(lib = "glyphicon", name = "folder-open", class = "pad-8"),
                     viewtype = "list")
    )
  )
}

dir_button <- function(id, volumes) {
  moduleServer(
    id,
    function(input, output, session) {
      shinyDirChoose(input, "btn_dir_selector", roots = volumes)

      selected_dir <- eventReactive(input$btn_dir_selector, {
        parseDirPath(volumes, input$btn_dir_selector)
      })

      return(selected_dir)
    }
  )
}


load_config_ui <- function(id) {
  ns <- NS(id)

  fileInput(ns("fl_load_config_yaml"),
            label = "Load configuration file:",
            accept = c(".yaml", ".yml"))
}

load_config <- function(id) {
  moduleServer(
    id,
    function(input, output, session) {
      config <- eventReactive(input$fl_load_config_yaml, {
        file <- input$fl_load_config_yaml
        cnf <- bipolar::read_bipolar_config(file$datapath)
        if (!"bipolar.config" %in% class(cnf)) {
          showNotification(
            "Error while reading config file. Please check if the selected file is a proper configuration file.",
            type = "error"
          )
          return()
        }
        showNotification("Configuration file read successfully.", type = "message")
        return(cnf)
      })

      return(config)
    }
  )
}
