# S3 class constructor: ----
bipolar_config <- function(x) {
  cnf <- structure(
    x,
    class = append("bipolar.config", class(x))
  )
  return(cnf)
}

# validator
validate_bipolar_config <- function(x) {
  stopifnot(is.list(x))
  stopifnot(all(c("inputs", "outputs") %in% names(x)))
  return(x)
}

#' User-friendly bipolar config object creator
#'
#' @param x should be a list with two named elements: inputs and outputs
#'
#' @return bipolar.config-class object
#' @export
new_bipolar_config <- function(x) {
  cnf <- validate_bipolar_config(bipolar_config(x))
  Sys.setenv("INPUTS_DIR" = cnf$inputs)
  Sys.setenv("OUTPUTS_DIR" = cnf$outputs)
  return(cnf)
}

# Yaml read/save: -----
#' Reads bipolar configuration from a YAML file
#'
#' @family bipolar-config
#'
#' @param path character; path to the yaml file
#'
#' @return bipolar.config-class object
#' @export
read_bipolar_config <- function(path = NULL) {
  config <- tryCatch({
    cnf <- yaml::read_yaml(path)
    new_bipolar_config(cnf)
  },
  error = function(err) {
    warning("Couldn't read bipolar config file.\n", err$message)
    return(NULL)
  })
  return(config)
}

#' Saves bipolar configuration object to YAML file
#'
#' @rdname bipolar-config-manipulation
#'
#' @family bipolar-config
#'
#' @description
#' \code{save_bipolar_config} saves bipolar configuration object in a YAML format.
#' \code{set_inputs_location} and \code{set_outputs_location} are convenient methods to modify the
#' paths of a configuration object
#'
#'
#' @param config bipolar.config-class object
#' @param path character; where to store the file
#'
#' @export
save_bipolar_config <- function(config, path) {
  stopifnot("bipolar.config" %in% class(config))
  yaml::write_yaml(config, file = path)
}

# Setters: ----
#' @rdname bipolar-config-manipulation
#' @export
set_inputs_location <- function(config, path) {
  stopifnot("bipolar.config" %in% class(config))
  stopifnot(is.character(path))
  config$inputs <- path
  return(config)
}

#' @rdname bipolar-config-manipulation
#' @export
set_outputs_location <- function(config, path) {
  stopifnot("bipolar.config" %in% class(config))
  stopifnot(is.character(path))
  config$outputs <- path
  return(config)
}

# Create config:----
#' Creates bipolar configuration object.
#'
#' @family bipolar-config
#'
#' @description
#' Bipolar configuration requires defining paths to two directories:
#' \item{inputs} - the directory where input datasets are stored
#' \item{outputs} - where all experimentation/modeling results will be stored
#'
#' @param inputs character; path to the location on disk where inputs are stored
#' @param outputs character; path to the location on disk where experiment outputs will be stored
#'
#' @details By default, \code{inputs} and \code{outputs} arguments are \code{NULL}. In this case inputs
#' and outputs directories will be created as subdirectories of the current working directory
#' (\code{getwd()} is used.)
#' A side effect of creation of a config object is setting two environment variables, namely
#' \code{INPUTS_DIR} and \code{OUTPUTS_DIR}. The main advantage of this effect is that inputs/outputs
#' locations can be obtained easily.
#'
#' @return bipolar.config-class object
#' @export
create_bipolar_config <- function(inputs = NULL, outputs = NULL) {
  base <- list(inputs = inputs, outputs = outputs)
  in_out <- lapply(names(base), function(io) {
    if (is.null(base[[io]])) return(file.path(getwd(), io))
    return(base[[io]])
  })
  names(in_out) <- names(base)
  for (e in in_out) {
    if (!dir.exists(e)) dir.create(e, recursive = TRUE)
  }
  new_bipolar_config(in_out)
}

# Read/save data:----
#' Read/save model outputs
#'
#' @family bipolar-config
#'
#' @rdname read-save-outputs
#'
#' @description
#' When experimenting a lot of different parameter sets may be tested as well as input data. Storing
#' the results for later analysis and deeper insights is a key aspect of modeling process and
#' experimentation in general. Here, the two functions are responsible for saving and reading the
#' outcomes from experimentation.
#'
#' @details Results are stored as RDS files via base R \code{saveRDS} function. For reading saved objects
#' back the reverse \code{readRDS} function is used.
#' If the outputs directory doesn't exist it will be created.
#' \code{list_results} function returns a data frame with information about all results stored in the
#' outputs directory. You can use it to get proper names for the assets You want to load.
#'
#' @param x R object to be saved
#' @param name character; for saving - the name of the resulting file; for reading - the name of the
#' resource to be loaded
#'
#' @export
save_output <- function(x, name) {
  out_dir <- Sys.getenv("OUTPUTS_DIR")
  if (nchar(out_dir) == 0)
    stop("You haven't configured Your environment yet. Call `crate_bipolar_config()` first or load it from a yaml file.")

  if (!dir.exists(out_dir))
    dir.create(out_dir)

  saveRDS(x, file = file.path(out_dir, name))
  message("Your results were saved successfully")
}

#' @rdname read-save-outputs
#' @export
load_results <- function(name) {
  out_dir <- Sys.getenv("OUTPUTS_DIR")
  readRDS(file.path(out_dir, name))
}

#' @rdname read-save-outputs
#' @export
list_results <- function() {
  out_dir <- Sys.getenv("OUTPUTS_DIR")
  if (nchar(out_dir) == 0)
    stop("You haven't configured Your environment yet. Call `crate_bipolar_config()` first or load it from a yaml file.")
  fls <- list.files(out_dir, full.names = TRUE)
  lapply(fls, file.info) %>%
    dplyr::bind_rows() %>%
    tibble::rownames_to_column(var = "full_path") %>%
    mutate(basename = basename(full_path)) %>%
    tibble::column_to_rownames(var = "basename") %>%
    mutate(`size (kB)` = ceiling(size / 1024)) %>%
    rename(modification_time = mtime,
           last_status_change = ctime,
           last_access_time = atime) %>%
    select(-size)
}
