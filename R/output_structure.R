#' A function to retrieve colnames of features that are identified with
#' prefix "X_".
#'
#' @param names character vector containing raw column names
#' @param original_names boolean, whether to keep raw names (if FALSE) or
#' remove "X_" prefix (if TRUE)
#'
#' @return character vector
#'
#' @examples
#' \dontrun{
#' df <- data.frame("a" = 1, "b" = 2, "X_ffa" = -99, "X_long_name" = NA)
#' get_acoustic_params(colnames(df))
#' }
#'
get_acoustic_params <- function(names, original_names = TRUE){
  feature_columns_names <- names[grepl("X_", names)]

  if (original_names) {
    output <- substring(feature_columns_names, 3)
  } else {
    output <- feature_columns_names
  }

  return(output)
}


#' Create a structure required to run shiny dashboards.
#'
#' @param dt_output dataframe with outcomes of prediction
#' it should contain specific columns (TBD)
#' @param model_name <str> the name of the model
#' @param hyperparams <list of strings>
#' @param validation_setting <str> the name of the validation scenario
#' @param .metric <str / list of strings> validation performance metrics,
#' should align with the names of results from caret::confusionMatrix
#'
#' @return List with multiple elements required for specific dashboards.
#' @export
#'
#' @examples
#' \dontrun{
#' cnf <- create_bipolar_config(
#'   inputs = file.path(Sys.getenv("DATA_REPO"), "inputs"),
#'   outputs = file.path(Sys.getenv("DATA_REPO"), "outputs")
#' )
#' dt_output <- readRDS(file.path(cnf$inputs, "validation_df2.rds"))
#' output <- create_information_for_dashboards(
#'   dt_output = dt_output,
#'   model_name = 'ssfcm_2patients',
#'   hyperparams = list(
#'     alfa = 1.5,
#'     beta = 5.5,
#'     gamma = 10,
#'     single = FALSE,
#'     type = 'boost'
#'   ),
#'   validation_setting = "Foo_validation",
#'   .metric = c("F1", "Precision", "Recall")
#' )
#' }
#'
create_information_for_dashboards <- function(
    dt_output,
    model_name,
    hyperparams,
    validation_setting,
    .metric = "F1"
){
  metric <- match.arg(.metric, c("F1", "Precision", "Recall"), several.ok = TRUE)
  if(length(.metric) != length(metric)) {
    warning(">= 1 values of `arg` did not match choices")}

  patients_ids <- unique(dt_output$patient_id)
  acoustic_params <- colnames(dt_output)[grepl("X_", colnames(dt_output))] |> substring(3)

  performance_metrics <- bipolar::calculate_metrics(
    true_y = dt_output$true,
    predicted_y = dt_output$predict,
    metric = metric)

  performance_metrics <- setNames(
    object = select(tail(performance_metrics, 1), -Class) |> as.numeric() |> as.list(),
    metric
  )

  confusion_matrix <- caret::confusionMatrix(
    table(dt_output$true, dt_output$predict),
    mode = "everything"
  )

  list(patients_id = unique(dt_output$patient_id),
       acoustic_params = get_acoustic_params(colnames(dt_output)),
       model_name = model_name,
       hyperparams = hyperparams,
       dt_output = dt_output,
       performance_metrics = performance_metrics,
       confusion_matrix = confusion_matrix,
       validation_setting = validation_setting)
}
