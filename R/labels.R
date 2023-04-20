# Constants
BD_LABELS <- c("mania", "depression", "mixed", "euthymia")

#' Transforms column into label with healthy-unhealthy values
#'
#' @param dataset data.frame
#' @param label_name character; the name of a column storing state values (mania, depression etc.)
#'
#' @importFrom dplyr %>% mutate case_when
#'
#' @return data.frame
#' @export
#'
#' @examples
#' visits <- get_sample_visits()
#' transform_label_healthy_unhealthy(visits, "hamd_ymrs")
transform_label_healthy_unhealthy <- function(dataset, label_name) {
  dataset <- dataset %>% mutate(
    label = case_when(
      dataset[[label_name]] == 'mania' ~ "unhealthy",
      dataset[[label_name]] == 'depression' ~ "unhealthy",
      dataset[[label_name]] == 'mixed' ~ "unhealthy",
      dataset[[label_name]] == 'euthymia' ~ "healthy"
    )
  )
  return(dataset)
}

#' Transforms column with states into custom label
#'
#' @param dataset data.frame
#' @param label_name character
#'
#' @return data.frame
#' @export
transform_label_custom <- function(dataset, label_name) {
  dataset["label"] = dataset[label_name]
  return(dataset)
}

#' Transform column into cgi label
#'
#' @param dataset data.frame
#' @param label_name character
#'
#' @importFrom dplyr %>% distinct pull
#'
#' @return dataset
#' @export
#'
#' @examples
#' visits <- get_sample_visits()
#' transform_label_healthy_unhealthy(visits, "hamd_ymrs")
transform_label_cgi <- function(dataset, label_name) {
  tryCatch({
    dataset_labels <- pull(dataset, label_name) %>% unique()
    if (any(dataset_labels %in% BD_LABELS)) {
      dataset["label"] <- dataset[label_name]
    }
    return(dataset)
  }, error = function(err) {
    warning(sprintf("An error occured while transforming labels into CGI. %s", err))
    return(NA)
  })
}
