#' Read sample dataset
#'
#' @param dataset_name character;
#' @name get_sample_ds
#' @return tibble
NULL

#' @rdname get_sample_ds
get_sample_csv <- function(dataset_name) {
  tryCatch(
    readr::read_csv(
      system.file(
        file.path("sample_data", dataset_name),
        package = "bipolarPreprocessing"
      )
    ),
    error = function(err) {
      warning(sprintf("Cannot read the dataset: %s.\n %s", dataset_name, err))
      return(NA)
    }
  )
}

#' @rdname get_sample_ds
#' @family get_sample_data
#' @export
get_sample_visits <- function() {
  get_sample_csv("visits1472.csv")
}

#' @rdname get_sample_ds
#' @family get_sample_data
#' @importFrom dplyr %>% mutate
#' @importFrom rlang .data
#' @export
get_sample_mobile_recordings <- function() {
  mobile_recordings <- get_sample_csv("mobilerecording.csv")

  m_recs_repaired <- mobile_recordings
  # create date as date: ----
  m_recs_repaired <- m_recs_repaired %>%
    mutate(create_date = lubridate::as_date(as.character(.data$create_date_date_id),
                                            format = "%Y%m%d"))
  # remove .x suffixes
  cols <- colnames(m_recs_repaired)
  new_names <- gsub(pattern = "\\.x$",
                    x = cols,
                    replacement = "")
  colnames(m_recs_repaired) <- new_names

  m_recs_repaired <- m_recs_repaired[, -c(1)]
  unique(m_recs_repaired)
}

#' @rdname get_sample_ds
#' @family get_sample_data
#' @export
get_sample_mobile_chunks <- function() {
  mobile_chunks <- get_sample_csv("mobilerecordingschunks.csv")

  m_chunks <- mobile_chunks
  cols <- colnames(m_chunks)
  new_names <- gsub(pattern = "\\.y$",
                    x = cols,
                    replacement = "")
  colnames(m_chunks) <- new_names
  m_chunks <- m_chunks[, -c(1)]
  m_chunks
}
