#' Read sample dataset
#'
#' @param dataset_name character;
#' @return tibble
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
#' @export
get_all_visits <- function() {
  file_path <- file.path(
    Sys.getenv("DATA_REPO"),
    "wizyty.csv"
  )
  data.table::fread(file_path, header = TRUE)
}



#' @rdname get_sample_ds
#' @family get_sample_data
#' @export
get_sample_aggregated_data <- function() {
  file_path <- file.path(
    Sys.getenv("DATA_REPO"),
    "data_BIPOLAR.csv"
  )
  data.table::fread(file_path, header = TRUE)
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


#' Creates a small dataset for illustrative purposes
#' @import dplyr
#' @export
create_toy_dataset <- function() {
  phases <- BD_LABELS

  dates <- seq.Date(
    from = as.Date("2022-01-01"),
    to = as.Date("2022-12-31"),
    by = 1
  )
  sample_days <- sample(dates, size = 30)

  # visits dataset
  visits <- suppressWarnings(
    cbind(phases, visit_date = as.character(sample_days)) %>%
      as_tibble() %>%
      mutate(visit_date = as.Date(visit_date))
  )

  patient_ids <- sample(1:500, size = nrow(visits))
  visits <- visits %>%  mutate(patient_id = patient_ids)

  visits$visit_id <- sample(1:1000, size = nrow(visits))

  # recordings dataset
  recordings <- expand.grid(
    date = dates,
    patient_id = patient_ids
  ) %>% dplyr::as_tibble()

  recordings <- recordings %>%
    mutate(date = as.Date(date),
           recording_id = 1:n(),
           x1 = rnorm(n = nrow(.)),
           x2 = rnorm(n = nrow(.), sd = 3),
           x3 = rexp(n = nrow(.)))

  list(visits = visits, recordings = recordings)
}
