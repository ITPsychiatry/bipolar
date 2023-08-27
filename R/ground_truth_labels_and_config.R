# Constants ----

DEFAULT_PHASE_DAYS_BEFORE <- 7
DEFAULT_PHASE_DAYS_AFTER <- 2

# Funs -----

#' Extracts uniqe phases from a dataframe
#'
#' @param df tibble
#' @param phases_column tidy-select specification of the column with phases
#'
#' @export
get_unique_phases <- function(df, phases_column) {
  df %>%
    dplyr::pull({{phases_column}}) %>%
    unique() %>%
    sort()
}


#' Gets the default time window borders
#' @export
get_default_intervals_for_phases <- function() {
  interval <- c(before = DEFAULT_PHASE_DAYS_BEFORE,
                after = DEFAULT_PHASE_DAYS_AFTER)
  return(as.integer(interval))
}

#' Automatically creates config object based on input data
#'
#' @inheritParams get_unique_phases
#'
#' @export
auto_create_phases_config <- function(df, phases_column) {
  phases <- get_unique_phases(df, {{phases_column}})
  default_interval <- get_default_intervals_for_phases() %>% as.integer()
  default_df <- tibble::tibble(
    default_interval[1],
    default_interval[2]
  )
  cnf <- merge(
    phases,
    default_df
  ) %>% setNames(
    c("phase", "days_before", "days_after")
  )

  new_phases_config(cnf)
}


#' Gets time window borders for selected phase
#'
#' @param config time window configuration (\code{phases_config} class object)
#' @param .phase character; selected phase
#'
#' @export
get_phase_interval_from_config <- function(config, .phase) {
  config %>%
    filter(phase == .phase) %>%
    select(days_before, days_after) %>%
    as.integer()
}



#' Extrapolates ground truth labels according to phases configuration
#'
#' @description Phases with their corresponding before and after limits should be provided as
#' \code{config} argument. Phases (labels) will be extrapolated to cover the whole period. Technically,
#' rows will be multiplicated and the original label will be extrapolated on the new rows.
#'
#' New rows will be added for every combination of phase and visit date.
#'
#' @param d data.frame (tibble) with original visit dates
#' @param config period configuration class object;
#'
#' @param phases_col which column in \code{visits} stores phases; tidy-selection is available
#' @param visit_date_col which column in \code{visits} stores visit date; tidy-selection is available
#' @param patient_id_col which column in \code{visits} stores the ID of a patient; tidy-selection is available
#'
#' @importFrom dplyr %>% group_by mutate
#' @importFrom tidyr nest unnest
#'
#' @return data.frame (tibble) with extra rows covering the interval between
#' before and after dates from \code{config}
#' @export
#'
#' @examples
#' set.seed(123)
#' df <- create_toy_dataset()
#' visits <- df$visits
#' auto_config <- auto_create_phases_config(visits, phases)
#'
#' #insert new rows for each record
#' expand_ground_truth_period(df, auto_config, phases, visit_date)
expand_ground_truth_period <- function(d, config, phases_col, visit_date_col, patient_id_col) {
  if (!"phases_config" %in% class(config)) {
    warning("The config file is wrong.")
    return(NA)
  }
  if (!"data.frame" %in% class(d)) {
    warning("The 'd' argument has to be a data.frame object.")
    return(NA)
  }

  df_nested <- d %>%
    group_by({{patient_id_col}}, {{phases_col}}, {{visit_date_col}} ) %>%
    nest()

  df_nested_extrapolated <- df_nested %>%
    mutate(extended_rows = lapply(data, function(i) {
      interval <- get_phase_interval_from_config(config, {{phases_col}})
      before <- interval[1]
      after <- interval[2]
      date <- seq.Date(
        from = visit_date - before,
        to = visit_date + after,
        by = 1
      )
      time_point <- time_window(c(-before, after))
      dplyr::tibble(date = date, time_point = time_point)
    }))

  df_nested_extrapolated %>%
    unnest(cols = c(data)) %>%
    unnest(cols = c(extended_rows)) %>%
    rename(phase = {{phases_col}}) %>%
    mutate(time_point = as.integer(time_point))
}



#' Solves an issue of overlapping time windows of phases
#'
#' @description When the visits of a patient are very close to each other, i.e. their time windows
#' intersect we need to decide which phase is valid. The most natural choice would be the one with
#' higher confidence and this is what this function offers.
#' Moreover, taking the point with maximum confidence may be not enough, for example in case when
#' the confidence is constantly equal 1. In this case the following criteria are taken into account:
#' \enumerate{
#'  \item{distance to the original visit date - minimize}
#'  \item{seniority of original visit dates - take the latter}
#' }
#'
#' @param extended_visits tibble; visits expanded to relevant time windows
#' @param phases_with_confidence tibble; time windows configurations enhanced with confidence value
#'
#' @return tibble; overlapping time windows are solved
#' @export
#'
#' @examples
#' # prepare sample data
#' patient_id <- 888
#' visit_date <- as.Date(c("2023-02-01", "2023-02-05"))
#'
#' test_visits <- dplyr::tibble(
#'   patient_id = patient_id,
#'   visit_date = visit_date,
#'   visit_id = 1:2,
#'   phase = c("depression", "mania")
#' )
#'
#' auto_conf <- auto_create_phases_config(test_visits, phase)
#' extended_test_visits <- expand_ground_truth_period(
#'   d = test_visits,
#'   config = auto_conf,
#'   phases_col = phase,
#'   visit_date_col = visit_date,
#'   patient_id_col = patient_id
#' )
#'
#' # add confidence; toggle commented lines to try different scenarios
#' problematic <- add_confidence(
#'   auto_conf,
#'   # values = 1
#'   values = c(rep(c(1, .7), c(8, 2)))
#'   # func = dnorm
#' )
#'
#' library(ggplot2)
#' ggplot(
#'   data = extended_test_visits,
#'   aes(y = problematic$confidence, x = date, color = as.factor(visit_id))
#' ) +
#'   geom_point(position = position_jitter(height = 0.015, width = 0)) +
#'   geom_line(position = position_jitter(height = 0.015)) +
#'   ylim(c(-0.1, 1.1)) +
#'   geom_vline(aes(xintercept = visit_date, color = as.factor(visit_id)))
#'
#' # solution
#' resolved <- transform_overlapping_phases(extended_test_visits, problematic)
#' ggplot(
#'   data = resolved,
#'   aes(y = confidence, x = date, color = as.factor(visit_id))
#' ) +
#'   geom_point(position = position_jitter(height = 0.015, width = 0)) +
#'   geom_line(position = position_jitter(height = 0.015)) +
#'   ylim(c(-0.1, 1.1)) +
#'   geom_vline(aes(xintercept = visit_date, color = as.factor(visit_id)))
transform_overlapping_phases <- function(extended_visits,
                                         phases_with_confidence) {
  stopifnot(is.data.frame(extended_visits))
  stopifnot(is.data.frame(phases_with_confidence))
  stopifnot(all(c("phase", "time_point") %in% colnames(extended_visits)))
  stopifnot(all(c("phase", "time_point") %in% colnames(phases_with_confidence)))

  extended_visits_with_conf <- inner_join(
    extended_visits,
    phases_with_confidence,
    by = c("phase", "time_point")
  )

  max_confidence <- extended_visits_with_conf %>%
    group_by(patient_id, date) %>%
    summarise(confidence = max(confidence))

  unique_phases <- inner_join(
    extended_visits_with_conf,
    max_confidence,
    by = c("patient_id", "date", "confidence")
  ) %>%
    group_by(patient_id, date) %>%
    arrange(date, desc(max(confidence)), abs(time_point), desc(visit_date)) %>%
    dplyr::slice(1)
  unique_phases
}
