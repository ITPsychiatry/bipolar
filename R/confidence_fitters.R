#' Normalize values
#'
#' @description The function re-scales values of non-negative numeric vector so the maximum is 1
#' @param x numeric; all its elements must be non-negative
#'
#' @details Notice, that we cannot use min-max rescaling because this method returns a vector in 0-1
#' range. This means that the minimal element of \code{x} would be rescaled to 0.
#'
#' @return vector of the same length as \code{x}
normalize_values <- function(x) {
  stopifnot(is.numeric(x))
  stopifnot(all(x >= 0))
  # scaling factor
  alpha <- 1 / max(x)
  x * alpha
}


#' Fitting time window with steps
#'
#' @param values numeric vector; must be length 1 or the same as \code{time_window}
#' @param f function; a function returning a numeric vector of the same lenght as \code{time_window}
#' @param .time_window two-element numeric vector; the first element is the beginning of a time window,
#' the other element is the end of it.
#' @param ... arguments passed to the function \code{f}
#'
#' @return tibble; the first column is a sequence of days according to the \code{time_window} definition,
#' the second column is populated with \code{values}
#'
#' @name conf_fitters
#'
#' @details For \code{fit_with_steps} - in case \code{values} is one element vector it will be
#' repeated to fit the length of \code{time_window}
NULL


#' @rdname conf_fitters
#' @export
#'
#' @examples
#' fit_with_steps(0.5)
#' fit_with_steps(c(0.5, 0.5, 0.7, 0.7, 1), time_window(c(-2, 2)))
fit_with_steps <- function(
    values,
    .time_window = time_window(c(-DEFAULT_PHASE_DAYS_BEFORE, DEFAULT_PHASE_DAYS_AFTER)),
    ...
  ) {
  stopifnot(is.numeric(values))
  stopifnot(length(values) > 0)
  stopifnot(is_time_window(.time_window))
  stopifnot(all(values >= 0))

  stopifnot(length(values) == 1 | length(values) == length(.time_window))
  if (length(values) == 1) {
    values <- rep(values, length(.time_window))
  }
  dplyr::tibble(
    time_point = as.integer(.time_window),
    confidence = values
  )
}

#' @rdname conf_fitters
#' @export
#'
#' @examples
#' # fit time window with values from a gaussian density function
#' fit_with_function(dnorm, mean = -1, sd = 3)
fit_with_function <- function(
    f,
    .time_window = time_window(c(-DEFAULT_PHASE_DAYS_BEFORE, DEFAULT_PHASE_DAYS_AFTER)),
    ...
  ) {
  stopifnot(is.numeric(.time_window))
  stopifnot(is_time_window(.time_window))
  stopifnot(class(f) == "function")

  x <- unclass(.time_window)
  values <- f(x, ...)
  fit_with_steps(values, .time_window)
}

#' Add confidence values to the config
#'
#' @param config phases_config class object
#' @param values numeric vector; see \code{fit_with_steps} for details
#' @param func function; see \code{fit_with_function} for details
#' @param normalize logical; \code{TRUE} is the default; should the confidence values be normalized
#' so that the maximum value is 1 and other elements are rescaled proportionally; see \code{normalize_values}
#' for details
#' @param ... arguments passed to fitter functions
#'
#' @import dplyr
#'
#' @seealso \code{\link{fit_with_steps}}, \code{\link{fit_with_function}}, \code{\link{normalize_values}}
#'
#' @return tibble
#' @export
#'
#' @examples
#' set.seed(123)
#' df <- create_toy_dataset()
#' visits <- df$visits
#' auto_config <- create_time_window_config(visits, phases)
#' auto_config %>% add_confidence(values = .5)
#' auto_config %>% add_confidence(values = .5, normalize = FALSE)
#' auto_config %>% add_confidence(values = c(0.5, 0.5, 0.7, 0.7, rep(1, 6)))
#'
#' # fit with gaussian distribution:
#' gauss <- auto_config %>% add_confidence(func = dnorm)
#' gauss <- auto_config %>% add_confidence(func = dnorm, mean = -1, sd = 3)
#'
#' library(ggplot2)
#' ggplot(
#'   data = gauss,
#'   aes(x = time_point, y = confidence)
#' ) +
#'   geom_point() +
#'   geom_line() +
#'   facet_wrap(facets = "phase")
add_confidence <- function(config, values = NULL, func = NULL, normalize = TRUE, ...) {
  if (!(xor(!is.null(values), !is.null(func)))) { # allow only for one to be not null
    stop("Either 'values' or 'func' must be specified." )
  }
  stopifnot("phases_config" %in% class(config))
  config %>%
    tidyr::nest(data = -phase) %>%
    mutate(confidence = lapply(data, function(x) {
      before <- as.numeric(x["days_before"])
      after <- as.numeric(x["days_after"])
      time_wdw <- time_window(c(-before, after))
      confidence <- NULL
      if (!is.null(values)) {
        confidence <- fit_with_steps(values, time_wdw)
      }
      if (!is.null(func)) {
        confidence <- fit_with_function(func, time_wdw, ...)
      }
      if (normalize) {
        confidence <- confidence %>%
          mutate(confidence = normalize_values(confidence))
      }
      confidence
    })) %>%
    tidyr::unnest(-phase) %>%
    select(-days_before, -days_after)
}
