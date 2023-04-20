#' Simple class for time window sequencer
#'
#' @param x two element integer vector specifying limits of a window, for example \code{c(-7, 2)}
#'
#' @return sequence of integer values from \code{x[1]} to \code{x[2]}
#'
#' @export
#' @examples
#' time_window(c(-2, 2)) # returns sequence -2 -1 0 1 2
time_window <- function(x) {
  stopifnot(length(x) == 2)
  stopifnot(is.numeric(x))
  stopifnot(x[1] < x[2])

  days_seq <- x[1]:x[2]

  obj <- structure(
    days_seq,
    class = c("time_window", class(days_seq))
  )

  return(obj)
}


#' Checks for time window class
#' @param x object
#' @export
is_time_window <- function(x) UseMethod("is_time_window")

#' @export
is_time_window.default <- function(x) return(FALSE)

#' @export
is_time_window.time_window <- function(x) return(TRUE)

