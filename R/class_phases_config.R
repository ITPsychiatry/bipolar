# simple phases_config S3 Class
# Wrapping the code into a class helps controlling object's state and structure

## class constructor
phases_config <- function(x) {
  return(structure(
    x,
    class = append("phases_config", class(x))
  ))
}

# A user-friendly helper that provides a convenient way to create objects
new_phases_config <- function(x) {
  validate_phases_config(phases_config(x))
}

# validator
validate_phases_config <- function(x) {
  stopifnot(is.data.frame(x))
  req_fields <- c("phase", "days_before", "days_after")
  stopifnot(all(req_fields %in% colnames(x)))
  return(x)
}
