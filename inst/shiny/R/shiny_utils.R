#' Renders HTML5 details element with 'title-bar' class
#'
#' @param title character; the title that should be inserted in 'summary' tag
#' @param ... extra tags/shiny elements to render
#'
#' @return details tag
render_details <- function(title, ...) {
  if (length(list(...)) == 0) {
    return(
      tags$details(
        class = "title-bar",
        tags$summary(h4(title)),
        helpText("No content to display"),
        br()
      )
    )
  }
  tags$details(
    class = "title-bar",
    tags$summary(h4(title)),
    ...
  )
}
