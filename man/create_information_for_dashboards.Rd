% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/output_structure.R
\name{create_information_for_dashboards}
\alias{create_information_for_dashboards}
\title{Create a structure required to run shiny dashboards.}
\usage{
create_information_for_dashboards(
  dt_output,
  model_name,
  hyperparams,
  validation_setting,
  .metric = "F1"
)
}
\arguments{
\item{dt_output}{dataframe with outcomes of prediction
it should contain specific columns (TBD)}

\item{model_name}{<str> the name of the model}

\item{hyperparams}{<list of strings>}

\item{validation_setting}{<str> the name of the validation scenario}

\item{.metric}{<str / list of strings> validation performance metrics,
should align with the names of results from caret::confusionMatrix}
}
\value{
List with multiple elements required for specific dashboards.
}
\description{
Create a structure required to run shiny dashboards.
}
\examples{
\dontrun{
cnf <- create_bipolar_config(
  inputs = file.path(Sys.getenv("DATA_REPO"), "inputs"),
  outputs = file.path(Sys.getenv("DATA_REPO"), "outputs")
)
dt_output <- readRDS(file.path(cnf$inputs, "validation_df2.rds"))
output <- create_information_for_dashboards(
  dt_output = dt_output,
  model_name = 'ssfcm_2patients',
  hyperparams = list(
    alfa = 1.5,
    beta = 5.5,
    gamma = 10,
    single = FALSE,
    type = 'boost'
  ),
  validation_setting = "Foo_validation",
  .metric = c("F1", "Precision", "Recall")
)
}

}
