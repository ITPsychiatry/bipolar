% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/labels.R
\name{transform_label_hy}
\alias{transform_label_hy}
\title{Transform column into hamilton/young label}
\usage{
transform_label_hy(dataset, label_name)
}
\arguments{
\item{dataset}{data.frame}

\item{label_name}{character}
}
\value{
dataset
}
\description{
Transform column into hamilton/young label
}
\examples{
visits <- get_sample_psychiatric_data()
transform_label_hy(visits, "hamd_suma")
}
