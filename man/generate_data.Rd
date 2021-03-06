% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/generate-data.R
\name{generate_data}
\alias{generate_data}
\title{Generate data}
\usage{
generate_data(layers = 50, n_objects = 20, change_rate = 0.05)
}
\arguments{
\item{layers}{Desired number of output layers.}

\item{n_objects}{Desired number of objects per layer.}

\item{change_rate}{The rate of randomness introduced to each new layer.}
}
\value{
A list of \code{sf} polygons.
}
\description{
This function can be used to generate randomised test data.
}
