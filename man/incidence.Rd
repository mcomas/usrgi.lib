% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/incidences.R
\name{incidence}
\alias{incidence}
\title{Desnity incidence coefficient}
\usage{
incidence(event, time, alpha = 0.05, period = 1, pers = 1000)
}
\arguments{
\item{event}{vector with the indicator variable indicating the event}

\item{time}{vector with time to event for each person}

\item{alpha}{significance for the given interval (default 0.05)}

\item{period}{years for denominator (default 1)}

\item{pers}{numerator for the density (default 1000)}
}
\value{
return an incidence object
}
\description{
Desnity incidence coefficient
}

