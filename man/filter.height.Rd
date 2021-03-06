% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/vars_recover-filter.R
\name{filter.height}
\alias{filter.height}
\title{Filter height variable eliminating measurements below and above a constant inf and sup respectively}
\usage{
filter.height(height, inf = 140, sup = 220)
}
\arguments{
\item{inf}{lower bound (default 140)}

\item{sup}{upper bound (default 220)}

\item{value}{vector with height measurements to be filtered}
}
\value{
the measurement vector filtered with NA in measurements outside the rank [ind, sup]
}
\description{
Filter height variable eliminating measurements below and above a constant inf and sup respectively
}

