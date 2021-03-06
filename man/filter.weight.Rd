% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/vars_recover-filter.R
\name{filter.weight}
\alias{filter.weight}
\title{Filter weight variable eliminating measurements below and above a constant inf and sup respectively}
\usage{
filter.weight(weight, inf = 40, sup = Inf)
}
\arguments{
\item{inf}{lower bound (default 140)}

\item{sup}{upper bound (default 220)}

\item{value}{vector with weight measurements to be filtered}
}
\value{
the measurement vector filtered with NA in measurements outside the rank [ind, sup]
}
\description{
Filter weight variable eliminating measurements below and above a constant inf and sup respectively
}

