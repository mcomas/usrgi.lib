% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/vars_recover-filter.R
\name{filter.bmi}
\alias{filter.bmi}
\title{Filter bmi variable eliminating measurements below and above a constant inf and sup respectively}
\usage{
filter.bmi(bmi, obesity = NULL, inf = 14, sup = 40)
}
\arguments{
\item{obesity}{indicative variable of obesity}

\item{inf}{lower bound (default 140)}

\item{sup}{upper bound (default 220)}

\item{value}{vector with bmi measurements to be filtered}
}
\value{
the measurement vector filtered with NA in measurements outside the rank [ind, sup]
}
\description{
Filter bmi variable eliminating measurements below and above a constant inf and sup respectively
}

