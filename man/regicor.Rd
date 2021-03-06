% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/vars_create.R
\name{regicor}
\alias{regicor}
\title{Cardiovascular risk calculation according to REGICOR model}
\usage{
regicor(age, men, smoking, diabetes, coltot, colhdl, sbp, dbp, years = 10)
}
\arguments{
\item{age}{age in years}

\item{men}{indicator of men}

\item{smoking}{indicator of smoker}

\item{diabetes}{indicator of diabetes}

\item{coltot}{total cholesterol measure in mg/l}

\item{colhdl}{high density cholesterol measure in mg/l}

\item{sbp}{systolic blood pressure}

\item{dbp}{diastolic blood pressure}

\item{years}{year to where the risk is calculated (default:10)}
}
\value{
REGICOR risk
}
\description{
Cardiovascular risk calculation according to REGICOR model
}

