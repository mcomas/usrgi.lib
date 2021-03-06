% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/models.R
\name{fit_coxph.mi}
\alias{fit_coxph.mi}
\title{fit a cox model to multiple imputated data}
\usage{
fit_coxph.mi(frm, mi_data, mi_ind = "imp")
}
\arguments{
\item{frm}{formula passed to coxph}

\item{mi_data}{imputated dataset}

\item{mi_ind}{variable used to stratify the imputed datasetss}
}
\value{
result of the fitted the model
}
\description{
fit a cox model to multiple imputated data
}

