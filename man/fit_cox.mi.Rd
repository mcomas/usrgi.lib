% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/models.R
\name{fit_cox.mi}
\alias{fit_cox.mi}
\title{ifelse when used with Dates}
\usage{
fit_cox.mi(mi_data, str_coxph = "coxph(Surv(time = time, event = event)~1",
  analysis = "intention.to.treat", str_survfit = gsub("user",
  "strata(user)", str_coxph))
}
\arguments{
\item{mi_data}{multiple imputation data with each imputation in stacked by rows}

\item{str_coxph}{string containing the cox models to be evaluated (default="coxph(Surv(time = time, event = event)~1")}

\item{analysis}{type of analysis to perform 'intention.to.treat', 'per.protocol' and 'as.treated' (default: 'intention.to.treat')}

\item{str_survfit}{string containing the survival curve to be evaluated}
}
\value{
result of fitting the model
}
\description{
ifelse when used with Dates
}

