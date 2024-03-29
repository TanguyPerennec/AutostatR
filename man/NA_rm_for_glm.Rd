% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/dataprep.R
\name{NA_rm_for_glm}
\alias{NA_rm_for_glm}
\title{Removing of all NAs and change the dataframe if too many rows are deleted}
\usage{
NA_rm_for_glm(
  DF,
  y = colnames(DF)[1],
  EPV = 0,
  min_explicatives = 0.1 * (ncol(DF)),
  floor_pval = 1,
  verbose = TRUE,
  method_NA = c("lessNA", "significance"),
  keep = FALSE,
  kNN = NULL,
  rf = NULL,
  median = NULL,
  mean = NULL,
  NA_as_level = NULL
)
}
\arguments{
\item{DF}{: the dataframe}

\item{y}{: the variable to explain}

\item{EPV}{: the event-per-variable threeshold.}

\item{keep}{: the variable(s) to keep whathever are its significance and number of NA}

\item{imputed_variables}{: variables to be imputed if imputed method is selected in 'method_NA'.}
}
\description{
Removing of all NAs and change the dataframe if too many rows are deleted
}
