% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/stability.R
\name{variables_means}
\alias{variables_means}
\title{Mean coefficents on bootstrap}
\usage{
variables_means(
  DF,
  y = colnames(DF)[1],
  meaningful_variables = colnames(DF)[colnames(DF) != y],
  nbre = 30
)
}
\arguments{
\item{DF}{: dataframe}

\item{y}{: variable to explain}

\item{meaningful_variables}{: variables pre-selected by bootstrap}

\item{nbre}{: number of iteration}
}
\description{
Mean coefficents on bootstrap
}
