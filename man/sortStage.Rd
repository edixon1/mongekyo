% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/stages.R
\name{sortStage}
\alias{sortStage}
\title{Create a $sort aggration stage}
\usage{
sortStage(value, field, kvPairs = NULL)
}
\arguments{
\item{value}{integer, -1 or 1 indicating whether to sort field in descending (-1)
or ascending (1) order.}

\item{field}{character string, specifies which field to be sorted}

\item{kvPairs}{named list, if sorting multiple fields.  If this argument is provided,
the 'value' and 'field' parameters are ignored.}
}
\description{
Create a $sort aggration stage
}
