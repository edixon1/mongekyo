% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/Operators.R
\name{kvCombine}
\alias{kvCombine}
\title{Internal function to combine fields and vlaues}
\usage{
kvCombine(value, field, parseDate = TRUE, format = TRUE)
}
\arguments{
\item{value}{value to assign to a given field}

\item{field}{field to which value will be assigned}
}
\value{
string in BSON equal foramt with no surrounding brackets

TODO: Need to add logical argument to specify whether to use formatValue or not,
This should default to TRUE
}
\description{
Internal function to combine fields and vlaues
}
