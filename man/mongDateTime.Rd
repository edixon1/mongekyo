% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/Operators.R
\name{mongDateTime}
\alias{mongDateTime}
\title{Creates a date operator using in the BSON format ISO 8601, with a colon
in the offset.}
\usage{
mongDateTime(value, tz = "UTC")
}
\arguments{
\item{value}{Date or Datetime}

\item{tz}{timezone, run OlsonNames(tzdir = NULL) for a list of
timezone names

TODO: testing, input validation}
}
\description{
Creates a date operator using in the BSON format ISO 8601, with a colon
in the offset.
}
