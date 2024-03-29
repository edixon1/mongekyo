% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/Operators.R
\name{mongIn}
\alias{mongIn}
\title{Creates a BSON statement with the $in operator and a given BSON array}
\usage{
mongIn(array, field = NULL)
}
\arguments{
\item{array}{a character, a string formatted as a BSON array, see the mongArray() function}

\item{field}{a character, optional argument to bypass a call to mongEq(), you can pass the field
whose value should be in the provided array.}
}
\description{
Creates a BSON statement with the $in operator and a given BSON array
}
