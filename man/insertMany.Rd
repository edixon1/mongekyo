% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/insert.R
\name{insertMany}
\alias{insertMany}
\title{Send request to data API /insertMany endpoint
Inserts a one or many documents into a collection}
\usage{
insertMany(collection, documents)
}
\arguments{
\item{collection}{collection object}

\item{document}{array of documents in BSON format
TODO: input validation, add check if document already contains _id field}
}
\description{
Send request to data API /insertMany endpoint
Inserts a one or many documents into a collection
}
