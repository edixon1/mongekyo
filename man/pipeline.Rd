% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/aggregate.R
\name{pipeline}
\alias{pipeline}
\title{Create an aggregation pipeline}
\usage{
pipeline(stages)
}
\arguments{
\item{stages}{list of stages or character string containing stages.
If list, each element should contain a stage encased in curly braces.  If character
string, it should be a correctly formatted BSON array of stages
(use mongArray to convert an expression to BSON array).}
}
\description{
Create an aggregation pipeline
}
\examples{
# Create a pipeline which will pull movie documents for comments made by 'Jojen Reed'
pipeline <- list(match = mongEq("Jojen Reed", "name") \%>\% matchStage(),
                 lookup = lookupStage(from = "movies", localField = "movie_id",
                                      foreignField = "_id", as = "movieDocs")) \%>\% 
  pipeline()

TODO: Write code examples once more stages are written
}
