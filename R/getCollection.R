# Eliot Dixon
# 2023-01-27


#' Create a collection object
#' This function takes all base level information to access api and consolodates
#' it into a single object, which can then be passed to other exported functions to
#' create an API call
#' @param url Base URL to mongoDB data api, can be copied from MongoDB Data API
#' page.
#' @param cluster cluster name
#' @param database database name
#' @param collection collection name
#' @param apiKey API key obtained from mongodb.  Recommend to store this in .Renviron file.
#' @export
getCollection <- function(url, cluster, database, collection, apiKey){
  out <- list(
    URL = url,
    KEY = apiKey,
    REQBODYHEAD = sprintf(
      '{ "dataSource": "%s",
         "database": "%s",
         "collection": "%s",
         <query>

       }',
      cluster, database, collection)
  )

  return(out)
}
