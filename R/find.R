# Eliot Dixon
# 2023-01-27


#' Send request to data API /findOne endpoint
#'@param collection mongekyo collection object
#'@param filter character, filter to subset documents
#'
findOne <- function(collection, filter){

  reqBody <- stringr::str_replace(collection$REQBODYHEAD, "<query>",
                                  filter)


  resp <- httr::POST(url = paste0(collection$URL, "/action/findOne"),
                     httr::add_headers("Content-Type" = "application/json",
                                 "api-key" = collection$KEY),
                     body = reqBody)

  return(resp)

}

#' Send request to data API /find endpoint
#'@param collection mongekyo collection object
#'@param filter character, filter to subset documents
#'
find <- function(collection, filter){

  reqBody <- stringr::str_replace(collection$REQBODYHEAD, "<query>",
                                  filter)


  resp <- httr::POST(url = paste0(collection$URL, "/action/find"),
                     httr::add_headers("Content-Type" = "application/json",
                                       "api-key" = collection$KEY),
                     body = reqBody)

  return(resp)

}





# reqBody <- '{ "dataSource": "Cluster0",
#               "database": "sample_mflix",
#               "collection": "comments",
#               "filter": {"name": "Kelsey Smith"}
#               }'
