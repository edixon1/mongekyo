# Eliot Dixon
# 2023-01-31

#' Send request to data API /insertOne endpoint
#' Inserts a single document into collection
#' @param collection collection object
#' @param filter value(s) by which to identify document to be deleted
#'TODO: input validation
#'
#'@export
deleteOne <- function(collection, filter){

  reqBody <- stringr::str_replace(collection$REQBODYHEAD, "<query>",
                                  filter)



  resp <- httr::POST(url = paste0(collection$URL, "/action/deleteOne"),
                     httr::add_headers("Content-Type" = "application/json",
                                       "api-key" = collection$KEY),
                     body = reqBody)

  return(resp)

}


#' Send request to data API /insertMany endpoint
#' Inserts a one or many documents into a collection
#' @param collection collection object
#' @param filter value(s) by which to identify documents to be deleted
#'TODO: input validation
#'
#'@export
deleteMany <- function(collection, filter){
  reqBody <- stringr::str_replace(collection$REQBODYHEAD, "<query>",
                                  filter)


  resp <- httr::POST(url = paste0(collection$URL, "/action/deleteMany"),
                     httr::add_headers("Content-Type" = "application/json",
                                       "api-key" = collection$KEY),
                     body = reqBody)

  return(resp)

}
