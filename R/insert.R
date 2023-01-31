# Eliot Dixon
# 2023-01-28


#' Send request to data API /insertOne endpoint
#' Inserts a single document into collection
#' @param collection collection object
#' @param document document in BSON format
#'TODO: input validation
#'
#'@export
insertOne <- function(collection, document){

  reqBody <- stringr::str_replace(collection$REQBODYHEAD, "<query>",
                                  document)



  resp <- httr::POST(url = paste0(collection$URL, "/action/insertOne"),
                     httr::add_headers("Content-Type" = "application/json",
                                       "api-key" = collection$KEY),
                     body = reqBody)

  return(resp)

}


#' Send request to data API /insertMany endpoint
#' Inserts a one or many documents into a collection
#' @param collection collection object
#' @param document array of documents in BSON format
#'TODO: input validation
#'
#'@export
insertMany <- function(collection, documents){
  reqBody <- stringr::str_replace(collection$REQBODYHEAD, "<query>",
                                  documents)

  print(reqBody)



  resp <- httr::POST(url = paste0(collection$URL, "/action/insertMany"),
                     httr::add_headers("Content-Type" = "application/json",
                                       "api-key" = collection$KEY),
                     body = reqBody)

  return(resp)

}





asDocument <- function(docList){

  jsonlite::toJSON(docList, pretty = TRUE, auto_unbox = TRUE)

}



