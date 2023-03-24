# Eliot Dixon
# 2023-01-28


#' Send request to data API /insertOne endpoint
#' Inserts a single document into collection
#' @param collection collection object
#' @param document document in BSON format
#'TODO: input validation, add check if document already contains _id field
#'
#'@export
insertOne <- function(collection, document){

  if(!grepl("document", document)){
    document <- mongDoc(document)
  }

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
#'TODO: input validation, add check if document already contains _id field
#'
#'@export
insertMany <- function(collection, documents){

  if(!grepl("documents", documents)){
    document <- mongDocs(document)
  }


  reqBody <- stringr::str_replace(collection$REQBODYHEAD, "<query>",
                                  documents)

  resp <- httr::POST(url = paste0(collection$URL, "/action/insertMany"),
                     httr::add_headers("Content-Type" = "application/json",
                                       "api-key" = collection$KEY),
                     body = reqBody)

  return(resp)

}




#'Converts data.frame to document format
#'@param data data.frame to be converted to a array of documents in BSON format
#'
#'@export
asDocument <- function(data){

  jsonlite::toJSON(data, pretty = TRUE, auto_unbox = TRUE)

}



