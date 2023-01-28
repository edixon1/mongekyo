# Eliot Dixon
# 2023-01-28



#' Send request to data API /updateOne endpoint
#'@param collection mongekyo collection object
#'@param filter character, filter to subset documents
#'
#'TODO: testing, input validation, output validation
updateOne <- function(collection, filter, update){

  reqBody <- stringr::str_replace(collection$REQBODYHEAD, "<query>",
                                  paste0(filter, ",\n",update))



  resp <- httr::POST(url = paste0(collection$URL, "/action/updateOne"),
                     httr::add_headers("Content-Type" = "application/json",
                                       "api-key" = collection$KEY),
                     body = reqBody)

  return(resp)

}




#' Send request to data API /updateOne endpoint
#'@param collection mongekyo collection object
#'@param filter character, filter to subset documents
#'
#'TODO: testing, input validation, output validation
updateMany <- function(collection, filter, update){

  reqBody <- stringr::str_replace(collection$REQBODYHEAD, "<query>",
                                  paste0(filter, ",\n",update))



  resp <- httr::POST(url = paste0(collection$URL, "/action/updateMany"),
                     httr::add_headers("Content-Type" = "application/json",
                                       "api-key" = collection$KEY),
                     body = reqBody)

  return(resp)

}
