# Eliot Dixon
# 2023-06-16


#' Send request to data API /find endpoint
#'@param collection mongekyo collection object
#'@param pipeline character, aggregation pipeline to subset/summarize documents
#'TODO: testing, input validation, output validation
#'@export
aggregate <- function(collection, filter){

  reqBody <- stringr::str_replace(collection$REQBODYHEAD, "<query>",
                                  filter)


  resp <- httr::POST(url = paste0(collection$URL, "/action/aggregate"),
                     httr::add_headers("Content-Type" = "application/json",
                                       "api-key" = collection$KEY),
                     body = reqBody)

  return(resp)

}
