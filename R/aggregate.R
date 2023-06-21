# Eliot Dixon
# 2023-06-16


#' Send request to data API /find endpoint
#'@param collection mongekyo collection object
#'@param pipeline character, aggregation pipeline to subset/summarize documents
#'
#'@export
aggregate <- function(collection, pipeline){

  reqBody <- stringr::str_replace(collection$REQBODYHEAD, "<query>",
                                  pipeline)


  resp <- httr::POST(url = paste0(collection$URL, "/action/aggregate"),
                     httr::add_headers("Content-Type" = "application/json",
                                       "api-key" = collection$KEY),
                     body = reqBody)

  return(resp)

}


#' Create an aggregation pipeline
#'
#' @param stages list of stages or character string containing stages.
#' If list, each element should contain a stage encased in curly braces.  If character
#' string, it should be a correctly formatted BSON array of stages 
#' (use mongArray to convert an expression to BSON array).
#' @examples
#' # Create a pipeline which will pull movie documents for comments made by 'Jojen Reed'
#' pipeline <- list(match = mongEq("Jojen Reed", "name") %>% matchStage(),
#'                  lookup = lookupStage(from = "movies", localField = "movie_id",
#'                                       foreignField = "_id", as = "movieDocs")) %>% 
#'   pipeline()
#' @export
pipeline <- function(stages){
  # TODO: Write code examples once more stages are written

  if(class(stages) == "list"){
    stages <- mongArray(stages)
  }

  out <- sprintf('"pipeline": %s', stages)

  return(out)
}
