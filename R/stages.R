# Eliot Dixon
# 2023-06-19

#' Create a $match aggregation stage
#' 
#' @param expression a BSON expression which provides query conditions
#' (identical to an expression passed to the filter endpoint) by which to subset
#' the data.
#' 
#' @export
matchStage <- function(expression){
  out <- sprintf(
  '{"$match": %s}',
  expression)

  return(out)
}

#' Create a $sort aggration stage
#'
#' @param value integer, -1 or 1 indicating whether to sort field in descending (-1)
#' or ascending (1) order.
#' @param field character string, specifies which field to be sorted
#' @param kvPairs named list, if sorting multiple fields.  If this argument is provided,
#' the 'value' and 'field' parameters are ignored.
#' 
#' @export
sortStage <- function(value, field, kvPairs = NULL){

  if(is.null(kvPairs)){
    exp <- sprintf('"%s": %s', field, value)
  } else {
    exp <- Map(kvCombine, kvPairs, names(kvPairs), FALSE) %>%
      unlist() %>%
      paste(collapse = ", ")
  }

  out <- sprintf('{"$sort": {%s}}', exp)
  
  return(out)
}



#' Create a $group aggration stage
#' 
#' @param id character string, contains a field or expression to be used as the group key.
#' @param accumulators named list, list names will be the name of fields created within the output
#'  document, list values are character strings containing accumulator expressions.
#' @param kvPairs named list, replaces the id parameter if provided.  list names
#'  will be the name of _id fields created within the output, list values are the fields
#'  which should be used as _ids
#' 
#' @examples
#'  
#'  # Basic call:
#'  groupStage(id = "PID", accumulators = list(totalHours = mongSum("THOURS")))
#'  
#'  # kvPairs to use multiple _id fields:
#'  groupStage(kvPairs = list("project" = "PID", "employee" = "TSTAFF"), 
#'  accumulators = list(totalHours = mongSum("THOURS")))
#' 
#'
#' @export
groupStage <- function(id = NULL, accumulators = NULL, kvPairs = NULL){

  if(!is.null(kvPairs)){
    
    id <- lapply(kvPairs, formatAggregationValue)
    
    
    id <- Map(kvCombine, id, names(id), FALSE, FALSE) %>%
      unlist() %>%
      paste(collapse = ", ")
    
    id <- sprintf("{%s}", id)
    
  } else {
    # Quality of life, so we can use NULL reserved word in R instead of 'null' string
    if(is.null(id)){
      id = "null"
    }  else {
      id = formatAggregationValue(id)
    }
    
  }
  
  # If no accumulators are provided, return stage with just _id
  if(is.null(accumulators)){
    return(sprintf('{"$group": {"_id": %s}}', id))
  } 
  
  # Create expression from list of accumulators
  accExp <- Map(kvCombine, accumulators, names(accumulators), FALSE) %>%
    unlist() %>%
    paste(collapse = ", ")
  
  out <- sprintf('{"$group": {"_id": %s, %s}}', id, accExp)
  
  return(out)

}


#' Create a $lookup aggregation stage
#' 
#' @param from Specifies the collection in the same database to perform the join with
#' @param localField Specifies the field from the documents input to the $lookup stage
#' @param foreignField Specifies the field from the documents in the from collection
#' @param as Specifies the name of the new array field to add to the input documents
#' 
#' TODO implement optional parameters, see issue #5
#'
#' @examples
#' 
#' # Create a lookup stage to get movie documents from comment document
#' lookupStage(from = "movies", localField = "movie_id",
#'             foreignField = "_id", as = "movieDocs")
#'   
#' # Use lookup stage in pipeline 
#' pipeline <- list(mongEq("Jojen Reed", "name") %>% matchStage(),
#'                  lookup = lookupStage(from = "movies", localField = "movie_id",
#'                                      foreignField = "_id", as = "movieDocs")) %>% 
#'   pipeline()
#' @export
lookupStage <- function(from, localField, foreignField, as){

  out <- sprintf('{"$lookup": {"from": "%s", "localField": "%s", "foreignField": "%s", "as": "%s"}}', 
                 from, localField, foreignField, as)
  
  return(out)
  
}

