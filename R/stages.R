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
#' document, list values are character strings containing accumulator expressions.
#'
#' @export
groupStage <- function(id = NULL, accumulators = NULL){
  # Quality of life, so we can use NULL reserved word in R instead of 'null' string
  if(is.null(id)){
    id = "null"
  } else {
    id = formatAggregationValue(id)
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

