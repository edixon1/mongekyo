# Eliot Dixon
# 2023-06-19

#' Create expression with $sum accumulator
#'
#' @param expression character string containing an expression or field which 
#' evaluates as numeric.
#' @examples
#' # sum the HOURS field
#' mongSum("HOURS")
#' # create count
#' mongSum(1)
#' 
#' @export
mongSum <- function(expression){
  # Parse input expression, encapsulate with "$<expression>" if paramater refers to a field
  formattedExp <- formatAggregationValue(expression)
  return(sprintf('{"$sum": %s}', formattedExp))
}

#' Internal helper function to probably format arguments to accumulator functions
#' 
#' @param value incomming argument to accumulator function
#' 
formatAggregationValue <- function(value){
  if(is.numeric(value)){
    out <- sprintf('%s', value)
  } else if(jsonlite::validate(value)){
    out <- sprintf('%s', value)
  } else {
    out <- sprintf('"$%s"', value)
  }
  return(out)
}

