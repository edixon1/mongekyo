# Eliot Dixon
# 2023-01-27


#' Creates BSON formatted AND of two or more expressions
#'
#' TODO: Input validation
#' @export
mongAnd <- function(expressions){
  combinedexpressions <- paste0(expressions, collapse = ", ")
  out <- sprintf('{ "$and": [%s] }', combinedexpressions)
  return(out)
}


#' Creates BSON formatted OR of two or more expressions
#' TODO: Input validation
#' @export
mongOr <- function(expressions){
  combinedexpressions <- paste0(expressions, collapse = ", ")
  out <- sprintf('{ "$or": [%s] }', combinedexpressions)
  return(out)
}


#' Creates a BSON statement with the $in operator and a given BSON array
#' @param array a character, a string formatted as a BSON array, see the mongArray() function
#' @param field a character, optional argument to bypass a call to mongEq(), you can pass the field
#'  whose value should be in the provided array.
#' @export
#'
mongIn <- function(array, field = NULL){
  if(is.null(field)){
    out <- sprintf('{ "$in": %s }', array)
  } else {
    out <- sprintf('{ "%s": { "$in": %s } }', field, array)
  }
  return(out)
}

#' Formats a vector as a BSON array
#' @param values a vector which will be formatted based on its type, and formatted
#'  as a BSON array
#' @export
mongArray <- function(values){
  formattedValues <- sapply(values, formatValue) %>%
    paste(collapse = ", ")

  out <- sprintf("[%s]", formattedValues)

  return(out)
}



#' @export
mongOid <- function(id){
  return(sprintf('{ "$oid": "%s" }', id))
}

#' Creates BSON formatted key-value pair
#' @param value value
#' @param field field
#' @param kvPairs named list of key value pairs to be used if working with multiple fields and values.  value and field parameter will be ignored if this parameter is used.
#'
#' @export
mongEq <- function(value, field, kvPairs = NULL, parseDates = TRUE){


  if(!is.null(kvPairs)){
    if(is.null(names(kvPairs))){
      stop("kvPairs must be a named list")
    }
    # Convert kv pairs into json format
    out <- Map(kvCombine, kvPairs, names(kvPairs), parseDates) %>%
      unlist() %>%
      paste(collapse = ", ")

    out <- sprintf("{ %s }", out)
    return(out)

  } else {
    out <- sprintf("{ %s }", kvCombine(value, field, parseDates))
  }

  return(out)
}

#' Internal function to combine fields and vlaues
#' @param value value to assign to a given field
#' @param field field to which value will be assigned
#' @returns string in BSON equal foramt with no surrounding brackets
#'
#'TODO: Need to add logical argument to specify whether to use formatValue or not,
#'   This should default to TRUE
kvCombine <- function(value, field, parseDate = TRUE, format = TRUE){

  if(format){
    value <- formatValue(value, parseDate)
  }

  out <- sprintf('"%s": %s', field, value)

  return(out)
}


#' Internal function to format vlaues according to their type
#' @param value value to assign to a given field
#' @returns value formatted in BSON according to its data type
#' @importFrom lubridate is.timepoint
#'
formatValue <- function(value, parseDate = TRUE){
  # Check if value is numeric
  if(is.numeric(value)){
    out <- sprintf('%s', value)
    # Check if value is a date
  } else if(is.timepoint(value)) {
    if(parseDate){
      out <- sprintf('%s', mongDateTime(value))
    } else{
      out <- sprintf('"%s"', value)
    }

    # Check if value is a factor
  } else if(is.factor(value)){
    out <- sprintf('"%s"', value)
    # Check if value is a expression
  } else if(jsonlite::validate(value)){
    out <- sprintf('%s', value)
    # Value is string
  } else {
    out <- sprintf('"%s"', value)
  }

  return(out)
}

#' TODO: documentaion, input validation
#' @export
mongGte <- function(value, field = NULL){

  if(is.null(field)){
    out <- sprintf('{ "$gte": %s }', value)
  } else {
    out <- sprintf('{ "%s": { "$gte": %s } }', field, value)
  }



  return(out)

}

#' TODO: documentaion, input validation
#' @export
mongGt <- function(value, field = NULL){

  if(is.null(field)){
    out <- sprintf('{ "$gt": %s }', value)
  } else {
    out <- sprintf('{ "%s": { "$gt": %s } }', field, value)
  }

  return(out)

}

#' TODO: documentaion, input validation
#' @export
mongLte <- function(value, field = NULL){

  if(is.null(field)){
    out <- sprintf('{ "$lte": %s }', value)
  } else {
    out <- sprintf('{ "%s": { "$lte": %s } }', field, value)
  }

  return(out)

}

#' @export
mongLt <- function(value, field = NULL){

  if(is.null(field)){
    out <- sprintf('{ "$lt": %s }', value)
  } else {
    out <- sprintf('{ "%s": { "$lt": %s } }', field, value)
  }

  return(out)

}

#' @export
mongRegex <- function(value){
  out <- sprintf('{ "$regex": "%s" }', value)
  return(out)
}

#' @export
mongNot <- function(expression){
  out <- sprintf('{ "$not": %s }', expression)
  return(out)
}

# todo: lots of testing, input validation
#' Creates a date operator using in the BSON format ISO 8601, with a colon
#' in the offset.
#'@param value Date or Datetime
#'@param tz timezone, run OlsonNames(tzdir = NULL) for a list of
#' timezone names
#'
#' TODO: testing, input validation
#' @export
mongDateTime <- function(value, tz = "UTC"){

  timeStr <- as.POSIXlt(value, tz) |>
    strftime("%Y-%m-%dT%H:%M:%S%z")

  timeStr <- sub('(+[0-9]{2})([0-9]{2}$)','\\1:\\2', timeStr, fixed=FALSE)


  out <- sprintf('{ "$date": "%s" }', timeStr)

  return(out)
}


# TEMPORARY
mongDateFormat <- function(value, tz = "UTC"){
  timeStr <- as.POSIXlt(value, tz) |>
    strftime("%Y-%m-%dT%H:%M:%S%z")

  timeStr <- sub('(+[0-9]{2})([0-9]{2}$)','\\1:\\2', timeStr, fixed=FALSE)


  out <- sprintf('\"%s\"', timeStr)
  return(out)
}

# todo: testing
#' @export
mongFilter <- function(expression){
  return(sprintf('"filter": %s', expression))
}

#' @export
mongUpdate <- function(expression){
  return(sprintf('"update": %s', expression))
}

#' @export
mongSet <- function(expression){
  return(sprintf('{ "$set": %s }', expression))
}

#' @export
mongDoc <- function(expression){
  return(sprintf('"document": %s', expression))
}

#' @export
mongDocs <- function(expression){
  return(sprintf('"documents": %s', expression))
}





