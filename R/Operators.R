# Eliot Dixon
# 2023-01-27


#' Creates BSON formatted AND of two or more clauses
#' TODO: Input validation
#' @export
mongAnd <- function(clauses){
  combinedClauses <- paste0(clauses, collapse = ", ")
  out <- sprintf('{ "$and": [%s] }', combinedClauses)
  return(out)
}


#' Creates BSON formatted OR of two or more clauses
#' TODO: Input validation
#' @export
mongOr <- function(clauses){
  combinedClauses <- paste0(clauses, collapse = ", ")
  out <- sprintf('{ "$or": [%s] }', combinedClauses)
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
mongEq <- function(value, field, kvPairs = NULL){


  if(!is.null(kvPairs)){
    if(is.null(names(kvPairs))){
      stop("kvPairs must be a named list")
    }
    # Convert kv pairs into json format
    out <- Map(kvCombine, kvPairs, names(kvPairs)) %>%
      unlist() %>%
      paste(collapse = ", ")

    out <- sprintf("{ %s }", out)
    return(out)

  } else {
    out <- sprintf("{ %s }", kvCombine(value, field))
  }

  return(out)
}

#' Internal function to combine fields and vlaues
#' @param value value to assign to a given field
#' @param field field to which value will be assigned
#' @returns string in BSON equal foramt with no surrounding brackets
#'
kvCombine <- function(value, field){

  formattedValue <- formatValue(value)

  out <- sprintf('"%s": %s', field, formattedValue)

  return(out)
}


#' Internal function to format vlaues according to their type
#' @param value value to assign to a given field
#' @returns value formatted in BSON according to its data type
#' @importFrom lubridate is.timepoint
#'
formatValue <- function(value){
  # Check if value is numeric
  if(is.numeric(value)){
    out <- sprintf('%s', value)
    # Check if value is a date
  } else if(is.timepoint(value)) {
    out <- sprintf('%s', mongDateTime(value))
    # Check if value is a clause
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
mongNot <- function(clause){
  out <- sprintf('{ "$not": %s }', clause)
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
mongFilter <- function(clause){
  return(sprintf('"filter": %s', clause))
}

#' @export
mongUpdate <- function(clause){
  return(sprintf('"update": %s', clause))
}

#' @export
mongSet <- function(clause){
  return(sprintf('{ "$set": %s }', clause))
}

#' @export
mongDoc <- function(clause){
  return(sprintf('"document": %s', clause))
}

#' @export
mongDocs <- function(clause){
  return(sprintf('"documents": %s', clause))
}





