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

#' @export
mongOid <- function(id){
  return(sprintf('{ "$oid": "%s" }', id))
}

#' Creates BSON formatted key-value pair
#' @param value value
#' @param field field
#'
#' @export
mongEq <- function(value, field){

  # Check if value is numeric
  if(is.numeric(value)){
    out <- sprintf('{ "%s": %s }', field, value)
  # Check if value is a clause
  } else if(jsonlite::validate(value)){
    out <- sprintf('{ "%s": %s }', field, value)
  # Value is string
  } else {
    out <- sprintf('{ "%s": "%s" }', field, value)
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




