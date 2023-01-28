# Eliot Dixon
# 2023-01-27


#' Creates BSON formatted AND of two or more clauses
#' TODO: Input validation
mongAnd <- function(clauses){
  combinedClauses <- paste0(clauses, collapse = ", ")
  out <- sprintf('{ "$and": [%s] }', combinedClauses)
  return(out)
}


#' Creates BSON formatted OR of two or more clauses
#' TODO: Input validation
mongOr <- function(clauses){
  combinedClauses <- paste0(clauses, collapse = ", ")
  out <- sprintf('{ "$or": [%s] }', combinedClauses)
  return(out)
}


mongOid <- function(id){
  return(sprintf('{ "$oid": "%s" }', id))
}

#' Creates BSON formatted key-value pair
#' @param field
#' @param value
#' TODO: document, input validation
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
mongGte <- function(value, field = NULL){

  if(is.null(field)){
    out <- sprintf('{ "$gte": %s }', value)
  } else {
    out <- sprintf('{ "%s": { "$gte": %s } }', field, value)
  }



  return(out)

}

#' TODO: documentaion, input validation
mongGt <- function(value, field = NULL){

  if(is.null(field)){
    out <- sprintf('{ "$gt": %s }', value)
  } else {
    out <- sprintf('{ "%s": { "$gt": %s } }', field, value)
  }

  return(out)

}


#' TODO: documentaion, input validation
mongLte <- function(value, field = NULL){

  if(is.null(field)){
    out <- sprintf('{ "$lte": %s }', value)
  } else {
    out <- sprintf('{ "%s": { "$lte": %s } }', field, value)
  }

  return(out)

}


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
mongDateTime <- function(value, tz = "EST"){

  timeStr <- as.POSIXlt(value, tz) |>
    strftime("%Y-%m-%dT%H:%M:%S%z")

  timeStr <- sub('(+[0-9]{2})([0-9]{2}$)','\\1:\\2', timeStr, fixed=FALSE)


  out <- sprintf('{ "$date": "%s" }', timeStr)

  return(out)
}

# todo: testing
mongFilter <- function(clause){
  return(sprintf('"filter": %s', clause))
}

mongUpdate <- function(clause){
  return(sprintf('"update": %s', clause))
}


mongSet <- function(clause){
  return(sprintf('{ "$set": %s }', clause))
}




