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
mongEq <- function(field, value){

  if(is.numeric(value)){
    out <- sprintf('{ "%s": %s }', field, value)
  } else {
    out <- sprintf('{ "%s": "%s" }', field, value)
  }

  return(out)
}

#' THIS DOESN'T WORK, figure it out
#' TODO: documentaion, input validation
mongGte <- function(field, value){

  out <- sprintf('{ "$gte": %s }', value)

  return(out)

}

# mongGte
#
# mongLte
#
# mongLt
