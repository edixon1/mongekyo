# Eliot Dixon
# 2023-03-24

# Returns a list of data.frames with one data.frame for each
parseResponse <- function(resp){
  if(httr::http_error(resp)){
    warning(sprintf("STATUS 400: %s", content(resp)))
    return(NULL)
  } else {
    docs <- httr::content(resp)[[1]]
    docs <- myTest

    out <- list()
    # For each document
    for(i in 1:length(docs)){
      thisDoc <- as.data.frame(docs[[i]], check.names = FALSE)
      # For each unique set of features in documents
      for(j in 1:(length(out) + 1)){
        # If this document contains a new unique set of features
        if(j > length(out)){
          out[[j]] <- thisDoc
        } else {
          # If features of this document match data.frame in out

          if(all(names(thisDoc) == names(out[[j]]))){
            out[[j]] <- rbind(out[[j]], thisDoc)
            break
          }
        }

      }
    }

    return(out)
  }
}

