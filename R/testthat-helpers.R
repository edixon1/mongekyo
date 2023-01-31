# Eliot Dixon
# 2023-01-31

# Internal functions which are designed to help testing processes



resetTestIris <- function(){

  # Setup collection
  irisCollection <- getCollection(url = "https://data.mongodb-api.com/app/data-nmzks/endpoint/data/beta",
                                  cluster = "Cluster0",
                                  database = "mongekyoTesting",
                                  collection = "iris",
                                  apiKey = packageKey)

  # Delete all documents
  delResp <- deleteMany(irisCollection, mongFilter("{}")) |>
    httr::content()

  # Reinsert original iris set
  myIris <- dplyr::mutate(iris, Source.Id = paste0(dplyr::row_number(), "_iris_LocalId")) |>
    dplyr::select(Source.Id, dplyr::everything())

  irisDocs <- asDocument(myIris)

  insResp <- insertMany(irisCollection, documents = sprintf('"documents": %s', irisDocs)) |>
    httr::content()

  # Return n deleted, n inserted
  out <- c("Deleted" = delResp$deletedCount, "Inserted" = length(insResp$insertedIds))
  return(out)
}
