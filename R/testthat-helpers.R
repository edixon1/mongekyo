# Eliot Dixon
# 2023-01-31

# Internal functions which are designed to help testing processes


# Return collection object for tests
getTestCollection <- function(key){
  irisCollection <- getCollection(url = "https://data.mongodb-api.com/app/data-nmzks/endpoint/data/beta",
                                  cluster = "Cluster0",
                                  database = "mongekyoTesting",
                                  collection = "iris",
                                  apiKey = key)

  return(irisCollection)
}


# Reset state of test collection
resetTestIris <- function(key){

  # Setup collection
  irisCollection <- getCollection(url = "https://data.mongodb-api.com/app/data-nmzks/endpoint/data/beta",
                                  cluster = "Cluster0",
                                  database = "mongekyoTesting",
                                  collection = "iris",
                                  apiKey = key)

  # Delete all documents
  delResp <- deleteMany(irisCollection, mongFilter("{}")) |>
    httr::content()

  # Reinsert original iris set
  testIris <- readRDS(test_path("fixtures", "testIris.rds"))
  #readRDS(test_path("fixtures", "useful_thing1.rds"))

  irisDocs <- asDocument(testIris)

  insResp <- insertMany(irisCollection, documents = sprintf('"documents": %s', irisDocs)) |>
    httr::content()

  # Return n deleted, n inserted
  out <- c("Deleted" = delResp$deletedCount, "Inserted" = length(insResp$insertedIds))
  return(out)
}

writeTestIris <- function(){
  testIris <- dplyr::mutate(iris, Source.Id = paste0(dplyr::row_number(), "_iris_LocalId")) |>
    dplyr::select(Source.Id, dplyr::everything()) |>
    dplyr::mutate(Species = as.character(Species))

  names(testIris) <- stringr::str_replace_all(names(testIris), "\\.", "_")

  saveRDS(testIris, "./tests/testthat/fixtures/testIris.Rds")
}
