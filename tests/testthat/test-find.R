test_that("findOne correct input", {

  key <- Sys.getenv("mongekyoTestKey")

  testCollection <- getTestCollection(key)

  irisFilter <- mongEq("1_iris_LocalId", "Source_Id") |>
    mongFilter()

  resp <- findOne(collection = testCollection, filter = irisFilter) |>
    httr::content()

  expect_equal(names(resp$document), c("_id", "Source_Id", "Sepal_Length",
                                       "Sepal_Width", "Petal_Length", "Petal_Width",
                                       "Species"))
})


test_that("find correct input", {
  key <- Sys.getenv("mongekyoTestKey")

  testCollection <- getTestCollection(key)

  irisFilter <- mongEq("1_iris_LocalId", "Source_Id") |>
    mongFilter()

  resp <- findOne(collection = testCollection, filter = irisFilter) |>
    httr::content()

  expect_equal(names(resp$document), c("_id", "Source_Id", "Sepal_Length",
                                       "Sepal_Width", "Petal_Length", "Petal_Width",
                                       "Species"))
})
