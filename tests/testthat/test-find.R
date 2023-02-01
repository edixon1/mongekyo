test_that("findOne check names", {

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

test_that("findOne statusCode", {
  key <- Sys.getenv("mongekyoTestKey")
  testCollection <- getTestCollection(key)

  resp <- findOne(collection = testCollection, filter = mongFilter("{}"))

  expect_equal(resp$status_code, 200)

})



test_that("find check names", {
  key <- Sys.getenv("mongekyoTestKey")

  testCollection <- getTestCollection(key)

  irisFilter <- mongEq("setosa", "Species") |>
    mongFilter()

  resp <- find(collection = testCollection, filter = irisFilter) |>
    httr::content()

  respNames <- names(unlist(resp$documents)) |> unique()

  expect_equal(respNames, c("_id", "Source_Id", "Sepal_Length",
                                       "Sepal_Width", "Petal_Length", "Petal_Width",
                                       "Species"))
})


test_that("find statusCode", {
  key <- Sys.getenv("mongekyoTestKey")
  testCollection <- getTestCollection(key)

  irisFilter <- mongEq("40_iris_LocalId", "Source_Id") |>
    mongFilter()

  resp <- find(collection = testCollection, filter = irisFilter)

  expect_equal(resp$status_code, 200)

})

