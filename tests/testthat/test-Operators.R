test_that("multiplication works", {
  expect_equal(2 * 2, 4)
})


# mongEq ----
test_that("mongEq value field params work", {
  expect_equal(mongEq(15, "height"), '{ "height": 15 }')

})

test_that("mongEq value field true parseDates", {
  expect_equal(mongEq(as.Date("2023-04-01"), "foolDate"),
               '{ "foolDate": { "$date": "2023-04-01T00:00:00+00:00" } }')
})

test_that("mongEq value field false parseDates", {
  expect_equal(mongEq(as.Date("2023-04-01"), "foolDate", parseDates = FALSE),
               '{ "foolDate": "2023-04-01" }')
})


test_that("mongEq basic KV pairs", {
  expect_equal(mongEq(kvPairs = iris[1,]),
               '{ "Sepal.Length": 5.1, "Sepal.Width": 3.5, "Petal.Length": 1.4, "Petal.Width": 0.2, "Species": "setosa" }')
})



