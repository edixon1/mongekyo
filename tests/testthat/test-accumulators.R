# mongSum ----
test_that("mongSum works with numeric input", {
  expect_equal(mongSum(1), "{\"$sum\": 1}")
})

test_that("mongSum works when input is a field", {
  expect_equal(mongSum("Quantity"), "{\"$sum\": \"$Quantity\"}")
})

# TODO: test case when input is an expression


