# Eliot Dixon
# 2023-06-19

# sortStage ----


test_that("sort stage works with single arguments", {
  expect_equal(sortStage(1, "THOURS"), "{\"$sort\": {\"THOURS\": 1}}")
})



test_that("sort stage works with single arguments in list", {
  expect_equal(sortStage(kvPairs = list("THOURS" = -1)), "{\"$sort\": {\"THOURS\": -1}}")
})


test_that("sort stage works with two arguments in list", {
  expect_equal(sortStage(kvPairs = list("TDATE" = 1, "THOURS" = -1)), "{\"$sort\": {\"TDATE\": 1, \"THOURS\": -1}}")
})

# groupStage ----

test_that("group stage works with one id and $sum accumulator", {
  accumulators <- list(totalHours = '{"$sum": "$THOURS"}')
  expect_equal(groupStage('"$PID"', accumulators), 
               "{\"$group\": {\"_id\": \"$PID\", \"totalHours\": {\"$sum\": \"$THOURS\"}}}")
  
  
})
