# Eliot Dixon
# 2023-06-19

# sortStage ----


test_that("sortStage works with single arguments", {
  expect_equal(sortStage(1, "THOURS"), "{\"$sort\": {\"THOURS\": 1}}")
})



test_that("sortStage works with single arguments in list", {
  expect_equal(sortStage(kvPairs = list("THOURS" = -1)), "{\"$sort\": {\"THOURS\": -1}}")
})


test_that("sortStage works with two arguments in list", {
  expect_equal(sortStage(kvPairs = list("TDATE" = 1, "THOURS" = -1)), "{\"$sort\": {\"TDATE\": 1, \"THOURS\": -1}}")
})

# groupStage ----

test_that("groupStage works with one id and $sum accumulator", {
  accumulators <- list(totalHours = '{"$sum": "$THOURS"}')
  expect_equal(groupStage("PID", accumulators), 
               "{\"$group\": {\"_id\": \"$PID\", \"totalHours\": {\"$sum\": \"$THOURS\"}}}")
  
  
})


test_that("groupStage works when KVpairs is provided with multiple _id fieds", {
  output <- groupStage(kvPairs = list("project" = "PID", "employee" = "TSTAFF"), 
             accumulators = list(totalHours = mongSum("THOURS")))
  
  expect_equal(output, "{\"$group\": {\"_id\": {\"project\": \"$PID\", \"employee\": \"$TSTAFF\"}, \"totalHours\": {\"$sum\": \"$THOURS\"}}}")
})



# lookupStage ----

test_that("lookupStage works with basic input", {
output <- lookupStage(from = "movies", localField = "movie_id",
                      foreignField = "_id", as = "movieDocs")

expect_equal(output, "{\"$lookup\": {\"from\": \"movies\", \"localField\": \"movie_id\", \"foreignField\": \"_id\", \"as\": \"movieDocs\"}}")
})
