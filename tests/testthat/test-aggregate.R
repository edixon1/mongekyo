test_that("pipeline works with single match stage", {
  
  stage <- "[{\"$match\": { \"PID\": \"trailProject\" }}]"
  
  expect_equal(pipeline(stage), "\"pipeline\": [{\"$match\": { \"PID\": \"trailProject\" }}]")
})



test_that("pipeline works with two stages, sort and match", {
  
  stages <- list(
    match = "{\"$match\": { \"PID\": \"trailProject\" }}",
    sort = "{\"$sort\": {\"THOURS\": 1}}"
  )
  
  expect_equal(pipeline(stages), "\"pipeline\": [{\"$match\": { \"PID\": \"trailProject\" }}, {\"$sort\": {\"THOURS\": 1}}]")
})

