# library(testthat)
test_that("get_water_quality_indicators() works", {

  f <- kwb.smartwater:::get_water_quality_indicators
  
  expect_error(f())
  
  result_10 <- f(10)
  expect_true(is.list(result_10))
  expect_equal(
    names(result_10), 
    c("overflow_volume", "critical_hours", "critical_events")
  )
  
  result_100 <- f(100)
  expect_equal(result_100$critical_hours, 0)
  
  expect_true(result_10$overflow_volume > result_100$overflow_volume)
  expect_true(result_10$critical_hours > result_100$critical_hours)
  expect_true(result_10$critical_events > result_100$critical_events)
})
