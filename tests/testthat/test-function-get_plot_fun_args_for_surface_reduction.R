#library(testthat)
test_that("get_plot_fun_args_for_surface_reduction() works", {
  
  f <- kwb.smartwater:::get_plot_fun_args_for_surface_reduction
  
  expect_error(f())
  expect_error(f(10))
  
  expect_no_error(result_critical_hours <- f(10, "critical_hours"))
  expect_no_error(result_critical_events <- f(10, "critical_events"))
  expect_no_error(result_unpleasant_hours <- f(10, "unpleasant_hours"))
  expect_no_error(result_negative_deviation <- f(10, "negative_deviation"))
  
  results <- list(
    result_critical_hours,
    result_critical_events,
    result_unpleasant_hours,
    result_negative_deviation
  )
  
  for (result in results) {
    expect_equal(names(result), c("plot_args", "additional_values"))
    expect_equal(names(result$additional_values), c("overflowVolume", "berlinWide"))
  }
})
