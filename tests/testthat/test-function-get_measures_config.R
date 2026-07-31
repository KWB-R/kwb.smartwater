#library(testthat)

test_that("get_measures_config() works", {
  
  result <- kwb.smartwater:::get_measures_config()
  
  expect_true(is.list(result))
  expect_equal(names(result), c("green_roof", "infiltration", "retention"))

})

test_that("get_measures_config() accepts parameters to be overridden", {
  
  f <- kwb.smartwater:::get_measures_config
  parameters <- get_measure_info(parameters_only = TRUE)
  expect_identical(f(parameters), f())
  
  parameters[[c("green_roof_ext", "bagrov_value")]] <- 0.75
  result <- f(parameters)
  
  green_roof <- result[["green_roof"]]
  input_columns <- sapply(green_roof, "[[", "input_column")
  expect_equal(
    green_roof[[which(input_columns == "green_roof_ext")]][["bagrov_value"]],
    0.75
  )
})

test_that("get_measures_config() accepts an empty parameter list", {
  f <- kwb.smartwater:::get_measures_config
  expect_no_error(config <- f(list()))
  expect_identical(config, f())
})
