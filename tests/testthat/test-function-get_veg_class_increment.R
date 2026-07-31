test_that("get_veg_class_increment() works", {

  f <- kwb.smartwater:::get_veg_class_increment
  # library(testthat)
  expect_error(f())
  
  expect_equal(f(tree_measure_volume = 0, unsealed_area_m2 = 200), 0)
  expect_equal(f(tree_measure_volume = 200, unsealed_area_m2 = 0), 0)
  
})

