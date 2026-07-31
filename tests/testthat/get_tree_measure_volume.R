test_that("get_tree_measure_volume() works", {
  #library(testthat)
  
  f <- kwb.smartwater:::get_tree_measure_volume
  
  expect_error(f())
  
  expect_error(f(block_measures = 1))
  
  expect_error(f(block_measures = data.frame(trees_sm = 1:2)))
  
  expect_error(f(block_measures = data.frame(trees_sm = 1)))
  
  expect_equal(f(block_measures = data.frame(trees_sm = 1,
                                             trees_md = 0,
                                             trees_lg = 0)), 200)
  
  expect_equal(f(block_measures = data.frame(trees_sm = 0,
                                             trees_md = 0,
                                             trees_lg = 0)), 0)
  
  expect_equal(f(block_measures = data.frame(trees_sm = 0,
                                             trees_md = 0,
                                             trees_lg = 10)), 4000)
  
})

