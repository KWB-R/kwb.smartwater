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
  result$green_roof[[1]]

})



parameter_list <- '{
  "green_roof_ext": {
    "bagrov_value": 0.65
  },
  "green_roof_int": {
    "bagrov_value": 0.75
  },
  "permeable_paving": {},
  "unpaving": {},
  "trees_sm": {
    "tree_volume": 200
  },
  "trees_md": {
    "tree_volume": 300
  },
  "trees_lg": {
    "tree_volume": 400
  },
  "to_swale": {
    "evaporation_factor": 0.1,
    "overflow_factor": 0.05
  },
  "to_surf_infil": {
    "evaporation_factor": 0.15,
    "overflow_factor": 0.15
  },
  "to_swale_trench": {
    "evaporation_factor": 0.08,
    "overflow_factor": 0.1
  },
  "to_tree_pit": {
    "evaporation_factor": 0.2,
    "overflow_factor": 0.15
  },
  "to_trench": {
    "evaporation_factor": 0.15,
    "overflow_factor": 0.15
  },
  "to_cistern": {
    "overflow_factor": 0.5
  }
}'

kwb.smartwater:::get_measures_config(parameter_list)
