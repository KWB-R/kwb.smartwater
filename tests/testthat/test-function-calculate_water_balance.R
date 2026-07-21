#library(testthat)
test_that("calculate_water_balance() works", {
  
  f <- kwb.smartwater:::calculate_water_balance
  
  expect_error(f())
  
  blocks <- rbind(
    data.frame(
      "code" = "0100988021000000",
      "prec_yr" = 616,
      "prec_s" = 326,
      "epot_yr" = 825,
      "epot_s" = 0,
      "district" = "1",
      "total_area" = 27703.6579,
      "roof" = 0,
      "green_roof" = 0,
      "swg_roof" = 0,
      "pvd" = 0,
      "swg_pvd" = 0,
      "srf1_pvd" = 0,
      "srf2_pvd" = 0,
      "srf3_pvd" = 0,
      "srf4_pvd" = 0,
      "srf5_pvd" = 1,
      "to_swale" = 0,
      "gw_dist" = 0,
      "ufc30" = 0,
      "ufc150" = 0,
      "land_type" = "waterbody",
      "veg_class" = 0,
      "irrigation" = 0
    ),
    data.frame(
      "code" = "1100541241000000",
      "prec_yr" = 626,
      "prec_s" = 334,
      "epot_yr" = 668,
      "epot_s" = 510,
      "district" = "11",
      "total_area" = 6738.9059,
      "roof" = 0.6305,
      "green_roof" = 0.122,
      "swg_roof" = 0.95,
      "pvd" = 0.2076,
      "swg_pvd" = 0.7,
      "srf1_pvd" = 0.56,
      "srf2_pvd" = 0.22,
      "srf3_pvd" = 0.03,
      "srf4_pvd" = 0.19,
      "srf5_pvd" = 0,
      "to_swale" = 0,
      "gw_dist" = 7.1,
      "ufc30" = 13,
      "ufc150" = 10,
      "land_type" = "urban",
      "veg_class" = 17.8,
      "irrigation" = 0
    ),
    data.frame(
      "code" = "1400761421000000",
      "prec_yr" = 641,
      "prec_s" = 337,
      "epot_yr" = 669,
      "epot_s" = 511,
      "district" = "14",
      "total_area" = 12737.3007,
      "roof" = 0.4466,
      "green_roof" = 0.1363,
      "swg_roof" = 0.87,
      "pvd" = 0.3408,
      "swg_pvd" = 0.66,
      "srf1_pvd" = 0.57,
      "srf2_pvd" = 0.32,
      "srf3_pvd" = 0.04,
      "srf4_pvd" = 0.07,
      "srf5_pvd" = 0,
      "to_swale" = 0,
      "gw_dist" = 3.1,
      "ufc30" = 12,
      "ufc150" = 10,
      "land_type" = "urban",
      "veg_class" = 31.5,
      "irrigation" = 0
    )
  )
  
  measures <- rbind(
    data.frame(
      "code" = "1100541241000000",
      "green_roof_ext" = 10,
      "green_roof_int" = 10,
      "permeable_paving" = 10,
      "unpaving" = 10,
      "trees_sm" = 10,
      "trees_md" = 10,
      "trees_lg" = 10,
      "to_swale" = 10,
      "to_surf_infil" = 10,
      "to_swale_trench" = 10,
      "to_tree_pit" = 10,
      "to_trench" = 10,
      "to_cistern" = 10
    ),
    data.frame(
      "code" = "1400761421000000",
      "green_roof_ext" = 10,
      "green_roof_int" = 10,
      "permeable_paving" = 10,
      "unpaving" = 10,
      "trees_sm" = 10,
      "trees_md" = 10,
      "trees_lg" = 10,
      "to_swale" = 10,
      "to_surf_infil" = 10,
      "to_swale_trench" = 10,
      "to_tree_pit" = 10,
      "to_trench" = 10,
      "to_cistern" = 10
    )
  )
  
  result <- f(blocks, measures, convert_types = TRUE)
  
  expect_true(is.list(result))
  expect_equal(names(result), c(
    "water_balance",
    "statistics"
  ))

  expect_equal(names(result$water_balance), c(
    "status_quo",
    "with_measures"
  ))
  
  check_water_balance <- function(df) {
    expect_equal(names(df), c(
      "code",
      "area",
      "runoff",
      "infiltr",
      "evapor",
      "delta_w"
    ))
  }
  
  check_water_balance(result$water_balance$with_measures)
  check_water_balance(result$water_balance$status_quo)
  
  expect_equal(names(result$statistics), c(
    "water_balance",
    "runoff_reduction_percent",
    "water_quality_indicators"
  ))

  expected_fields <- c("status_quo", "with_measures")
  expect_equal(names(result$statistics$water_balance), expected_fields)
  expect_equal(names(result$statistics$water_quality_indicators), expected_fields)

  expected_fields <- c("overflow_volume", "critical_hours", "critical_events")
  expect_equal(names(result$statistics$water_quality_indicators$status_quo), expected_fields)
  expect_equal(names(result$statistics$water_quality_indicators$with_measures), expected_fields)
  
})
