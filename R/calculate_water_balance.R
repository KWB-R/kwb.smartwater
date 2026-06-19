#' Calculate Water Balance for Given Block Areas and Measures

#' @param blocks data.frame as returned by \code{\link{get_test_blocks}}
#' @param measures data.frame containing information about the planned measures
#'   in m2. Each row refers to a block area, linked by the text field
#'   \code{code}. There is one numeric field per measure. The names of the
#'   measure-related fields must correspond to the \code{field_name}s returned 
#'   by \code{\link{get_measure_info}}.
#' @param convert_types logical value indicating whether or not to convert the
#'   data types in the \code{blocks} data frame as required by R-ABIMO.
#' @export
calculate_water_balance <- function(blocks, measures, convert_types = FALSE) {

  #blocks <- kwb.smartwater::get_test_blocks()
  #measures <- kwb.smartwater::get_test_block_measures()
  #convert_types = FALSE

  # bring measures into the same order as blocks
  indices <- match(blocks[["code"]], measures[["code"]])
  if (any(is_missing <- is.na(indices))) {
    stop(
      "There are no measures given for blocks with code: ", 
      paste0("'", blocks[["code"]][is_missing], "'", collapse = ", ")
    )
  }
  measures <- measures[indices, ]
  
  # kwb.rabimo is strict about data types. Therefore, convert data types as
  # necessary
  if (convert_types) {
    blocks <- kwb.rabimo:::check_or_convert_data_types(
      data = blocks, 
      types = kwb.rabimo:::get_expected_data_type(names(blocks)), 
      convert = TRUE
    )
  }

  config <- kwb.rabimo:::reconfigure(kwb.rabimo::rabimo_inputs_2025$config)
  config[["measures"]] <- get_measures_config()
  
  # Calculate water balance for natural state
  water_balance_natural <- kwb.rabimo::run_rabimo(
    data = kwb.rabimo::data_to_natural(blocks),
    config = config
  )

  # Run R-Abimo without measures (added by the user, green_roof may already be 
  # there in the blocks)
  water_balance_before <- kwb.rabimo::run_rabimo(blocks, config)
  water_balance_before <- add_delta_w(
    water_balance = water_balance_before, 
    delta_w = kwb.rabimo::calculate_delta_w(
      natural = water_balance_natural,
      urban = water_balance_before
    )
  )
  
  # TODO: modify the blocks according to the measures
  new_blocks <- blocks
  
  # green roof measures: convert m2 to fraction of roof
  green_roof_m2 <- new_blocks$total_area * new_blocks$roof
  fields_green_roof <- get_measure_info("green_roof", TRUE)
  new_blocks[fields_green_roof] <- measures[fields_green_roof] / green_roof_m2

  # infiltration/retention measures: convert m2 to fraction of sealed
  sealed_m2 <- new_blocks$total_area * (new_blocks$roof + new_blocks$pvd)
  fields_inf_ret <- get_measure_info(c("infiltration", "retention"), TRUE)
  new_blocks[fields_inf_ret] <- measures[fields_inf_ret] / sealed_m2
  
  # TDOO: pavement measures
  # TODO: tree measures
  
  # Run R-Abimo with measures
  water_balance_after <- kwb.rabimo::run_rabimo(new_blocks, config)
  water_balance_after <- add_delta_w(
    water_balance = water_balance_after, 
    delta_w = kwb.rabimo::calculate_delta_w(
      natural = water_balance_natural,
      urban = water_balance_after
    )
  )
  
  list(
    water_balance_with_measures = water_balance_after,
    water_balance_original = water_balance_before,
    statistics = list(
      runoff_reduction_percent = 100 * (
        1 - sum(water_balance_after[["runoff"]]) / 
          sum(water_balance_before[["runoff"]])
      )
    )
  )
}

# helper function to add delta_w column  
add_delta_w <- function(water_balance, delta_w) {
  stopifnot(identical(delta_w[["code"]], water_balance[["code"]]))
  cbind(water_balance, delta_w = delta_w[["delta_w"]])
}

get_measures_config <- function() {
  list(
    green_roof = list(
      list(
        input_column = "green_roof_ext",
        bagrov_value = 0.65
      ), 
      list(
        input_column = "green_roof_int",
        bagrov_value = 0.75
      )
    ),
    infiltration = list(
      list(
        input_column = "to_swale",
        evaporation_factor = 0.1,
        overflow_factor = 0.05
      ),
      list(
        input_column = "to_surf_infil",
        evaporation_factor = 0.15,
        overflow_factor = 0.15
      ),
      list(
        input_column = "to_swale_trench",
        evaporation_factor = 0.08,
        overflow_factor = 0.1
      ),
      list(
        input_column = "to_tree_pit_sm",
        evaporation_factor = 0.1,
        overflow_factor = 0.2
      ),
      list(
        input_column = "to_tree_pit_md",
        evaporation_factor = 0.2,
        overflow_factor = 0.15
      ),
      list(
        input_column = "to_tree_pit_lg",
        evaporation_factor = 0.3,
        overflow_factor = 0.15
      ),
      list(
        input_column = "to_trench",
        evaporation_factor = 0.15,
        overflow_factor = 0.15
      )
    ),
    retention = list(
      list(
        input_column = "to_cistern",
        overflow_factor = 0.5
      )
    )
  )
}
