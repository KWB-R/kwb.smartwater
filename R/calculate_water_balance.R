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
  #kwb.utils::assignPackageObjects("kwb.smartwater")
  
  # kwb.rabimo is strict about data types. Therefore, convert data types as
  # necessary
  if (convert_types) {
    blocks <- kwb.rabimo:::check_or_convert_data_types(
      data = blocks, 
      types = kwb.rabimo:::get_expected_data_type(names(blocks)), 
      convert = TRUE, 
      dbg = FALSE
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

  # Prepare vectors with names of fields/columns (= names of measures)
  fields_green_roof <- get_measure_info("green_roof", TRUE)
  fields_inf_ret <- get_measure_info(c("infiltration", "retention"), TRUE)

  # Calculate absolute areas in m2 from area percentages and add one column per
  # measure
  all_areas_m2 <- kwb.smartwater:::rabimo_block_to_partial_areas_m2(blocks)
  
  # Modify the blocks according to the measures (if any) for each block
  new_blocks <- do.call(rbind, lapply(seq_len(nrow(blocks)), function(i) {
    #i <- 1L
    block <- blocks[i, ]
    areas_m2 <- all_areas_m2[i, ]
    
    if (block$code %in% measures$code) {
      
      block_measures <- measures[measures$code == block$code, ]
      
      # green roof measures: increase the green roof area
      areas_m2[, fields_green_roof] <- areas_m2[, fields_green_roof] +
        block_measures[fields_green_roof]
      
      # infiltration and retention measures: increase the connected area
      areas_m2[, fields_inf_ret] <- areas_m2[, fields_inf_ret] +
        block_measures[fields_inf_ret]
      
      # paving measures
      for (measure_name in c("unpaving", "permeable_paving")) {
        measure_value <- measures[[measure_name]][i]
        if (measure_value > 0) {
          areas_m2 <- apply_measure(
            areas = areas_m2, 
            measure = list(name = measure_name, area = measure_value)
          )
        }
      }
      
      # TODO: tree measures
    }
    
    # Calculate m2 back into percentages
    update_block(block, areas_m2)
  }))
  
  # Run R-Abimo with measures
  water_balance_after <- kwb.rabimo::run_rabimo(new_blocks, config)
  water_balance_after <- add_delta_w(
    water_balance = water_balance_after, 
    delta_w = kwb.rabimo::calculate_delta_w(
      natural = water_balance_natural,
      urban = water_balance_after
    )
  )

  area_weighted <- function(pathway, water_balance) {
    area <- water_balance[["area"]]
    sum(water_balance[[pathway]] * area) / sum(area)
  }
  
  runoff_before <- area_weighted("runoff", water_balance_before)
  runoff_after <- area_weighted("runoff", water_balance_after)
  
  list(
    water_balance_with_measures = water_balance_after,
    water_balance_original = water_balance_before,
    statistics = list(
      with_measures = data.frame(
        runoff = runoff_after,
        infiltr = area_weighted("infiltr", water_balance_after),
        evapor = area_weighted("evapor", water_balance_after)
      ),
      original = data.frame(
        runoff = runoff_before,
        infiltr = area_weighted("infiltr", water_balance_before),
        evapor = area_weighted("evapor", water_balance_before)
      ),
      runoff_reduction_percent = 100 * (
        1 - safe_division(runoff_after, runoff_before)
      )
    )
  )
}

update_block <- function(block, areas_m2, check = TRUE) {
  
  # remove "old" measure fields
  block <- block[!names(block) %in% c("green_roof", "to_swale")]
  
  # green roof measures
  fields_green_roof <- get_measure_info("green_roof", TRUE)
  for (field in fields_green_roof) {
    block[[field]] <- safe_division(areas_m2[[field]], areas_m2$roof)
  }
  
  # assume that pavement-related measures are applied first -> update sealed
  fields_paved <- c("pvd_1", "pvd_2", "pvd_3", "pvd_4", "pvd_na")
  paved_parts_m2 <- unlist(areas_m2[fields_paved])
  total_paved_m2 <- sum(paved_parts_m2)
  fields_srf <- c("srf1_pvd", "srf2_pvd", "srf3_pvd", "srf4_pvd", "srf5_pvd")
  block[fields_srf] <- safe_division(paved_parts_m2, total_paved_m2)
  block[["pvd"]] <- safe_division(total_paved_m2, block[["total_area"]])
  
  # infiltration/retention measures: convert m2 to fraction of sealed
  # REMEMBER! Do not allow more than 1 (100%)
  fields_inf_ret <- get_measure_info(c("infiltration", "retention"), TRUE)
  sealed_m2 <- areas_m2$roof + total_paved_m2
  for (field in fields_inf_ret) {
    block[[field]] <- safe_division(areas_m2[[field]], sealed_m2)
  }
  
  # Correct percentages of infiltration/retention measures in case that their
  # sum exceeds a value of 1.
  percentages <- unlist(block[fields_inf_ret])
  if (sum(percentages) > 1) {
    warning(
      "The sum of the areas connected to infiltration/retention measures ", 
      "exceeds the sealed area. The connected areas are equally reduced so ", 
      "that their sum equals the total sealed area."
    )
    block[fields_inf_ret] <- percentages / sum(percentages)
  }
  
  if (check) {
    stopifnot(sum(unlist(block[fields_green_roof])) <= 1)
    stopifnot(sum(unlist(block[fields_inf_ret])) <= 1)
  }

  block
}

# helper function for division by zero
safe_division <- function(dividends, divisors) {
  mapply(kwb.utils::quotient, dividends, divisors, substitute.value = 0)
}

# helper function to add delta_w column  
add_delta_w <- function(water_balance, delta_w) {
  stopifnot(identical(delta_w[["code"]], water_balance[["code"]]))
  cbind(water_balance, delta_w = delta_w[["delta_w"]])
}

get_measures_config <- function() {
  lapply(
    X = stats::setNames(nm = c("green_roof", "infiltration", "retention")), 
    FUN = function(type) {
      abimo_parameters <- lapply(get_measure_info(type), function(x) {
        c(
          list(input_column = x[["field_name"]]),
          x[["abimo_parameters"]]
        )
      })
      abimo_parameters[!sapply(abimo_parameters, is.null)]
    }
  )
}
