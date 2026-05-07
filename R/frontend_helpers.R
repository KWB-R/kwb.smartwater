#' Get names of measures supported by kwb.smartwater
#' @export
get_measure_names <- function() {
  list(
    "green_roof_ext" = "Extensives Gruendach",
    "green_roof_int" = "Intensives Gruendach",
    "unpaving" = "Entsiegelung",
    "permeable_paving" = "Teilentsiegelung",
    "to_inf_mulde" = "Anschluss an Muldenversickerung",
    "to_inf_rigole" = "Anschluss an Rigolenversickerung",
    "to_inf_mulde_rigole" = "Anschluss an Mulden-Rigolenversickerung",
    "to_retention" = "Anschluss an Speicher"
  )
}

get_test_config <- function() {
  list(
    
  )
  green_roof = list(
    list(
      roof_fraction_column ="green_roof_ext",
      bagrov_value = 0.65
    ),
    list(
      roof_fraction_column ="green_roof_int",
      bagrov_value = 0.8
    )
  )  
} 

#' Get one block (columns as expected by kwb.rabimo) for testing
#' @export
get_test_block <- function() {
  block <- as.data.frame(kwb.rabimo::rabimo_inputs_2025$data[
    kwb.rabimo::rabimo_inputs_2025$data$code == "1100541241000000",
  ])
  block[, !names(block) %in% c("Shape", "block_type")]
}

#' Get a list of measures to apply for testing
#' @export
get_test_measures <- function() {
  list(
    list(
      name = "green_roof_ext",
      area = 1000
    ),
    list(
      name = "green_roof_int",
      area = 800
    ),
    list(
      name = "unpaving",
      area = 1000
    ),
    list(
      name = "permeable_paving",
      area = 800
    )
  )
}

#' Convert R-Abimo-block to partial areas given in m2
#' @param block R-Abimo block, as e.g. returned by \code{\link{get_test_block}}
#' @export
rabimo_block_to_partial_areas_m2 <- function(block) {
  total <- block$total_area
  roof <- block$total_area * block$roof
  pvd <- total * block$pvd
  current <- list(
    total = total,
    roof = roof,
    pvd = NA, # calculated from pvd_1, pvd_2, pvd_3, pvd_4, pvd_na
    pvd_1 = block$srf1_pvd * pvd,
    pvd_2 = block$srf2_pvd * pvd,
    pvd_3 = block$srf3_pvd * pvd,
    pvd_4 = block$srf4_pvd * pvd,
    pvd_na = block$srf5_pvd * pvd,
    sealed = NA, # calculated
    unsealed = NA, # calculated,
    green_roof_ext = roof * block$green_roof,
    green_roof_int = 0,
    to_inf_mulde = block$to_swale,
    to_inf_rigole = 0,
    to_inf_mulde_rigole = 0,
    to_retention = 0
  )
  update_calculated_fields(areas = as.data.frame(current))
}

update_calculated_fields <- function(areas) {
  
  areas$pvd <- with(areas, pvd_1 + pvd_2 + pvd_3 + pvd_4 + pvd_na)
  areas$sealed <- with(areas, pvd + roof)
  areas$unsealed <- with(areas, total - sealed)
  areas
}

#' Get available area for each measure, based on current "state"
#' @param areas areas in m2, as returned by rabimo_block_to_partial_areas_m2
#' @export
get_available_m2 <- function(areas) {
  available_green_roof <- with(
    areas, 
    roof - green_roof_ext - green_roof_int
  )
  data.frame(
    green_roof_ext = available_green_roof,
    green_roof_int = available_green_roof,
    unpaving = areas$pvd,
    # Everything of pvd that is not yet in surface class 4
    permeable_paving = with(areas, pvd - pvd_4),
    to_inf_mulde = areas$sealed,
    to_inf_rigole = areas$sealed,
    to_inf_mulde_rigole = areas$sealed,
    to_retention = areas$sealed
  )
}

#' Apply a measure to the current state of area assignments
#' 
#' @param areas one-row data frame with each column representing a partial area
#'   of the total block area, in m2, as e.g. returned by 
#'   \code{\link{rabimo_block_to_partial_areas_m2}}
#' @param measure list with elements "name" (name of measurement) and
#'   area (area in m2 that is assigned to the measure)
#'
#' @export
apply_measure <- function(areas, measure) {
  measure_name <- measure$name
  
  if (measure_name %in% c("green_roof_ext", "green_roof_int")) {
    
    areas[[measure_name]] <- areas[[measure_name]] + measure$area
    
  } else if (measure_name == "unpaving") {
    
    new_pvd <- areas$pvd - measure$area
    scaling_factor <- new_pvd / areas$pvd
    
    areas$pvd_1 <- areas$pvd_1 * scaling_factor
    areas$pvd_2 <- areas$pvd_2 * scaling_factor
    areas$pvd_3 <- areas$pvd_3 * scaling_factor
    areas$pvd_4 <- areas$pvd_4 * scaling_factor
    areas$pvd_na <- areas$pvd_na * scaling_factor
    
    stopifnot(new_pvd == with(areas, pvd_1 + pvd_2 + pvd_3 + pvd_4 + pvd_na))
    
  } else if (measure_name == "permeable_paving") {
    
    # increase pvd_4, take equally from pvd_1, pvd_2, pvd_3, pvd_na
    pvd_not_4 <- with(areas, pvd_1 + pvd_2 + pvd_3 + pvd_na)
    scaling_factor <- 1 - 1 / pvd_not_4 * measure$area
    
    areas$pvd_4 <- areas$pvd_4 + measure$area
    areas$pvd_1 <- areas$pvd_1 * scaling_factor
    areas$pvd_2 <- areas$pvd_2 * scaling_factor
    areas$pvd_3 <- areas$pvd_3 * scaling_factor
    areas$pvd_na <- areas$pvd_na * scaling_factor

  } else {
    
    stop(sprintf("Measure '%s' not supported!", measure_name))
  }
  
  update_calculated_fields(areas)
}

#' Test the plumber API
#' @export
test_plumber_api <- function() {
  plumber_file <- system.file("scripts/plumber.R", package = "kwb.smartwater")
  plumber::pr_run(plumber::pr(plumber_file))
}
