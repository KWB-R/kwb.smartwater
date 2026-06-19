#' Get info on the measures supported by kwb.smartwater
#' 
#' Get information on the rainwater management measures supported by 
#' kwb.smartwater
#' @param type optional. Vector of character indicating the method types 
#'   ("green_roof", "pavement", "trees", "infiltration", "retention") for which 
#'   to filter the output.
#' @param field_name_only optional. Logical of length one indicating whether or 
#'   not to return only the "field_name" instead of all info fields per measure
#' @export
get_measure_info <- function(type = character(0), field_name_only = FALSE) {
  measures <- list(
    list(
      type = "green_roof",
      field_name = "green_roof_ext",
      long_name_de = "Extensive Dachbegr\u00fcnung"
    ),
    list(
      type = "green_roof",
      field_name = "green_roof_int",
      long_name_de = "Intensive Dachbegr\u00fcnung"
    ),
    list(
      type = "pavement",
      field_name = "permeable_paving",
      long_name_de = "Teilversiegelte Oberfl\u00e4chen"
    ),
    list(
      type = "pavement",
      field_name = "unpaving",
      long_name_de = "Vollst\u00e4ndige Entsiegelung/Gr\u00fcnfl\u00e4chen"
    ),
    list(
      type = "trees",
      field_name = "trees_sm",
      long_name_de = "B\u00e4ume (klein)"
    ),
    list(
      type = "trees",
      field_name = "trees_md",
      long_name_de = "B\u00e4ume (mittel)"
    ),
    list(
      type = "trees",
      field_name = "trees_lg",
      long_name_de = "B\u00e4ume (gro\u00df)"
    ),
    list(
      type = "infiltration",
      field_name = "to_swale", # "to_inf_mulde"
      long_name_de = "Mulde"
    ),
    list(
      type = "infiltration",
      field_name = "to_surf_infil",
      long_name_de = "Fl\u00e4chenversickerung"
    ),
    list(
      type = "infiltration",
      field_name = "to_swale_trench", # "to_inf_mulde_rigole"
      long_name_de = "Mulden-Rigolen-Element"
    ),
    # list(
    #   type = "infiltration",
    #   field_name = "to_bio_trench",
    #   long_name_de = "Tiefbeet-Rigole"
    # ),
    list(
      type = "infiltration",
      field_name = "to_tree_pit_sm",
      long_name_de = "Optimierter Baumstandort (kleiner Baum)"
    ),  
    list(
      type = "infiltration",
      field_name = "to_tree_pit_md",
      long_name_de = "Optimierter Baumstandort (mittlerer Baum)"
    ),  
    list(
      type = "infiltration",
      field_name = "to_tree_pit_lg",
      long_name_de = "Optimierter Baumstandort (gro\u00dfer Baum)"
    ),  
    list(
      type = "infiltration",
      field_name = "to_trench",
      long_name_de = "Rigole"
    ),
    list(
      type = "retention",
      field_name = "to_cistern", # "to_retention"
      long_name_de = "Zisterne" # (= Regentonne)
    )
  )
  # helper function to collect a specific field from each list element
  collect <- function(x, field) {
    sapply(x, `[[`, field)
  }
  if (length(type) > 0L) {
    allowed_types <- unique(collect(measures, "type"))
    unknown_types <- setdiff(type, allowed_types)
    collapse <- function(x) paste0("'", x, "'", collapse = ", ")
    if (length(unknown_types)) {
      stop(
        "Unknown types: ", collapse(unknown_types), ". Possible types are: ", 
        collapse(allowed_types)
      )
    }
    measures <- measures[collect(measures, "type") %in% type]
  }
  if (field_name_only) {
    collect(measures, "field_name")
  } else {
    measures
  }
}

#' Get one block (columns as expected by kwb.rabimo) for testing
#' @param codes codes of the blocks to be selected from the Berlin dataset
#' @export
get_test_blocks <- function(codes = c("1100541241000000", "1400761421000000")) {
  all_blocks <- kwb.rabimo::rabimo_inputs_2025[["data"]]
  blocks <- as.data.frame(all_blocks[all_blocks[["code"]] %in% codes, ])
  columns_to_remove <- c("Shape", "block_type")
  blocks[, !names(blocks) %in% columns_to_remove]
}

#' Get measures for testing
#' @param codes codes of the blocks to be selected from the Berlin dataset.
#' @export
get_test_block_measures <- function(codes) {
  measure_fields <- lapply(
    X = stats::setNames(nm = get_measure_info(field_name_only = TRUE)),
    function(x) 10
  )
  cbind(
    code = get_test_blocks()[["code"]], 
    as.data.frame(measure_fields)
  )
}

#' Convert R-Abimo-block to partial areas given in m2
#' @param block R-Abimo block, as e.g. returned by \code{\link{get_test_blocks}}
#' @export
rabimo_block_to_partial_areas_m2 <- function(block) {
  #block <- get_test_blocks()[1, ]
  total <- block[["total_area"]]
  roof <- total * block[["roof"]]
  pvd <- total * block[["pvd"]]
  current <- data.frame(
    code = block[["code"]],
    total = total,
    roof = roof,
    pvd = NA, # calculated from pvd_1, pvd_2, pvd_3, pvd_4, pvd_na
    pvd_1 = block[["srf1_pvd"]] * pvd,
    pvd_2 = block[["srf2_pvd"]] * pvd,
    pvd_3 = block[["srf3_pvd"]] * pvd,
    pvd_4 = block[["srf4_pvd"]] * pvd,
    pvd_na = block[["srf5_pvd"]] * pvd,
    sealed = NA, # calculated
    unsealed = NA # calculated
  )
  
  # append measure columns (all initialised with zero) for non-tree-measures
  measures <- get_measure_info(field_name_only = TRUE)
  current <- cbind(current, as.data.frame(as.list(
    stats::setNames(rep(0, length(measures)), measures)
  )))

  # recalculate `pvd`, `sealed`, `unsealed`
  current <- update_calculated_fields(current)
  
  # set measures for which we find information in the block data (as used in
  # the AMAREX project)
  current[["green_roof_ext"]] <- roof * block[["green_roof"]]
  current[["to_swale"]] <- current[["sealed"]] * block[["to_swale"]]
  
  # return the "current state" of partial areas
  current
}

update_calculated_fields <- function(areas) {
  areas[["pvd"]] <- with(areas, pvd_1 + pvd_2 + pvd_3 + pvd_4 + pvd_na)
  areas[["sealed"]] <- with(areas, pvd + roof)
  areas[["unsealed"]] <- with(areas, total - sealed)
  areas
}

#' Get available area for each measure, based on current "state"
#' @param areas areas in m2, as returned by rabimo_block_to_partial_areas_m2
#' @export
get_available_m2 <- function(areas) {
  
  # initialise result list, to be filled with one value per measure
  available <- list()
  
  ### green roof measures
  fields_green_roof <- get_measure_info(
    type = "green_roof", 
    field_name_only = TRUE
  )
  # "green_roof_ext" "green_roof_int"
  available[fields_green_roof] <- areas[["roof"]] - rowSums(areas[fields_green_roof])
  
  ### pavement measures
  # all paved can be unpaved
  available[["unpaving"]] <- areas[["pvd"]]
  # everything of pvd that is not yet in surface class 4 can go into class 4
  available[["permeable_paving"]] <- areas[["pvd"]] - areas[["pvd_4"]]
  
  ### infiltration and retention measures
  # all sealed area can be connected to infiltration/retention measures
  fields_inf_ret <- get_measure_info(
    type = c("infiltration", "retention"), 
    field_name_only = TRUE
  )
  # "to_swale" "to_surf_infil" "to_swale_trench" "to_tree_pit_sm" "to_tree_pit_md" 
  # "to_tree_pit_lg" "to_trench" "to_cistern"
  available[fields_inf_ret] <- areas[["sealed"]] - rowSums(areas[fields_inf_ret])
  
  ### tree measures: not considered here -> there are no limits!
  
  # return the available areas in m2 as a one-line data frame
  as.data.frame(available)
}

#' Apply a measure to the current state of area assignments
#' 
#' Attention: The tree-measures are not considered here!
#' @param areas one-row data frame with each column representing a partial area
#'   of the total block area, in m2, as e.g. returned by 
#'   \code{\link{rabimo_block_to_partial_areas_m2}}
#' @param measure list with elements "name" (name of measurement) and
#'   area (area in m2 that is assigned to the measure)
#'
#' @export
apply_measure <- function(areas, measure) {
  name <- measure[["name"]]
  
  if (!name %in% get_measure_info(field_name_only = TRUE)) {
    stop(sprintf("Measure '%s' not supported!", name))
  }

  # Add the area of the measure to area that is already allocated to the measure
  areas[[name]] <- areas[[name]] + measure[["area"]]

  # Only measures related to paving need special treatment ("accordeon"):
  if (name == "unpaving") {
    
    new_pvd <- areas[["pvd"]] - measure[["area"]]
    scaling_factor <- new_pvd / areas[["pvd"]]
    
    areas[["pvd_1"]] <- areas[["pvd_1"]] * scaling_factor
    areas[["pvd_2"]] <- areas[["pvd_2"]] * scaling_factor
    areas[["pvd_3"]] <- areas[["pvd_3"]] * scaling_factor
    areas[["pvd_4"]] <- areas[["pvd_4"]] * scaling_factor
    areas[["pvd_na"]] <- areas[["pvd_na"]] * scaling_factor
    
    stopifnot(all.equal(
      target = new_pvd, 
      current = with(areas, pvd_1 + pvd_2 + pvd_3 + pvd_4 + pvd_na)
    ))
    
  } else if (name == "permeable_paving") {
    
    # increase pvd_4, take equally from pvd_1, pvd_2, pvd_3, pvd_na
    pvd_not_4 <- with(areas, pvd_1 + pvd_2 + pvd_3 + pvd_na)
    scaling_factor <- 1 - 1 / pvd_not_4 * measure[["area"]]
    
    areas[["pvd_4"]] <- areas[["pvd_4"]] + measure[["area"]]
    areas[["pvd_1"]] <- areas[["pvd_1"]] * scaling_factor
    areas[["pvd_2"]] <- areas[["pvd_2"]] * scaling_factor
    areas[["pvd_3"]] <- areas[["pvd_3"]] * scaling_factor
    areas[["pvd_na"]] <- areas[["pvd_na"]] * scaling_factor
    
  }
  
  update_calculated_fields(areas)
}

pkg_env <- new.env()
pkg_env[["running_api"]] <- NULL

#' Test the plumber API
#' @param catch_errors whether or not to catch errors. If TRUE, errors are
#'   caught and the result object is a list with elements "data" (containing the
#'   function result or NULL in case of an error) and "error" (containing the
#'   error message in case of an error and NULL otherwise)
#' @param use_plumber2 if \code{TRUE}, the plumber2 package is used, otherwise
#'   the plumber package
#' @export
run_plumber_api <- function(catch_errors = FALSE, use_plumber2 = FALSE) {
  env <- new.env(parent = .GlobalEnv)
  assign(
    x = "to_plumber_response", 
    value = if (catch_errors) to_plumber_response else identity,
    envir = env
  )
  if (use_plumber2) {
    if (is.null(pkg_env[["running_api"]])) {
      api <- plumber2_api(env = env)
      plumber2::api_run(api = api)
      pkg_env[["running_api"]] <- api
    } else {
      message("There is already a running API server.")
    }
  } else {
    pr <- plumber_pr(env = env)
    pr <- plumber::pr_set_debug(pr, TRUE) # does not seem to have an effect!
    plumber::pr_run(pr = pr)
  }
}

plumber2_api <- function(env = NULL) {
  file <- system.file("scripts/plumber2.R", package = "kwb.smartwater")
  if (is.null(env)) {
    plumber2::api(file)
  } else {
    plumber2::api(file, env = env)
  }
}

plumber_pr <- function(env = NULL) {
  file <- system.file("scripts/plumber.R", package = "kwb.smartwater")
  if (is.null(env)) {
    plumber::pr(file)
  } else {
    plumber::pr(file, envir = env)
  }
}

#' Stop the API server
#' @export
stop_plumber_api <- function() {
  if (is.null(pkg_env[["running_api"]])) {
    message("No running API server found.")
  } else {
    plumber2::api_stop(pkg_env[["running_api"]])
    pkg_env[["running_api"]] <- NULL
  }
}

#' Convert function result (may inherit from "try-error") to a response
#' @param result result of calling a function within try()
to_plumber_response <- function(result) {
  failed <- inherits(result, "try-error")
  list(
    data = if (!failed) result,
    error = if (failed) attr(result, "condition")[["message"]]
  )
}
