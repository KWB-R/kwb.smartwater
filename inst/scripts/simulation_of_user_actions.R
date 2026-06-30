#
# Simulation of user-actions, i.e. drawing polygons for different measures
#
# - Source the script first to load the functions defined below
# - Manually go through the code block within "if (FALSE)"

# Main -------------------------------------------------------------------------
if (FALSE)
{
  library(kwb.smartwater)
  
  # user selects a block (area-related columns except "total" are fractions)
  (block <- get_test_block())
  
  # user selects measures and areas by clicking/drawing polygons
  measures_to_apply <- list(
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
      area = 320
    )
  )
  
  # convert R-Abimo block to current "state" (all areas in m2)
  current <- rabimo_block_to_partial_areas_m2(block)
  print_current_state(current)
  
  available <- get_available_m2(current)
  print_available(available)
  
  # apply all measures (fail if an area exceeds the available area)
  histories <- apply_measures(current, measures_to_apply)
  
  histories$current
  histories$available
  histories$measure
  
  row_diff(histories$current)
  row_diff(histories$available)
}

print_current_state <- function(current) {
  cat("current state:\n")
  current[] <- lapply(current, round, digits = 1L)
  print(current, row.names = FALSE)
}

print_available <- function(available) {
  cat("available:\n")
  available[] <- lapply(available, round, digits = 1L)
  print(available, row.names = FALSE)
}

apply_measures <- function(current, measures_to_apply) {

  append <- function(List, element) {
    List[[length(List) + 1L]] <- element
    List
  }
  
  history_current <- list(current)
  history_available <- list()
  history_measure <- list()
  
  for (measure in measures_to_apply) {
    
    #measure <- measures_to_apply[[1L]]
    
    # what areas are available for further measures?
    available <- get_available_m2(current)
    history_available <- append(history_available, available)
    
    print_available(available)
    
    if (measure$area > available[[measure$name]]) {
      stop(sprintf(
        "Not enough space available for measure '%s' (only %0.1f m2 allowed).",
        measure$name, 
        available[[measure$name]]
      ))
    } else {
      history_measure <- append(history_measure, as.data.frame(measure))
      
      current <- apply_measure(current, measure)
      history_current <- append(history_current, current)
      
      cat(sprintf(
        "\n### Measure '%s' (%0.2f m2) applied.\n\n", 
        measure$name, 
        measure$area
      ))
      print_current_state(current)
    }
  }
  
  cat("\n")
  
  # what areas are available for further measures?
  print_available(get_available_m2(current))
  
  list(
    current = do.call(rbind, history_current),
    available = do.call(rbind, history_available),
    measure = do.call(rbind, history_measure)
  )
}

row_diff <- function(data) {
  as.data.frame(lapply(data, diff))
}
