get_tree_measure_volume <- function(block_measures, parameters = NULL) {
  
  stopifnot(is.data.frame(block_measures), 
            nrow(block_measures) == 1L)
  
  default_tree_parameters <- get_measure_info("trees", parameters_only = TRUE)
  
  stopifnot(all(names(default_tree_parameters) %in% names(block_measures)))
  
  fields_trees <- get_measure_info("trees", TRUE)
  
  sum(sapply(fields_trees, function(field) {
    
    # field <- fields_trees[1]
    
    n_trees <- block_measures[[field]]
    volume_per_tree <- if (!is.null(parameters) && length(parameters[[field]])) {
      parameters[[field]]$tree_volume
    } else {
      default_tree_parameters[[field]]$tree_volume
    }
    n_trees * volume_per_tree
  }))
}


#' Vegetation Class Increment Caused by Planting Trees
#' 
#' @param unsealed_area_m2 unpaved area (= total_area * (1 - roof - pvd)) in m2
#' @param m slope of the linear relation between normalised vegetation volume 
#'   per unsealed area (vegnorm) and veg_scaled. Default: 4.7, calculated from
#'   block with highest vegetation class in Berlin (code = 0000000012002198): 
#'   m = veg_scaled/vegnorm = 118.1306/24.92435
#' @param tree_measure_volume assumed green volume of trees in m3.
get_veg_class_increment <- function(
    tree_measure_volume,
    unsealed_area_m2, 
    m = 4.7
)
{
  if (unsealed_area_m2 == 0) {
    return(0)
  }
  m * tree_measure_volume / unsealed_area_m2 
}

