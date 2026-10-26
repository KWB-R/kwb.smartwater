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

recalculate_vegetation_class <- function(
    old_veg_class, 
    tree_measure_volume, 
    unsealed_area_m2,
    max_veg_class,
    use_linear_relation = TRUE
) {
  
  if (use_linear_relation) {
    increment <- get_veg_class_increment(tree_measure_volume, unsealed_area_m2)
    new_veg_class <- old_veg_class + increment
    return(min(new_veg_class, max_veg_class))
  }
  
  old_veg_class + get_veg_class_increment_2(
    tree_measure_volume = tree_measure_volume, 
    unsealed_area_m2 = unsealed_area_m2, 
    veg_class = old_veg_class,
    max_vegetation_class = max_veg_class
  )
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

MAX_VEGETATION_CLASS <- 75
EXPONENT <- 1.23
SOURCE_MAX <- 28.3
TARGET_MAX <- 55

#' Alternative Version of get_veg_class_increment()
#' 
#' @param tree_measure_volume added green volume (m3)
#' @param unsealed_area_m2 unsealed area (m2)
#' @param veg_class current vegetation class
#' @param max_vegetation_class Default: 75
#' @param exponent Default: 1.23
#' @param source_max maximum veg_norm value. Default: 28.3 (found in Berlin data)
#' @param target_max maximum veg_class value. Default: 55 (found in Berlin data,
#'   for non-forested areas)
get_veg_class_increment_2 <- function(
    tree_measure_volume, 
    unsealed_area_m2, 
    veg_class,
    max_vegetation_class = MAX_VEGETATION_CLASS,
    exponent = EXPONENT,
    source_max = SOURCE_MAX,
    target_max = TARGET_MAX
) {
  veg_norm_old <- veg_class_to_veg_norm(
    veg_class, max_vegetation_class, exponent, source_max, target_max
  )
  
  green_volume_old <- veg_norm_old * unsealed_area_m2
  green_volume_new <- green_volume_old + tree_measure_volume
  veg_norm_new <- green_volume_new / unsealed_area_m2
  
  veg_norm_to_veg_class(
    veg_norm_new, max_vegetation_class, exponent, source_max, target_max
  ) - veg_class
}

# convert veg_class to veg_norm
veg_class_to_veg_norm <- function(
    veg_class, 
    max_vegetation_class = MAX_VEGETATION_CLASS,
    exponent = EXPONENT,
    source_max = SOURCE_MAX,
    target_max = TARGET_MAX
) {
  stopifnot(all(veg_class <= max_vegetation_class))
  veg_class^(1/exponent) / target_max * source_max
}

# convert veg_norm to veg_class
veg_norm_to_veg_class <- function(
    veg_norm, 
    max_vegetation_class = MAX_VEGETATION_CLASS,
    exponent = EXPONENT,
    source_max = SOURCE_MAX,
    target_max = TARGET_MAX
) {
  rescaled <- veg_norm / source_max * target_max
  pmin(rescaled^exponent, max_vegetation_class)
}
