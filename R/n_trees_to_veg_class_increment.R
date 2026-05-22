#' Vegetation Class Increment Caused by Planting Trees
#' 
#' @param n_trees number of trees to be planted
#' @param unpaved_area_m2 unpaved area (= total_area * (1 - roof - pvd)) in m2
#' @param m slope of the linear relation between normalised vegetation volume 
#'   per unpaved area (vegnorm) and veg_scaled. Default: 4.7, calculated from
#'   block with highest vegetation class in Berlin (code = 0000000012002198): 
#'   m = veg_scaled/vegnorm = 118.1306/24.92435
#' @param volume_per_tree_m3 assumed green volume per tree in m3. Default: 400
#'   (440 m3 determined for Bayerischer Platz, Berlin, rounded down to nearest
#'   multiple of 100)
n_trees_to_veg_class_increment <- function(
    n_trees, 
    unpaved_area_m2, 
    m = 4.7,
    volume_per_tree_m3 = 400
)
{
  m * n_trees * volume_per_tree_m3 / unpaved_area_m2
}
