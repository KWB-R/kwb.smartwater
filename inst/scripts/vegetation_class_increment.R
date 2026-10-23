#
# How to model the planting of trees?
#
# By adding green volume and by converting the green volume delta into a delta
# of the vegetation class
#

EXPONENT <- 1.23
MAX_VEGETATION_CLASS <- 75

# maximum veg_norm value found in Berlin data
SOURCE_MAX <- 28.32298

# maximum veg_class value found in Berlin data (for non-forested areas)
TARGET_MAX <- 55

# convert veg_norm to veg_class
veg_norm_to_veg_class <- function(veg_norm) {
  rescaled <- veg_norm / SOURCE_MAX * TARGET_MAX
  pmin(rescaled^EXPONENT , MAX_VEGETATION_CLASS)
}

# convert veg_class to veg_norm
veg_class_to_veg_norm <- function(veg_class) {
  stopifnot(all(veg_class <= MAX_VEGETATION_CLASS))
  veg_class^(1/EXPONENT) / TARGET_MAX * SOURCE_MAX
}

# alternative version of kwb.smartwater:::get_veg_class_increment()
get_veg_class_increment_2 <- function(
    tree_measure_volume, 
    unsealed_area_m2, 
    veg_class
) {
  veg_norm_old <- veg_class_to_veg_norm(veg_class)
  green_volume_old <- veg_norm_old * unsealed_area_m2
  green_volume_new <- green_volume_old + tree_measure_volume
  veg_norm_new <- green_volume_new / unsealed_area_m2
  veg_norm_to_veg_class(veg_norm_new) - veg_class
}

veg_norm <- 0:30
veg_class <- veg_norm_to_veg_class(veg_norm)

plot(veg_norm, veg_class)
plot(veg_norm, veg_class_to_veg_norm(veg_class))

# Create one block for testing
block <- kwb.rabimo::generate_rabimo_area("testblock")

# unsealed area within the block
unsealed_area_m2 <- with(block, (1 - roof - pvd) * total_area)

# add a small tree
get_veg_class_increment_2(
  tree_measure_volume = 150,
  unsealed_area_m2 = unsealed_area_m2,
  veg_class = block$veg_class
)

kwb.smartwater:::get_veg_class_increment(
  tree_measure_volume = 150,
  unsealed_area_m2 = unsealed_area_m2
)

# add a large tree
get_veg_class_increment_2(
  tree_measure_volume = 2300,
  unsealed_area_m2 = unsealed_area_m2,
  veg_class = block$veg_class
)

kwb.smartwater:::get_veg_class_increment(
  tree_measure_volume = 2300,
  unsealed_area_m2 = unsealed_area_m2
)

# Note: increments leading to veg_class values above MAX_VEGETATION_CLASS will 
# be limited by the caller of get_veg_class_increment()
