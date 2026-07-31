# Vegetation Class Increment Caused by Planting Trees

Vegetation Class Increment Caused by Planting Trees

## Usage

``` r
get_veg_class_increment(tree_measure_volume, unsealed_area_m2, m = 4.7)
```

## Arguments

- tree_measure_volume:

  assumed green volume of trees in m3.

- unsealed_area_m2:

  unpaved area (= total_area \* (1 - roof - pvd)) in m2

- m:

  slope of the linear relation between normalised vegetation volume per
  unsealed area (vegnorm) and veg_scaled. Default: 4.7, calculated from
  block with highest vegetation class in Berlin (code =
  0000000012002198): m = veg_scaled/vegnorm = 118.1306/24.92435
