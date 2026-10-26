# Alternative Version of get_veg_class_increment()

Alternative Version of get_veg_class_increment()

## Usage

``` r
get_veg_class_increment_2(
  tree_measure_volume,
  unsealed_area_m2,
  veg_class,
  max_vegetation_class = MAX_VEGETATION_CLASS,
  exponent = EXPONENT,
  source_max = SOURCE_MAX,
  target_max = TARGET_MAX
)
```

## Arguments

- tree_measure_volume:

  added green volume (m3)

- unsealed_area_m2:

  unsealed area (m2)

- veg_class:

  current vegetation class

- max_vegetation_class:

  Default: 75

- exponent:

  Default: 1.23

- source_max:

  maximum veg_norm value. Default: 28.3 (found in Berlin data)

- target_max:

  maximum veg_class value. Default: 55 (found in Berlin data, for
  non-forested areas)
