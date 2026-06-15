# Get info on the measures supported by kwb.smartwater

Get information on the rainwater management measures supported by
kwb.smartwater

## Usage

``` r
get_measure_info(type = character(0), field_name_only = FALSE)
```

## Arguments

- type:

  optional. Vector of character indicating the method types
  ("green_roof", "pavement", "trees", "infiltration", "retention") for
  which to filter the output.

- field_name_only:

  optional. Logical of length one indicating whether or not to return
  only the "field_name" instead of all info fields per measure
