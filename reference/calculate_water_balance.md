# Calculate Water Balance for Given Block Areas and Measures

Calculate Water Balance for Given Block Areas and Measures

## Usage

``` r
calculate_water_balance(
  blocks,
  measures,
  parameters = NULL,
  convert_types = FALSE,
  max_veg_class = 80
)
```

## Arguments

- blocks:

  data.frame as returned by
  [`get_test_blocks`](https://kwb-r.github.io/kwb.smartwater/reference/get_test_blocks.md)

- measures:

  data.frame containing information about the planned measures in m2.
  Each row refers to a block area, linked by the text field `code`.
  There is one numeric field per measure. The names of the
  measure-related fields must correspond to the `field_name`s returned
  by
  [`get_measure_info`](https://kwb-r.github.io/kwb.smartwater/reference/get_measure_info.md).

- parameters:

  optional. List of parameters for each measure for which parameter
  values shall be overridden. Its format should refer to the format of
  the list returned by
  [`get_measure_info`](https://kwb-r.github.io/kwb.smartwater/reference/get_measure_info.md)`(parameters_only = TRUE)`.

- convert_types:

  logical value indicating whether or not to convert the data types in
  the `blocks` data frame as required by R-ABIMO.

- max_veg_class:

  maximum vegetation class value. When increasing the vegetation class
  index in order to consider tree measures, the resulting vegetation
  class index of a block will be limited to this value. The default is
  80.
