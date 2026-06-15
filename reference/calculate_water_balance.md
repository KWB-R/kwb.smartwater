# Calculate Water Balance for Given Block Areas and Measures

Calculate Water Balance for Given Block Areas and Measures

## Usage

``` r
calculate_water_balance(blocks, measures, convert_types = FALSE)
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

- convert_types:

  logical value indicating whether or not to convert the data types in
  the `blocks` data frame as required by R-ABIMO.
