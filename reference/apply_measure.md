# Apply a measure to the current state of area assignments

Apply a measure to the current state of area assignments

## Usage

``` r
apply_measure(areas, measure)
```

## Arguments

- areas:

  one-row data frame with each column representing a partial area of the
  total block area, in m2, as e.g. returned by
  [`rabimo_block_to_partial_areas_m2`](https://kwb-r.github.io/kwb.smartwater/reference/rabimo_block_to_partial_areas_m2.md)

- measure:

  list with elements "name" (name of measurement) and area (area in m2
  that is assigned to the measure)
