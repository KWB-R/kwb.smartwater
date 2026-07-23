# Plot Effect of Disconnecting Surfaces from Sewer System

Plot Effect of Disconnecting Surfaces from Sewer System

## Usage

``` r
plot_effect_of_disconnect(
  surface_reduction,
  type,
  output_dir = NULL,
  png_args = list(width = 9, height = 6, units = "in", res = 600),
  margins = c(1, 1, 3, 1)
)
```

## Arguments

- surface_reduction:

  reduction of connected surface in percent

- type:

  one of "critical_hours", "unpleasant_hours", "critical_events",
  "negative_deviation"

- output_dir:

  if `NULL` (the default), the plot goes into the active device,
  otherwise the plot is written to a png file within `output_dir` (must
  be an existing directory)

- png_args:

  list of arguments passed to
  [`png`](https://rdrr.io/r/grDevices/png.html). Default:
  `list(width = 6 * width_factor, height = 6, units = "in", res = 600)`

- margins:

  numerical vector of the form c(bottom, left, top, right) which gives
  the number of lines of margin to be specified on the four sides of the
  plot. The default is `c(1, 1, 3 , 1)`.

## Value

path to created file (invisibly) if `output_dir` is not `NULL`,
otherwise `NULL` (invisibly)
