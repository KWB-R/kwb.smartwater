# Plot Effect of Disconnecting Surfaces from Sewer System

Plot Effect of Disconnecting Surfaces from Sewer System

## Usage

``` r
plot_effect_of_disconnect(
  surface_reduction,
  type,
  output_dir = NULL,
  width_factor = 10/6.789581
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

- width_factor:

  width factor. Default: 10/6.789581

## Value

path to created file (invisibly) if `output_dir` is not `NULL`,
otherwise `NULL` (invisibly)
