# Test the plumber API

Test the plumber API

## Usage

``` r
run_plumber_api(catch_errors = FALSE, use_plumber2 = FALSE)
```

## Arguments

- catch_errors:

  whether or not to catch errors. If TRUE, errors are caught and the
  result object is a list with elements "data" (containing the function
  result or NULL in case of an error) and "error" (containing the error
  message in case of an error and NULL otherwise)

- use_plumber2:

  if `TRUE`, the plumber2 package is used, otherwise the plumber package
