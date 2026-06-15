#' Calculate Water Balance for Given Block Areas and Measures

#' @param blocks data.frame as returned by \code{\link{get_test_blocks}}
#' @param measures data.frame containing information about the planned measures
#'   in m2. Each row refers to a block area, linked by the text field
#'   \code{code}. There is one numeric field per measure. The names of the
#'   measure-related fields must correspond to the \code{field_name}s returned 
#'   by \code{\link{get_measure_info}}.
#' @param convert_types logical value indicating whether or not to convert the
#'   data types in the \code{blocks} data frame as required by R-ABIMO.
#' @export
calculate_water_balance <- function(blocks, measures, convert_types = FALSE) {
  
  # kwb.rabimo is strict about data types. Therefore, convert data types as
  # necessary
  if (convert_types) {
    blocks <- kwb.rabimo:::check_or_convert_data_types(
      data = blocks, 
      types = kwb.rabimo:::get_expected_data_type(names(blocks)), 
      convert = TRUE
    )
  }
  
  # TODO: modify the blocks according to the measures
  config <- kwb.rabimo:::reconfigure(kwb.rabimo::rabimo_inputs_2025$config)
  kwb.rabimo::run_rabimo(blocks, config)
}
