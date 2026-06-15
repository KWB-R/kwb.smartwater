#* @apiTitle kwb.smartwater API (plumber)
#* @apiDescription API for accessing the R-package kwb.smartwater

TEST_CODES <- c("1100541241000000", "1400761421000000")

#* @get /get_measure_info
#* Get info on measures supported by kwb.smartwater
#' 
#' Get information on the rainwater management measures supported by kwb.smartwater
#' @param type:[chr] optional. Vector of character indicating the method types ("green_roof", "pavement", "trees", "infiltration", "retention") for which to filter the output.
#' @param field_name_only:logical optional. Logical of length one indicating whether or not to return only the "field_name" instead of all info fields per measure
#* @serializer unboxedJSON
function(type = character(0), field_name_only = FALSE)
{
  to_plumber_response(try({
    kwb.smartwater::get_measure_info(type, field_name_only)
  }))
}

#* @post /calculate_water_balance
#* Run R-Abimo for given block areas and measures
#* Run R-Abimo for a given set of block areas and a given set of corresponding measures.
#* @param blocks:data.frame Array of block areas, as e.g. returned by /get_test_blocks
#* @param measures:data.frame Array of objects containing information about the planned measures in m2. Each object has a text field "code" that identifies the block area to which the measures relate. All other fields are numeric and relate to a measure type. See /get_test_block_measures for an example object and for the expected measure names.
function(
    blocks = kwb.smartwater::get_test_blocks(), 
    measures = kwb.smartwater::get_test_block_measures()
)
{
  to_plumber_response(try({
    kwb.smartwater::calculate_water_balance(
      blocks, measure_related_areas, convert_types = TRUE
    )
  }))
}

#* @get /get_test_blocks
#* Get block areas, for testing
#* Get information on test block areas, with all the fields that are expected by R-ABIMO.
#* @param codes:[chr] codes to be selected from the Berlin R-ABIMO dataset
#* @serializer json
function(codes = TEST_CODES)
{
  to_plumber_response(try({
    kwb.smartwater::get_test_blocks(codes)
  }))
}

#* @get /get_test_block_measures
#* Get test measures for test block areas
#* Get measure objects for the test block areas, as required by /calculate_water_balance
#* @param codes:[chr] codes to be selected from the Berlin R-ABIMO dataset
#* @serializer json
function(codes = TEST_CODES)
{
  to_plumber_response(try({
    kwb.smartwater::get_test_block_measures(codes)
  }))
}

#* @post /rabimo_block_to_partial_areas_m2
#* Convert R-ABIMO-block to partial areas given in m2
#* @param block:data.frame One R-ABIMO-Block, as e.g. returned by /get_test_blocks
function(block = kwb.smartwater::get_test_blocks()[1, ])
{
  to_plumber_response(try({
    kwb.smartwater::rabimo_block_to_partial_areas_m2(block)
  }))
}

get_test_partial_areas <- function() {
  kwb.smartwater::rabimo_block_to_partial_areas_m2(
    block = kwb.smartwater::get_test_blocks()[1, ]
  )
}

#* @get /get_test_partial_areas
#* Get a test "state" of partial areas (in m2)
#* Returns the same as /rabimo_block_to_partial_areas_m2 when being given the response of /get_test_blocks for one block only
function()
{
  to_plumber_response(try(get_test_partial_areas()))
}

#* @post /get_available_m2
#* Get areas in m2 that are available for further measures
#* New measures must not exceed these values
#* @param areas:data.frame Partial areas in m2, as e.g. returned by /get_test_partial_areas
function(areas = get_test_partial_areas())
{
  to_plumber_response(try({
    kwb.smartwater::get_available_m2(areas)
  }))
}

#* @post /apply_measure
#* Apply a measure to a "state" of areas and return the updated state
#* @param areas:data.frame Partial areas in m2, as e.g. returned by /get_test_partial_areas
#* @param measure_name:string measure name (one of the `field_name`s returned by /get_measure_info with `field_name_only` = true)
#* @param measure_area:numeric area associated to the measure, given in m2. For the default block in this example, the default value should result in a remaining paved area of 1000 m2.
function(
    measure_name = "unpaving", 
    measure_area = 399.0427, # to achieve a remaining paved area of 1000 m2
    areas = get_test_partial_areas()
)
{
  measure <- list(
    name = measure_name, 
    area = as.numeric(measure_area)
  )
  to_plumber_response(try({
    kwb.smartwater::apply_measure(areas, measure)
  }))
}

#* @get /plot_effect_of_disconnect
#* Plot Effect of Disconnecting Surfaces
#* @param surface_reduction surface_reduction in percent
#* @param type one of "critical_hours", "unpleasant_hours", "critical_events", "negative_deviation"
#* @serializer contentType list(type="image/png")
function(surface_reduction, type)
{
  file <- kwb.smartwater::plot_effect_of_disconnect(
    surface_reduction = as.numeric(surface_reduction), 
    type = type, 
    output_dir = tempdir()
  )
  readBin(file, "raw", n = file.info(file)$size)
}
