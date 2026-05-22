#* @apiTitle kwb.smartwater API
#* @apiDescription API for accessing the R-package kwb.smartwater

#* @get /get_test_block
#* Get one block (columns as expected by kwb.rabimo) for testing
function()
{
  to_plumber_response(try({
    kwb.smartwater::get_test_block()
  }))
}

#* Convert R-ABIMO-block to partial areas given in m2
#* @param block:data.frame One R-ABIMO-Block, as e.g. returned by /get_test_block
#* @post /rabimo_block_to_partial_areas_m2
function(block)
{
  to_plumber_response(try({
    kwb.smartwater::rabimo_block_to_partial_areas_m2(block)
  }))
}

#* @get /get_test_partial_areas
#* Get a test "state" of partial areas (in m2)
#* Returns the same as /rabimo_block_to_partial_areas_m2 when being given the response of /get_test_block
function()
{
  to_plumber_response(try({
    block <- kwb.smartwater::get_test_block()
    kwb.smartwater::rabimo_block_to_partial_areas_m2(block)
  }))
}

#* @post /get_available_m2
#* @param areas:data.frame Partial areas in m2, as e.g. returned by /get_test_partial_areas
#* Get areas in m2 that are available for further measures
#* New measures must not exceed these values
function(areas)
{
  to_plumber_response(try({
    kwb.smartwater::get_available_m2(areas)
  }))
}

#* @post /apply_measure
#* @param areas:data.frame Partial areas in m2, as e.g. returned by /get_test_partial_areas
#* @param measure_name method name (one of those returned by /get_measure_names)
#* @param measure_area:numeric area associated to the measure, given in m2
#* Apply a measure to a "state" of areas and return the updated state
function(areas, measure_name, measure_area)
{
  measure <- list(
    name = measure_name, 
    area = as.numeric(measure_area)
  )
  to_plumber_response(try({
    kwb.smartwater::apply_measure(areas, measure)
  }))
}

#* @get /get_test_measures
#* Get a test "sequence" of measures
function()
{
  to_plumber_response(try({
    kwb.smartwater::get_test_measures()
  }))
}

#* @get /get_measure_names
#* Get names of measures supported by kwb.smartwater
function()
{
  to_plumber_response(try({
    kwb.smartwater::get_measure_names()
  }))
}

# /plot_effect_of_disconnect ---------------------------------------------------

#* Plot Effect of Disconnecting Surfaces
#* @get /plot_effect_of_disconnect
#* @param surface_reduction surface_reduction in percent
#* @param type one of "critical_hours", "unpleasant_hours", "critical_events", "negative_deviation"
#* @serializer contentType list(type="image/png")
function(surface_reduction, type)
{
  file <- kwb.smartwater:::plot_effect_of_disconnect(
    surface_reduction = as.numeric(surface_reduction), 
    type = type, 
    output_dir = tempdir()
  )
  readBin(file, "raw", n = file.info(file)$size)
}
