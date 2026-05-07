#* @get /get_test_block
#* Get one block (columns as expected by kwb.rabimo) for testing
function()
{
  to_plumber_response(try({
    kwb.smartwater::get_test_block()
  }))
}

#* @post /rabimo_block_to_partial_areas_m2
#* @param block:data.frame One R-ABIMO-Block, as e.g. returned by /get_test_block
#* Convert R-ABIMO-block to partial areas given in m2
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
#* @param measure:list object with elements "name" and "area" (given im m2), a list of which is e.g. returned by /get_test_measures
#* Apply a measure to a "state" of areas and return the updated state
function(areas, measure)
{
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
