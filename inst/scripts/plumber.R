#* @apiTitle kwb.smartwater API
#* @apiDescription API for accessing the R-package kwb.smartwater

################### plumber2 examples ###################

#* Echo the parameter that was sent in
#*
#* @get /echo/<msg>
#*
#* @param msg:string The message to echo back.
#*
function(msg) {
  list(
    msg = paste0("The message is: '", msg, "'")
  )
}

#* Plot out data from the palmer penguins dataset
#*
#* @get /plot
#*
#* @query spec:string If provided, filter the data to only this species
#* (e.g. 'Adelie')
#*
#* @serializer png
#*
function(query) {
  myData <- penguins
  title <- "All Species"
  
  # Filter if the species was specified
  if (!is.null(query$spec)){
    title <- paste0("Only the '", query$spec, "' Species")
    myData <- subset(myData, species == query$spec)
  }
  
  plot(
    myData$flipper_len,
    myData$bill_len,
    main=title,
    xlab="Flipper Length (mm)",
    ylab="Bill Length (mm)"
  )
}

################### plumber ####################

#* @get /get_test_block
#* @serializer json list(always_decimal = TRUE)
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
  file <- kwb.smartwater::plot_effect_of_disconnect(
    surface_reduction = as.numeric(surface_reduction), 
    type = type, 
    output_dir = tempdir()
  )
  readBin(file, "raw", n = file.info(file)$size)
}

#* @post /calculate_water_balance
#* @param blocks:data.frame Array of block areas, each of which looks like the object returned by /get_test_block
#* @param measures:data.frame Array of objects containing information about the planned measures in m2. Each object has a text field "code" (matching the code of the corresponding block area) and one numeric field per measure. The names of the measure-related field must correspond to the names returned by /get_measure_names.
#* Run R-Abimo for a given set of block areas and a given set of corresponding measures.
function(blocks, measures)
{
  to_plumber_response(try({
    kwb.smartwater::calculate_water_balance(blocks, measure_related_areas)
  }))
}
