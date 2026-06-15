#* kwb.smartwater API (plumber2)
#*
#* API for accessing the R-package kwb.smartwater
#*
#* @version 0.0.1
"_API"

#* Get one sample block area
#* 
#* This endpoint returns a sample list of block areas with exactly one block 
#* area. The block area is defined by exactly the same fields that were used in 
#* the AMAREX project.
#* 
#* @get /get_test_block
#* @serializer json
function()
{
  tryCatch(
    expr = kwb.smartwater::get_test_block(),
    error = function(e) {
      reqres::abort_internal_error(e$message)
    }
  )
}

#* A post endpoint
#*
#* @body test:integer an integer
#* @body test2:[string] an array of strings
#*
#* @post /hello/
#*
#* @parser json
#* @parser yaml
#*
function(body) {
  body
}

#* POST with 1 body parameter (does not work)
#* @body :{param1:array-of-blocks,param2:array-of-measures} The body
#* @post /post1
#* @parser json
function(body) {
  # @body param1:integer description of param1
  # @body param2:string description of param2
  body
}

#* POST with 2 body parameters
#* @body p1 parameter 1
#* @body p2 parameter 2
#* @post /post2
function(body) {
  body
}

#* POST with 3 body parameters
#* @body p1 parameter 1
#* @body p2 parameter 2
#* @body p3 parameter 3
#* @post /post3
function(body) {
  body
}

#* Convert block area to partial areas (m2)
#*
#* Convert block area with most of the fields representing area percentages
#* to an object with information on the partial areas (given in m2) being 
#* attributed to the different surface types and to each of the measures.
#*
#* @body p1 parameter 1
#* @body p2 parameter 2
#* @body p3 parameter 3
#* @post /rabimo_block_to_partial_areas_m2
#* @parser json
function(body) {
  # @body parameter_2:no-type One R-ABIMO-Block, as e.g. returned by get_test_block  
  # @body test2:[string] an array of strings
  
  body
}

#* Convert block area to partial areas 2 (m2)
#* 
#* Convert block area with most of the fields representing area percentages
#* to an object with information on the partial areas (given in m2) being 
#* attributed to the different surface types and to each of the measures.
#*
#* @body parameter_1:my-specific-data-type a data frame
#* @body parameter_2:no-type One R-ABIMO-Block, as e.g. returned by get_test_block
#* @post /hello3/
#* @parser json
#* 
function(body) {
  # @body myblock:integer One R-ABIMO-Block, as e.g. returned by get_test_block
  body
  # tryCatch(
  #   expr = kwb.smartwater::rabimo_block_to_partial_areas_m2(body$block),
  #   error = function(e) {
  #     reqres::abort_internal_error(e$message)
  #   }
  # )
}
