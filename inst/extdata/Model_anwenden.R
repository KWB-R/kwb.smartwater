surface_reduction <- 10.5 # Flaechenabkopplung

output_dir <- "./inst/extdata/output"
#output_dir <- NULL

# Unterschreitungsdauer 1.5 mg/L -----------------------------------------------
kwb.smartwater:::plot_effect_of_disconnect(
  surface_reduction = surface_reduction, 
  type = "critical_hours", 
  output_dir = output_dir
)

# Unterschreitungsdauer 3 mg/L -------------------------------------------------
kwb.smartwater:::plot_effect_of_disconnect(
  surface_reduction = surface_reduction, 
  type = "unpleasant_hours", 
  output_dir = output_dir
)

# Kritische Events -------------------------------------------------------------
kwb.smartwater:::plot_effect_of_disconnect(
  surface_reduction, 
  type = "critical_events", 
  output_dir = output_dir
)

# Negative Abweichung ----------------------------------------------------------
kwb.smartwater:::plot_effect_of_disconnect(
  surface_reduction, 
  type = "negative_deviation", 
  output_dir = output_dir
)
