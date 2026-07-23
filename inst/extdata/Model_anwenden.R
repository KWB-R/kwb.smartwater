surface_reduction <- 10.5 # Flaechenabkopplung

output_dir <- "./inst/extdata/output"
#output_dir <- NULL

xpdDim <- 6
width_factor <- 10/6.789581
margins <- c(xpdDim / 2, 0.2, xpdDim / 2 , xpdDim * width_factor - 0.2)
png_args = list(width = 6 * width_factor, height = 6 , units = "in", res = 600)

# Unterschreitungsdauer 1.5 mg/L -----------------------------------------------
kwb.smartwater::plot_effect_of_disconnect(
  surface_reduction = surface_reduction, 
  type = "critical_hours", 
  output_dir = output_dir,
  margins = margins,
  png_args = png_args
)

# Unterschreitungsdauer 3 mg/L -------------------------------------------------
kwb.smartwater::plot_effect_of_disconnect(
  surface_reduction = surface_reduction, 
  type = "unpleasant_hours", 
  output_dir = output_dir,
  margins = margins,
  png_args = png_args
)

# Kritische Events -------------------------------------------------------------
kwb.smartwater::plot_effect_of_disconnect(
  surface_reduction, 
  type = "critical_events", 
  output_dir = output_dir,
  margins = margins,
  png_args = png_args
)

# Negative Abweichung ----------------------------------------------------------
kwb.smartwater::plot_effect_of_disconnect(
  surface_reduction, 
  type = "negative_deviation", 
  output_dir = output_dir,
  margins = margins,
  png_args = png_args
)
