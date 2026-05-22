plot_effect_of_disconnect <- function(
    surface_reduction, 
    type, 
    output_dir = NULL, # "./inst/extdata/output", 
    width_factor = 10/6.789581
)
{
  # kwb.utils::assignPackageObjects("kwb.smartwater")
  # surface_reduction <- 10.5
  
  type <- match.arg(type, c(
    "critical_hours",    # Unterschreitungsdauer 1.5 mg/L
    "unpleasant_hours",  # Unterschreitungsdauer 3 mg/L
    "critical_events",   # Kritische Events
    "negative_deviation" # Negative Abweichung
  ))
  
  plot_fun_args <- get_plot_fun_args_for_surface_reduction(
    surface_reduction, 
    type = type, 
    width_factor = width_factor
  )
  
  if (!is.null(output_dir) && dir.exists(output_dir)) {
    plot_into_png_generic(
      filename = file.path(output_dir, paste0(surface_reduction, "_", type, ".png")), 
      png_args = list(
        width = 6 * width_factor, 
        height = 6 , 
        units = "in", 
        res = 600
      ),
      plot_fun = plot_rivers,
      plot_fun_args = plot_fun_args
    )
  } else {
    do.call(plot_rivers, plot_fun_args)
  }
}

#' @importFrom utils read.csv
get_plot_fun_args_for_surface_reduction <- function(
    surface_reduction,
    type,
    rivers = read_rivers(package_file("extdata/rivers.csv")),
    siteInfo = read_site_info(package_file("extdata/site-info.csv")),
    mappingTable = read.csv(package_file("extdata/mapping-table.csv")),
    districPolygons = readRDS(package_file("extdata/data/districPolygons")),
    waterPolygons = readRDS(package_file("extdata/data/waterPolygons")),
    xlim = c(13.18, 13.47),
    ylim = c(52.45, 52.57),
    xpdDim = 6,
    width_factor = 10/6.789581
) {
  type <- match.arg(type, allowed_types <- c(
    "critical_hours",
    "unpleasant_hours",
    "critical_events",
    "negative_deviation"
  ))
  
  models <- get_models()
  model_scaling_factor <- 1
  defModel <- defHourModel
  
  if (type == "critical_hours") {
    
    model <- models$critical_hour
    classBreaks <- c(0, 25, 50, 100, 200, 300, Inf)
    LegendTitle <- paste0(
      "Unterschreitungsdauer in Stunden (1,5 mg/L) bei ",
      sub(pattern = "\\.", replacement = ",", x = surface_reduction),
      "% Abkopplung"
    )
    
  } else if (type == "unpleasant_hours") {
    
    model <- models$unpleasant_hours
    classBreaks <- c(0, 50, 100, 200, 300, 500, Inf)
    LegendTitle <- paste0(
      "Unterschreitungsdauer in Stunden (3 mg/L) bei ",
      sub(pattern = "\\.", replacement = ",", x = surface_reduction),
      "% Abkopplung"
    )
    
  } else if (type == "critical_events") {
    
    model <- models$critical_events
    classBreaks <- c(-Inf, 0, 1, 3, 6, 10, Inf)
    LegendTitle <- paste0(
      "Kritische Sauerstoffereignisse bei ",
      sub(pattern = "\\.", replacement = ",", x = surface_reduction),
      "% Abkopplung"
    )
    defModel <- defEventModel
    
  } else if (type == "negative_deviation") {
    
    model <- models$negative_deviation
    model_scaling_factor <- 100
    classBreaks <- c(0, 2, 5, 10, 20, 30, Inf)
    LegendTitle <- paste0(
      "Negative Abwichung vom Referenzustand (in %) bei ",
      sub(pattern = "\\.", replacement = ",", x = surface_reduction),
      "% Abkopplung"
    )
    
  } else {
    
    stop("type must be one of ", paste(allowed_types, collapse = ", "))
  }
  
  list(
    ext_rivers = value_to_classes(
      river_list = prepare_rivers(
        rivers, 
        df_in = cbind(
          "value" = sapply(
            model, 
            FUN = defModel, 
            surface_reduction = surface_reduction
          ) * model_scaling_factor,
          siteInfo
        ), 
        mappingTable = mappingTable
      ),
      classBreaks = classBreaks,
      colorVector = MisaColor
    ), 
    LegendTitle = LegendTitle, 
    xlim = xlim,
    ylim = ylim,
    districPolygons = districPolygons, 
    waterPolygons = waterPolygons,
    xpdDim = xpdDim, 
    width_factor = width_factor
  )
}
