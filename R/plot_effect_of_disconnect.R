#' Plot Effect of Disconnecting Surfaces from Sewer System
#' 
#' @param surface_reduction reduction of connected surface in percent
#' @param type one of "critical_hours", "unpleasant_hours", "critical_events",
#'   "negative_deviation"
#' @param output_dir if \code{NULL} (the default), the plot goes into the active
#'   device, otherwise the plot is written to a png file within 
#'   \code{output_dir} (must be an existing directory)
#' @param width_factor width factor. Default: 10/6.789581
#' @return path to created file (invisibly) if \code{output_dir} is not 
#'   \code{NULL}, otherwise \code{NULL} (invisibly)
#' @export
plot_effect_of_disconnect <- function(
    surface_reduction, 
    type, 
    output_dir = NULL, # "./inst/extdata/output", 
    width_factor = 10/6.789581
)
{
  if (FALSE) 
  {
    kwb.utils::assignPackageObjects("kwb.smartwater")
    surface_reduction <- 10.5
    type <- "critical_hours"
    width_factor <- 10/6.789581
  }
  
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

  filename <- if (!is.null(output_dir) && dir.exists(output_dir)) {
    file.path(output_dir, paste0(surface_reduction, "_", type, ".png"))
  }
  
  if (!is.null(filename)) {
    filename <- file.path(output_dir, paste0(surface_reduction, "_", type, ".png"))
    plot_into_png_generic(
      filename = filename, 
      png_args = list(
        width = 6 * width_factor, 
        height = 6 , 
        units = "in", 
        res = 600
      ),
      plot_fun = plot_rivers,
      plot_fun_args = plot_fun_args$plot_args
    )
  } else {
    do.call(plot_rivers, plot_fun_args$plot_args)
  }
  
  invisible(filename)
  return(plot_fun_args$additional_values)
}

#' @importFrom utils read.csv
#' @importFrom stats median
get_plot_fun_args_for_surface_reduction <- function(
    surface_reduction,
    type,
    rivers = read_rivers(package_file("extdata/rivers.csv")),
    siteInfo = read_site_info(package_file("extdata/site-info.csv")),
    mappingTable = read.csv(package_file("extdata/mapping-table.csv")),
    districPolygons = read_district_polygons(),
    waterPolygons = read_water_polygons(),
    xlim = c(13.18, 13.47),
    ylim = c(52.45, 52.57),
    xpdDim = 6,
    width_factor = 10/6.789581
) {
  
  allowed_types <- c(
    "critical_hours",
    "unpleasant_hours",
    "critical_events",
    "negative_deviation"
  )
  
  type <- match.arg(type, allowed_types)
  
  models <- get_models()
  
  add_context_to_title <- function(title) {
    sprintf(
      "%s bei %s %% Abkopplung",
      title, format(surface_reduction, decimal.mark = ",")
    )
  }
  
  model_specs <- list(
    critical_hours = list(
      model = models$critical_hour,
      scaling_factor = 1,
      defModel = defHourModel,
      breaks = c(0, 25, 50, 100, 200, 300, Inf),
      title = "Unterschreitungsdauer in Stunden (1,5 mg/L)"
    ),
    unpleasant_hours = list(
      model = models$unpleasant_hours,
      scaling_factor = 1,
      defModel = defHourModel,
      breaks = c(0, 50, 100, 200, 300, 500, Inf),
      title = "Unterschreitungsdauer in Stunden (3 mg/L)"
    ),
    critical_events = list(
      model = models$critical_events,
      scaling_factor = 1,
      defModel = defEventModel,
      breaks = c(-Inf, 0, 1, 3, 6, 10, Inf),
      title = "Kritische Sauerstoffereignisse"
    ),
    negative_deviation = list(
      model = models$negative_deviation,
      scaling_factor = 100,
      defModel = defHourModel,
      breaks = c(0, 2, 5, 10, 20, 30, Inf),
      title = "Negative Abwichung vom Referenzustand (in %)"
    )
  )

  model_spec <- model_specs[[type]]
  overflowVolume_miom3 <- exp(surface_reduction * -0.044 + 1.55)
  
  ext_rivers <- value_to_classes(
    river_list = prepare_rivers(
      rivers, 
      df_in = cbind(
        "value" = sapply(
          model_spec[["model"]], 
          FUN = model_spec[["defModel"]], 
          surface_reduction = surface_reduction
        ) * model_spec[["scaling_factor"]],
        siteInfo
      ), 
      mappingTable = mappingTable
    ),
    classBreaks = model_spec[["breaks"]],
    colorVector = MisaColor
  )
  
  v <- unlist(lapply(ext_rivers, function(x){x$data$value}))
  berlinWide <- 
    if(grepl(pattern = "hours", x = type) | type == "negative_deviation"){
      stats::median(v, na.rm = TRUE)
    } else if(type == "critical_events"){
      d <- unlist(lapply(ext_rivers, function(x){x$data$distance_to_neighbour}))
      mean(v * d  / sum(d), na.rm = TRUE) * length(d)
    }
  
  list(
    plot_args = list(
      ext_rivers = ext_rivers, 
      LegendTitle = add_context_to_title(model_spec[["title"]]), 
      xlim = xlim,
      ylim = ylim,
      districPolygons = districPolygons, 
      waterPolygons = waterPolygons,
      xpdDim = xpdDim, 
      width_factor = width_factor), 
    additional_values = list(
      overflowVolume = overflowVolume_miom3,
      berlinWide = berlinWide
    )
  )
}
