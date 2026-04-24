sf <- 10.5 # Flaechenabkopplung

{
  models <- kwb.smartwater:::get_models()
  siteInfo <- kwb.smartwater:::read_site_info("./inst/extdata/site-info.csv")
  mappingTable <- read.csv("./inst/extdata/mapping-table.csv")
  
  path <- "./inst/extdata"
  rivers <- readRDS(file = file.path(path, "data", "rivers"))
  rivers2 <- kwb.smartwater:::read_rivers(file = "./inst/extdata/rivers.csv")
  arrange_cols <- function(x, element = NULL) {
    if (is.null(element)) {
      x <- x[sort(names(x))]  
    } else {
      x[[element]] <- x[[element]][sort(names(x[[element]]))]
    }
    x
  }
  stopifnot(all.equal(lapply(rivers, arrange_cols), lapply(rivers2, arrange_cols)))
  
  waterPolygons <- readRDS(file.path(path, "data", "waterPolygons"))
  districPolygons <- readRDS(file.path(path, "data", "districPolygons"))
  berlinPolygon <- readRDS(file.path(path, "data", "berlinPolygon"))
}

# Set limits -------------------------------------------------------------------
xlim <- c(13.18, 13.47)
ylim <- c(52.45, 52.57)
# plotDim <- getDimensions(xlim = xlim, ylim = ylim, width = 10)
plotDim <- c(10, 6.789581)
width_factor <- plotDim[1]/plotDim[2]

# Unterschreitungsdauer 1.5 mg/L -----------------------------------------------
{
  df_in <- cbind(
    "value" = sapply(
      models$critical_hours, 
      FUN = kwb.smartwater:::defHourModel, 
      surface_reduction = sf
    ),
    siteInfo
  )
  rivers_p <- kwb.smartwater:::prepare_rivers(rivers, df_in = df_in, mappingTable = mappingTable)
  rivers_p_2 <- kwb.smartwater:::prepare_rivers(rivers2, df_in = df_in, mappingTable = mappingTable)
  stopifnot(all.equal(lapply(rivers_p, arrange_cols, "data"), lapply(rivers_p_2, arrange_cols, "data")))
  kwb.smartwater:::plot_into_png(
    fn = file.path(path, "output", paste0(sf, "_critical_hours.png")),
    width_factor = plotDim[1]/plotDim[2], 
    xpdDim = 6, 
    xlim = xlim, 
    ylim = ylim, 
    rivers_p2 = kwb.smartwater:::value_to_classes(
      river_list = rivers_p,
      classBreaks = c(0, 25, 50, 100, 200, 300, Inf),
      colorVector = kwb.smartwater:::MisaColor
    ), 
    LegendTitle = paste0(
      "Unterschreitunsdauer in Stunden (1,5 mg/L) bei ",
      sub(pattern = "\\.", replacement = ",", x = sf),
      "% Abkopplung"
    ), 
    districPolygons = districPolygons, 
    waterPolygons = waterPolygons
  )
}

# Unterschreitungsdauer 3 mg/L -------------------------------------------------
{
  df_in <- cbind(
    "value" = sapply(
      models$unpleasant_hours, 
      FUN = kwb.smartwater:::defHourModel, 
      surface_reduction = sf
    ),
    siteInfo
  )
  kwb.smartwater:::plot_into_png(
    fn = file.path(path, "output", paste0(sf, "_unpleasant_hours.png")),
    width_factor = plotDim[1]/plotDim[2], 
    xpdDim = 6, 
    xlim = xlim, 
    ylim = ylim, 
    rivers_p2 = kwb.smartwater:::value_to_classes(
      river_list = kwb.smartwater:::prepare_rivers(
        rivers = rivers, 
        df_in = df_in,
        mappingTable = mappingTable
      ),
      classBreaks = c(0, 50, 100, 200, 300, 500, Inf),
      colorVector = kwb.smartwater:::MisaColor
    ), 
    LegendTitle = paste0(
      "Unterschreitunsdauer in Stunden (3 mg/L) bei ",
      sub(pattern = "\\.", replacement = ",", x = sf),
      "% Abkopplung"
    ), 
    districPolygons = districPolygons, 
    waterPolygons = waterPolygons
  )
}

# Kritische Events -------------------------------------------------------------
{
  df_in <- cbind(
    "value" = sapply(
      models$critical_events, 
      FUN = kwb.smartwater:::defEventModel, 
      surface_reduction = sf
    ),
    siteInfo
  )
  kwb.smartwater:::plot_into_png(
    fn = file.path(path, "output", paste0(sf, "_critical_events.png")),
    width_factor = plotDim[1]/plotDim[2], 
    xpdDim = 6, 
    xlim = xlim, 
    ylim = ylim, 
    rivers_p2 = kwb.smartwater:::value_to_classes(
      river_list = kwb.smartwater:::prepare_rivers(
        rivers = rivers, 
        df_in = df_in, 
        mappingTable = mappingTable
      ),
      classBreaks = c(-Inf, 0, 1, 3, 6, 10, Inf),
      colorVector = kwb.smartwater:::MisaColor
    ), 
    LegendTitle = paste0(
      "Kritische Sauerstoffereignisse bei ",
      sub(pattern = "\\.", replacement = ",", x = sf),
      "% Abkopplung"
    ), 
    districPolygons = districPolygons, 
    waterPolygons = waterPolygons
  )
}

# Negative Abweichung ----------------------------------------------------------
{
  df_in <- cbind(
    "value" = sapply(
      models$negative_deviation, 
      FUN = kwb.smartwater:::defHourModel, 
      surface_reduction = sf
    ) * 100,
    siteInfo
  )
  kwb.smartwater:::plot_into_png(
    fn = file.path(path, "output", paste0(sf, "_negative_deviation.png")),
    width_factor = plotDim[1]/plotDim[2], 
    xpdDim = 6, 
    xlim = xlim, 
    ylim = ylim, 
    rivers_p2 = kwb.smartwater:::value_to_classes(
      river_list = kwb.smartwater:::prepare_rivers(
        rivers = rivers, 
        df_in = df_in, 
        mappingTable = mappingTable
      ),
      classBreaks = c(0, 2, 5, 10, 20, 30, Inf),
      colorVector = kwb.smartwater:::MisaColor
    ), 
    LegendTitle = paste0(
      "Negative Abwichung vom Referenzustand (in %) bei ",
      sub(pattern = "\\.", replacement = ",", x = sf),
      "% Abkopplung"
    ), 
    districPolygons = districPolygons, 
    waterPolygons = waterPolygons
  )
}
