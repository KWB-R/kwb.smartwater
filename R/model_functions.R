get_models <- function() {
  model_files <- c(
    negative_deviation = "model_negative-deviation.csv",
    critical_events = "model_events-critical.csv",
    critical_hours = "model_hours-critical.csv",
    unpleasant_hours = "model_hours-unpleasant.csv"
  )
  model_dir <- system.file(
    "extdata", 
    "models", 
    package = "kwb.smartwater", 
    mustWork = TRUE
  )
  stats::setNames(
    lapply(file.path(model_dir, model_files), FUN = read_model),
    names(model_files)
  )
}

#' @importFrom utils read.csv
read_rivers <- function(file) {
  df <- read.csv(file)
  river_dfs <- lapply(split(df, df[[1L]]), function(x) {
    x <- x[-1L]
    rownames(x) <- NULL
    x
  })
  # Bring list elements in original order
  river_dfs[unique(df$river)]
}

#' @importFrom utils read.csv
read_site_info <- function(file) {
  df <- read.csv(file)
  rownames(df) <- df[[1L]]
  df[-1L]
}

#' @importFrom utils read.csv
read_model <- function(file) {
  df <- read.csv(file)
  model <- lapply(split(df, seq_len(nrow(df))), function(x) unlist(x[-1L]))
  stats::setNames(model, df[[1L]])
}

MisaColor <- stats::setNames(
  c("#17585E", "#99B579", "#F5A200", "#E98627", "#D95B3E", "#C71647"),
  c("perfect", "good", "acceptable", "bad", "critical", "very_serious") 
)

defHourModel <- function(reg_model, surface_reduction) {
  if(surface_reduction >= reg_model["A"] & !is.na(reg_model["A"])){
    0
  } else {
    reg_model["m"] * surface_reduction + reg_model["b"]
  }
}

defEventModel <- function(reg_model, surface_reduction){
  if(surface_reduction >= reg_model["A"] & !is.na(reg_model["A"])){
    0
  } else {
    round(reg_model["m"] * surface_reduction + reg_model["b"])
  }
}

# function based on qsimVis, changed for stand-alone use
prepare_rivers <- function(rivers = NULL, df_in = NULL, mappingTable = NULL) {
  check_arg_not_null <- get_arg_not_null_checker("prepare_rivers")
  check_arg_not_null(rivers)
  check_arg_not_null(df_in)
  check_arg_not_null(mappingTable)
  
  rivers_ext <- lapply(
    X = names(rivers), 
    FUN = extend_riverTable,
    rivers = rivers,
    aggregated_data = df_in,
    varName = "value",
    NA_processing = "interpolation"
  )
  names(rivers_ext) <- names(rivers)
  rivers_ext <- lapply(names(rivers), function(r){
    list(
      "data" = rivers_ext[[r]],
      "pp" = list( # list of plot properties
        "river_lwd" = mappingTable$size_type[mappingTable$qsimVis_ID == r]
      )
    )
  })
  names(rivers_ext) <- names(rivers)
  rivers_ext
}

extend_riverTable <- function(
    rivers, river_id, aggregated_data, varName,
    NA_processing = "interpolation"
){
  
  if(!(varName %in% colnames(aggregated_data))){
    stop(varName, " is no column in 'aggregated_data'")
  }
  river_table <- rivers[[river_id]]
  # river table needs to be ordered by river km
  river_table <- river_table[order(river_table$km),]
  
  
  
  river_table[["value"]] <- NA
  # filter results for river id
  data_table <- aggregated_data[aggregated_data$qsimVis_ID == river_id &
                                  !is.na(aggregated_data$qsimVis_ID),]
  
  if(nrow(data_table) > 0L & any(!is.na(data_table[[varName]]))){
    # apply results to closest verknet node, if not already defined
    km_verknet <- river_table$km
    for(i in seq_len(nrow(data_table))){
      km_result <- data_table$km[i]
      km_diff <- abs(km_result - km_verknet)
      node_match <- which(km_diff == min(km_diff))
      # select nodes only, that were not selected before
      single_node_match <- node_match[which(is.na(river_table$value[node_match]))]
      if(length(single_node_match) == 1L){
        river_table$value[single_node_match] <- data_table[[varName]][i]
      } else if(length(single_node_match) > 1L){ # if more than one points of equal distance, use first of them
        river_table$value[single_node_match[1]] <- data_table[[varName]][i]
      }
    }
    
    # if the last and the first verknet node are not defined, use the closest
    # data node, depending on distance
    tolerable_distance <- 0.5 # km
    if(is.na(river_table$value[1])){
      first_value <- which(!is.na(river_table$value))[1]
      if(river_table$km[first_value] - river_table$km[1] < tolerable_distance){
        river_table$value[1] <- river_table$value[first_value]
      }
    }
    
    l <- nrow(river_table)
    if(is.na(river_table$value[l])){
      last_value <- rev(which(!is.na(river_table$value)))[1]
      if(river_table$km[l] - river_table$km[last_value] < tolerable_distance){
        river_table$value[l] <- river_table$value[last_value]
      }
    }
    
    river_table$value <-
      if(NA_processing == "interpolation"){
        interpolate_multipleNA(
          data_vector = river_table$value,
          max_na = 1000,
          diff_x = river_table$distance_to_neighbour)[[1]]
      } else if(NA_processing == "steps"){
        insert_downstreamNA(data_vector = river_table$value)
      }
  }
  river_table
}


value_to_classes <- function(
    river_list, classBreaks, colorVector = NULL
){
  output_list <- lapply(names(river_list), function(N){
    r <- river_list[[N]]
    x <- r$data
    x$value_class <- if(any(!is.na(x$value))){
      cut(
        x = x$value,
        breaks = classBreaks,
        include.lowest = TRUE,
        ordered_result = TRUE
      )
    } else {
      NA
    }
    list("data" = x,
         "pp" = r$pp
    )
  })
  names(output_list) <- names(river_list)
  
  colorVector <- classes_to_color(
    class_levels = levels(output_list[[1]]$data$value_class),
    colorVector = colorVector
  )
  
  output_list2 <- lapply(names(output_list), function(N){
    r <- output_list[[N]]
    x <- r$data
    x$color <- colorVector[x$value_class]
    list("data" = x,
         "pp" = r$pp)
  })
  names(output_list2) <- names(river_list)
  
  output_list2
}

classes_to_color <- function(class_levels, colorVector = NULL){
  check_arg_not_null <- get_arg_not_null_checker("classes_to_color")
  check_arg_not_null(colorVector)
  
  nClasses <- length(class_levels)
  
  while(nClasses > length(colorVector)){
    ct <- grDevices::col2rgb(colorVector)
    ct2 <- matrix((ct[,-ncol(ct)] + ct[,-1]) / 2, ncol = ncol(ct) -1)
    ct <- cbind(ct[,1], matrix(sapply(2:ncol(ct), function(i){
      cbind(ct2[,i-1], ct[,i])
    }), nrow = 3))
    colorVector <- apply(ct, 2, function(x){
      grDevices::rgb(red = x[1], green = x[2], blue = x[3], maxColorValue = 255)
    })
  }
  
  colorVector <- colorVector[round(
    seq(
      from = 1,
      to = length(colorVector),
      length.out = nClasses
    )
  )]
  names(colorVector) <- class_levels
  factor(colorVector, levels = colorVector)
}


plot_empty_map <- function(){
  xlim <- c(13.18, 13.47)
  ylim <- c(52.45, 52.57)
  
  # plotDim <- getDimensions(xlim = xlim, ylim = ylim, width = 10)
  plotDim <- c(10, 6.789581)
  width_factor <- plotDim[1]/plotDim[2]
  xpdDim <- 6
  grDevices::dev.new(noRStudioGD = TRUE, height = 6, width = 6 * width_factor,
                     units = "in")
  graphics::par(mar = c(xpdDim / 2, 0.2, xpdDim / 2 , xpdDim * width_factor - 0.2))
  
  plot(x = 0, y = 0,
       xaxt = "n", yaxt = "n", type = "n",
       xaxs = "i", yaxs = "i",
       xlab = "", ylab = "",
       xlim = xlim, ylim = ylim)
}

get_arg_not_null_checker <- function(fun_name) {
  function(arg) {
    if (is.null(arg)) {
      stop(sprintf(
        "Please set argument '%s' in call to %s()",
        deparse(substitute(arg)), fun_name
      ))
    }
  }
}

Berlin_and_waterbodies <- function(
    water_color = "lightblue", 
    city_color = "gray60",
    districPolygons = NULL,
    waterPolygons = NULL
){
  check_arg_not_null <- get_arg_not_null_checker("Berlin_and_waterbodies")
  check_arg_not_null(districPolygons)
  check_arg_not_null(waterPolygons)
  
  for(i in seq_along(districPolygons[[1]])){
    graphics::polygon(
      x = districPolygons[[1]][[i]][,1],
      y = districPolygons[[1]][[i]][,2],
      col = city_color, border = "black")
  }
  polies <- unique(waterPolygons$gis_coordinates[,"L2"])
  for(poly in polies){
    poly_rows <-
      waterPolygons$gis_coordinates[waterPolygons$gis_coordinates[,"L2"] == poly,]
    graphics::polygon(
      x = poly_rows[,"X"],
      y = poly_rows[,"Y"],
      col = water_color, border = NA
    )
  }
}

add_coloredRivers <- function(
    ext_rivers
){
  for(j in seq_along(ext_rivers)){
    graphics::lines(x = ext_rivers[[j]]$data$x, y = ext_rivers[[j]]$data$y,
                    col = "steelblue", lwd = ext_rivers[[j]]$pp$river_lwd)
    for(i in seq_len(nrow(ext_rivers[[j]]$data) - 1)){
      graphics::lines(x = ext_rivers[[j]]$data$x[i:(i+1)],
                      y = ext_rivers[[j]]$data$y[i:(i+1)],
                      col = as.character(ext_rivers[[j]]$data$color[i+1]),
                      lwd = 4 / ext_rivers[[j]]$pp$river_lwd)
    }
  }
}

#' @importFrom graphics par
add_river_legend <- function(
    ext_rivers, LegendTitle = "", LegendLocation = "right", ...
){
  
  if (LegendLocation == "top"){
    lx <- mean(par("usr")[1:2])
    ly <- par("usr")[4]
    xadj <- 0.5
    hor <- TRUE
  } else if(LegendLocation == "right"){
    lx <- par("usr")[2]
    ly <- par("usr")[3]
    xadj <- 0
    hor <- FALSE
    LegendTitle <- gsub(
      pattern = '(.{1,20})(\\s|$)',
      replacement = '\\1\n',
      x = LegendTitle)
  }
  
  x <- ext_rivers[[1]]$data
  data_type <- if("value_class" %in% colnames(x)){
    "categorical"
  } else {
    "numerical"
  }
  
  if(data_type == "categorical"){
    cs <- levels(x$value_class)
    cc <- levels(x$color)
    
    nc <- length(cs)
    if(grepl(pattern = "^\\(", x = cs[1])){
      cs[1] <- paste0("> ", strsplit(x = cs[1], split = ",")[[1]][-1])
    }
    
    if(grepl(pattern = "\\)$", x = cs[nc])){
      cs[nc] <- paste0(strsplit(x = cs[nc], split = ",")[[1]][1])
    }
    cs <- gsub(pattern = " ", replacement = "", x = cs)
    cs <- gsub(pattern = "\\[", replacement = "", x = cs)
    cs <- gsub(pattern = "\\(", replacement = "> ", x = cs)
    cs <- gsub(pattern = "\\,", replacement = " - ", x = cs)
    cs <- gsub(pattern = "\\]", replacement = "", x = cs)
    cs <- gsub(pattern = "^-Inf - ", replacement = "<= ", x = cs)
    cs <- gsub(pattern = " - Inf$", replacement = "", x = cs)
    l_content <- cs
    graphics::legend(x = lx, y = ly, legend = cs, col = cc, lwd = 6,
                     bg= "white", bty = "n", title = LegendTitle,
                     xpd = T, xjust = xadj, yjust = 0, horiz = hor, ...)
    
  }
}

interpolate_multipleNA <- function(
    data_vector,
    max_na,
    diff_x = NULL
){
  # find NA data
  nas <- same_inarow(v = is.na(data_vector))
  
  nas <- nas[nas$Value, ]
  
  # if the first or last is NA, interpolation is not possible
  rfi <- nas[nas$repeats <= max_na &
               nas$starts_at != 1 &
               nas$ends_at != length(data_vector), ]
  
  if(nrow(rfi) > 0){
    for(i in 1:nrow(rfi)){
      beg_i <- rfi$starts_at[i] - 1
      end_i <- rfi$ends_at[i] + 1
      downstream <- data_vector[beg_i]
      upstream <- data_vector[end_i]
      
      new_values <-  if(is.null(diff_x)){
        # interpolated values
        seq(upstream, downstream, length.out = rfi$repeats[i] + 2)
        
      } else {
        x <- cumsum(diff_x[beg_i:end_i])
        new_values <- (x - min(x)) / diff(range(x)) * (upstream- downstream) + downstream
      }
      
      new_values <- new_values[-c(1, length(new_values))]
      
      
      # Replace NAs
      data_vector[rfi$starts_at[i]:rfi$ends_at[i]] <- new_values
    }
  }
  list(data_vector,
       c("NA's interpolated" = sum(rfi$repeats),
         "NA's in total" =  sum(nas$repeats)))
}

insert_downstreamNA <- function(data_vector) {
  nas <- is.na(data_vector)
  to_fill <- which(nas)
  by_using <- which(!nas)
  
  fill_values <- sapply(to_fill, function(x){
    rank_diff <- (x - by_using)
    upper_value <- by_using[which(rank_diff < 0)[1]]
    data_vector[upper_value]
  })
  data_vector[to_fill] <- fill_values
  data_vector
}

same_inarow <- function(v) {
  
  stopifnot(!anyNA(v))
  
  n <- length(v)
  if (n == 0L) {
    return(NULL)
  }
  changes_at <- which(v[-n] != v[-1L]) + 1L
  result <- data.frame(
    "starts_at" = c(1L, changes_at),
    "ends_at" = c(changes_at -  1L, n),
    "value" = v[c(1L, changes_at)],
    stringsAsFactors = FALSE
  )
  
  data.frame(
    Value = result$value,
    repeats = result$ends_at - result$starts_at + 1L,
    starts_at = result$starts_at,
    ends_at = result$ends_at
  )
}

plot_into_png <- function(
    fn, 
    width_factor = 10/6.789581, 
    xpdDim = 6, 
    xlim, 
    ylim, 
    rivers_p2, 
    LegendTitle, 
    districPolygons, 
    waterPolygons
) {
  plot_into_png_generic(
    filename = fn, 
    png_args = list(
      width = 6 * width_factor, 
      height = 6 , 
      units = "in", 
      res = 600
    ),
    plot_fun = plot_rivers,
    plot_fun_args = list(
      ext_rivers = rivers_p2, 
      LegendTitle = LegendTitle, 
      xlim = xlim,
      ylim = ylim,
      districPolygons = districPolygons, 
      waterPolygons = waterPolygons,
      xpdDim = xpdDim, 
      width_factor = width_factor
    )
  )
}

plot_into_png_generic <- function(
    filename, 
    png_args = list(), 
    plot_fun, 
    plot_fun_args = list()
)
{
  do.call(grDevices::png, c(list(filename = filename), png_args))
  on.exit(invisible(grDevices::dev.off()))
  do.call(plot_fun, plot_fun_args)
}

#' @importFrom graphics par
plot_rivers <- function(
    ext_rivers, 
    LegendTitle,
    xlim,
    ylim,
    districPolygons = read_district_polygons(), 
    waterPolygons = read_water_polygons(), 
    margins = c(1, 1, 3 , 1)
)
{
  old_par <- par(mar = margins)
  on.exit(par(old_par))
  
  plot(
    x = 0, 
    y = 0,
    xaxt = "n", 
    yaxt = "n", 
    type = "n",
    xaxs = "i", 
    yaxs = "i",
    xlab = "", 
    ylab = "",
    xlim = xlim, 
    ylim = ylim
  )
  
  Berlin_and_waterbodies(
    water_color = "lightblue", 
    city_color = "gray60", 
    districPolygons = districPolygons, 
    waterPolygons = waterPolygons
  )
  
  # Add colored Rivers
  add_coloredRivers(
    ext_rivers = ext_rivers
  )
  
  add_river_legend(
    ext_rivers = ext_rivers,
    LegendTitle = LegendTitle,
    LegendLocation = "top", 
    cex = 0.8
  )
}

package_file <- function(..., mustWork = TRUE) {
  system.file(..., package = "kwb.smartwater", mustWork = mustWork)
}

#' @importFrom kwb.utils textToObject
read_from_text <- function(text_file) {
  kwb.utils::textToObject(readLines(text_file))
}

read_district_polygons <- function() {
  read_from_text(package_file("extdata/data/districtPolygons.txt"))
}

read_water_polygons <- function() {
  read_from_text(package_file("extdata/data/waterPolygons.txt"))
}
