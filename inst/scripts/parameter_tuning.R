#
# What Bagrov values are appropriate for the different measures?
#
# We use the AMAREX tool by Uni Stuttgart (Eddy Tool): 
# https://amarex-projekt.de/media/pages/news/vergleichstool/5cd6c3098c-1752739226/amarex_massnahmenkatalogmodellierungen.xlsx
# and the analysis by Sieker
# https://www.sieker.de/fachinformationen/regenwasserbewirtschaftung/versickerung/article/versickerungsmulden-156.html


# GREEN ROOFS ------------------------------------------------------------------
# target evaporation
percentage_evaporation <- list(
  intensiv = 86,
  extensiv = 64
)

# Example area
mean_prec_yr <- mean(kwb.rabimo::rabimo_inputs_2025$data$prec_yr)
mean_epot_yr <- mean(kwb.rabimo::rabimo_inputs_2025$data$epot_yr)

block <- kwb.rabimo::generate_rabimo_area(
  "green_roof", 
  prec_yr = as.integer(mean_prec_yr), 
  epot_yr = as.integer(mean_epot_yr),
  roof = 1, 
  pvd = 0,
  green_roof = 1
)

config <- kwb.rabimo:::reconfigure(kwb.rabimo::rabimo_inputs_2025$config)

bagrov_values <- seq(0, 3.5, 0.05)
result <- do.call(rbind, lapply(bagrov_values, function(bagrov_value) {
  config$measures$green_roof[[1]]$bagrov_value <- bagrov_value
  kwb.rabimo::run_rabimo(block, config = config, silent = FALSE)
}))

result$bagrov_value <- bagrov_values
result$precipitation <- rowSums(result[, -(1:2)])
result$percentage_evaporation <- result$evapor/result$precipitation

plot(result$bagrov_value, result$percentage_evaporation)
abline(a = percentage_evaporation$intensiv/100, b = 0)
abline(a = percentage_evaporation$extensiv/100, b = 0)

bagrov_for_evaporation <- function(target) {
  bagrov_values[which.min(abs(result$percentage_evaporation*100 - target))] 
}

lapply(percentage_evaporation, bagrov_for_evaporation)

# INFILTRATION MEASURES --------------------------------------------------------
# from Sieker
# Swale: 14% evaporation, 0% runoff
# Surface Infiltration: 33% evaporation, 0% runoff
# Swale-trench: 7% evaporation, 0% runoff

# RETENTION --------------------------------------------------------------------
# from Eddy Tool
# ~60% overflow 
# in SMARTWATER the rest evaporates because the water is used for irrigation.

# OPTIMIERTER BAUMSTANDORT -----------------------------------------------------
# Assumption: ~twice as much evaporation as a swale and no runoff

# TREES ------------------------------------------------------------------------
# veg_class in the Berlin blocks has been determined proportionally to the
# green volume (Gruenvolumen) from the Umweltatlas.There, a "cylindrical" model
# has been used, is which each tree's volume is calculated as canopy area times
# the height. When a new new tree is set in a block as a measure, the 
# corresponding increment of the veg_class is calculated. Tree measures come
# in three sizes: small, medium, large.
#
# See also inst/scripts/vegetation_class_increment.R where we demonstrate the
# calculation from green volume to vegetation class and vice versa. We decided
# to keep the simple approach with a linear instead of an exponential relation.
#
# small tree: 150 m³
# medium tree: 700 m³
# large tree: 2300 m³

# Those numbers were derived from the tree cataster with the following script: 
### Determine green volume for small, medium, large trees in Berlin

# Download link to "Baumkataster-berlin"
url <- paste0(
  "https://hub.arcgis.com/api/download/v1/items/",
  "05c3f9d7dea6422b86e30967811bddd7",
  "/csv?layers=0"
)

# Download CSV file
trees <- data.table::fread(text = paste(readLines(url), collapse = "\n"))

# Calculate green volume
trees$volume_m3 <- (trees$`Krone Durchschnitt (m)` / 2)^2 * pi * trees$`Höhe (m)`

# Split trees into three groups by volume
groups <- split(trees, dplyr::ntile(trees$volume_m3, 3))
names(groups) <- c("small", "medium", "large")

summaries <- lapply(groups, function(group) {
  summary(group$volume_m3)
})

signif(sapply(summaries, `[[`, "Median"), digits = 2L)

