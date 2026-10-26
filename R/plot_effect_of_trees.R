#' Plot the Effect of Trees
#' 
#' @param block_code code of Berlin block. Default: "1100556171000000" 
#'   (Bayerischer Platz)
#' @param max_vegetation_class maximal vegetation class. Default: 80
#' @param n_trees vector of tree numbers to be simulated. Default: 0:100
#' @export
plot_effect_of_trees <- function(
    block_code = "1100556171000000",
    max_vegetation_class = 80,
    n_trees = 0:100
) {
  
  # unsealed area within the block
  unsealed <- function(block) {
    with(block, (1 - roof - pvd) * total_area)
  }
  
  tree_volumes <- stats::setNames(
    unlist(get_measure_info("trees", parameters_only = TRUE)),
    c("small", "medium", "large")
  )
  
  blocks <- kwb.rabimo::rabimo_inputs_2025$data
  block <- blocks[blocks$code == block_code, ]
  
  scenarios <- expand.grid(
    n_trees = n_trees,
    tree_size = names(tree_volumes),
    use_linear_relation = c(TRUE, FALSE)
  )
  
  data <- do.call(rbind, lapply(seq_len(nrow(scenarios)), function(i) {
    scenario <- scenarios[i, ]
    unsealed_area_m2 <- unsealed(block)
    tree_volume <- tree_volumes[[scenario$tree_size]]
    data.frame(
      tree_size = scenario$tree_size,
      n_trees = scenario$n_trees,
      use_linear_relation = scenario$use_linear_relation,
      veg_class = recalculate_vegetation_class(
        old_veg_class = block$veg_class, 
        tree_measure_volume = scenario$n_trees * tree_volume,
        unsealed_area_m2 = unsealed_area_m2, 
        max_veg_class = max_vegetation_class, 
        use_linear_relation = scenario$use_linear_relation
      )
    )
  }))
  
  x_var <- "n_trees"
  y_var <- "veg_class"
  col_var <- "tree_size"
  pch_var <- "use_linear_relation"
  
  ggplot2::ggplot(data, ggplot2::aes_string(
    x = !!x_var, 
    y = !!y_var, 
    col = !!col_var,
    pch = !!pch_var
  )) + 
    ggplot2::geom_point() +
    ggplot2::theme_minimal() +
    ggplot2::labs(
      title = "Vegetation Class Increase With Number of Trees",
      subtitle = paste("block code: ", block_code)
    )
}
