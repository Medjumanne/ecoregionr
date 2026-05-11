# =============================================================================
# 7. make_cluster_heatmap
# =============================================================================
#' Z-score heatmap of cluster mean variable values for one group.

#' @param data        Data frame with cluster assignments (output of
#'                    run_clustering()).
#' @param group_name  The specific group value to plot (e.g. "Kenya").
#' @param group_col   Name of the grouping column.
#' @param exclude_cols Columns to drop before computing means (IDs, PC scores,
#'                    metadata).  Defaults cover common cases.
#' @param cat_threshold Minimum mean value for a category to appear on heatmap.
#'
#' @return A ggplot object.
#' @export
make_cluster_heatmap <- function(data,
                                 group_name,
                                 group_col = "ADM0_NAME",
                                 cluster_col = "cluster",
                                 facet_by = NULL,           # ← NEW: Add this parameter
                                 exclude_cols = c("ID", "k_used", "Shape_Leng",
                                                  "Shape_Area", "FID_1"),
                                 cat_threshold = 0.01) {

  heatmap_data <- data |>
    filter(.data[[group_col]] == group_name) |>
    group_by(.data[[cluster_col]], .data[[facet_by]]) |>     # Support faceting
    summarise(across(where(is.numeric), \(x) mean(x, na.rm = TRUE)),
              .groups = "drop") |>
    select(-any_of(exclude_cols)) |>
    select(-starts_with("PC"))

  # Drop near-zero columns
  heatmap_data <- heatmap_data |>
    select(.data[[cluster_col]],
           any_of(facet_by),
           where(~ !is.numeric(.x) || max(abs(.x), na.rm = TRUE) > cat_threshold))

  heatmap_scaled <- heatmap_data |>
    mutate(across(-c(all_of(cluster_col), any_of(facet_by)),
                  \(x) as.vector(scale(x)))) |>
    pivot_longer(-c(all_of(cluster_col), any_of(facet_by)),
                 names_to = "Variable", values_to = "Z_score") |>
    filter(!is.na(Z_score))

  p <- ggplot(heatmap_scaled, aes(x = .data[[cluster_col]],
                                  y = Variable,
                                  fill = Z_score)) +
    geom_tile(color = "white", linewidth = 0.2) +
    scale_fill_gradientn(colors = c("blue", "white", "red"), name = "Z-score") +
    labs(title = paste("Cluster Characterisation:", group_name),
         subtitle = "Red = above-average; blue = below-average",
         x = "Bioregion",
         y = "Variable") +
    theme_test() +
    theme(axis.text.y = element_text(size = 8),
          legend.position = "right")

  # Add faceting if requested
  if (!is.null(facet_by)) {
    p <- p + facet_wrap(vars(.data[[facet_by]]), scales = "free_x", ncol = 1)
  }

  return(p)
}
