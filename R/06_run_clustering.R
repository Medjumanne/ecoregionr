# =============================================================================
# 6. run_clustering (with constraint support)
# =============================================================================
#' Apply cluster_pca_kmeans() to each group, with optional elevation band constraint.
#'
#' @param df Wide covariate table.
#' @param group_col Main grouping column (e.g. "ADM0_NAME").
#' @param constraint_var Variable to constrain by (e.g. "elev_band").
#' @param constraint_mode "none" (default) or "split".
#' @param ... Additional arguments passed to cluster_pca_kmeans().
#'
#' @return Tibble with cluster assignments.
#' @export
run_clustering <- function(df,
                           group_col,
                           constraint_var = NULL,
                           constraint_mode = "none",
                           ...) {

  if (constraint_mode == "split" && !is.null(constraint_var)) {

    if (!constraint_var %in% names(df)) {
      stop("constraint_var '", constraint_var, "' not found in data.")
    }

    message("🔀 Splitting clustering by ", constraint_var, " within each ", group_col)

    # Create combined grouping key
    df <- df %>%
      dplyr::mutate(.split_group = interaction(
        .data[[group_col]],
        .data[[constraint_var]],
        drop = TRUE,
        sep = " | "
      ))

    # Run clustering on each (country × elev_band) combination
    result <- df %>%
      dplyr::group_split(.split_group) %>%
      purrr::map_dfr(~ cluster_pca_kmeans(
        .x,
        group_col = ".split_group",   # for nice messages
        ...
      )) %>%
      dplyr::select(-.split_group)

    message("✅ Completed constrained clustering (", constraint_var, " split)")
    return(result)

  } else {
    # Original behavior - no constraint
    message("▶️ Running standard clustering by ", group_col)
    df %>%
      dplyr::group_split(.data[[group_col]]) %>%
      purrr::map_dfr(~ cluster_pca_kmeans(.x, group_col = group_col, ...))
  }
}
