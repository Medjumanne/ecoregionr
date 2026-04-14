
# =============================================================================
# 6. run_clustering
# =============================================================================
#' Apply cluster_pca_kmeans() to each group in a data frame.

#' @param df        Wide covariate table (output of build_cluster_table /
#'                  impute_medians).
#' @param group_col Name of the grouping column (e.g. "ADM0_NAME").
#' @param ...       Additional arguments forwarded to cluster_pca_kmeans().
#'
#' @return A single tibble containing all rows with cluster assignments.
#'
#' @importFrom purrr map_dfr
#' @importFrom dplyr group_split
#' @export
run_clustering <- function(df, group_col, ...) {
  df |>
    dplyr::group_split(.data[[group_col]]) |>
    purrr::map_dfr(~ cluster_pca_kmeans(.x, group_col = group_col, ...))
}

