# =============================================================================
# 10. summarize_clusters_compact
# =============================================================================
#' Build a wide cluster summary table with [min–max] ranges for numeric
#' variables and mean proportions for categorical variables.

#' @param cluster_df    Output of run_clustering().
#' @param polygons_sf   The original sf (for ADM2 names lookup).
#' @param numeric_vars  Character vector of numeric covariate column names.
#' @param group_col     Grouping column name.
#' @param id_col        Row ID column name.
#' @param name_col      Column in `polygons_sf` with polygon labels (e.g. district name).
#' @param cat_threshold Minimum mean proportion to include a category.
#'
#' @return A tibble with one row per (group × cluster) combination.
#' @export
summarize_clusters_compact <- function(cluster_df,
                                       polygons_sf,
                                       numeric_vars,
                                       group_col      = "ADM0_NAME",
                                       id_col         = "ID",
                                       name_col       = "ADM2_NAME",
                                       cat_threshold  = 0.01) {

  # Numeric ranges
  cluster_summary <- cluster_df |>
    group_by(Group = .data[[group_col]], cluster) |>
    summarise(
      across(
        all_of(numeric_vars),
        ~ paste0("[", round(min(.x, na.rm = TRUE), 3), "-",
                 round(max(.x, na.rm = TRUE), 3), "]"),
        .names = "{.col}_range"
      ),
      Unit_names = {
        ids_in_cluster <- .data[[id_col]]
        nm_vec <- polygons_sf[[name_col]][polygons_sf[[id_col]] %in% ids_in_cluster]
        paste(nm_vec, collapse = ", ")
      },
      .groups = "drop"
    )

  # Categorical means
  cat_vars <- setdiff(names(cluster_df),
                      c(id_col, group_col, "cluster", "k_used",
                        numeric_vars, grep("^PC", names(cluster_df), value = TRUE)))
  cat_vars_num <- cat_vars[sapply(cluster_df[cat_vars], is.numeric)]

  if (length(cat_vars_num) > 0) {
    cat_summary <- cluster_df |>
      group_by(Group = .data[[group_col]], cluster) |>
      summarise(
        across(all_of(cat_vars_num), ~ mean(.x, na.rm = TRUE), .names = "{.col}_mean"),
        k_used = first(k_used),
        .groups = "drop"
      )

    # col_nms must live in the enclosing environment so c_across(all_of())
    # can see it — defining it inside the {} block causes "object not found".
    col_nms <- paste0(cat_vars_num, "_mean")

    cat_compact <- cat_summary |>
      rowwise() |>
      mutate(
        Categories = {
          vals    <- c_across(all_of(col_nms))
          keep    <- !is.na(vals) & vals > cat_threshold
          nm_keep <- sub("_mean$", "", col_nms)[keep]
          v_keep  <- round(vals[keep], 3)
          paste(paste0(nm_keep, " (", v_keep, ")"), collapse = ", ")
        }
      ) |>
      ungroup() |>
      select(Group, cluster, Categories, k_used)

    cluster_summary <- cluster_summary |>
      left_join(cat_compact, by = c("Group", "cluster"))
  }

  return(cluster_summary)
}
