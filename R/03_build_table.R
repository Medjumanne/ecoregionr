
# =============================================================================
# 3. build_cluster_table
# =============================================================================
#' Join polygon attributes with categorical composition tables.
#' @param polygons    sf object already enriched with continuous stats
#'                    (output of extract_continuous_stats).
#' @param cat_tables  List of wide tibbles from extract_categorical_composition.
#' @param keep_cols   Character vector of columns from `polygons` to retain.
#'                    Geometry is always dropped.
#' @param id_col      Join key.
#'
#' @return A plain tibble ready for clustering.
#' @export
build_cluster_table <- function(polygons,
                                cat_tables = list(),
                                keep_cols,
                                id_col = "ID") {

  base_df <- polygons |>
    sf::st_drop_geometry() |>
    select(all_of(c(id_col, keep_cols)))

  for (tbl in cat_tables) {
    base_df <- base_df |> left_join(tbl, by = id_col)
  }

  return(base_df)
}

